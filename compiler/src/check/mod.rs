pub mod cache;
pub(crate) mod deps;

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};

use miette::{IntoDiagnostic, WrapErr};
use rayon::prelude::*;

use cache::{
    cache_key, check_cache_path, compute_file_metadata, file_fingerprint, load_check_cache,
    save_check_cache, CacheEntry, FileMetadata,
};
use deps::{compute_invalidation_set, DepGraph};
use crate::{frontend, FrontendArtifacts, FrontendTiming};

pub fn check(path: &Path, changed: bool, timings: bool) -> miette::Result<()> {
    let command_start = Instant::now();
    let targets = collect_check_targets(path)?;
    let is_single_target = targets.len() == 1;

    let cache_path = check_cache_path();
    let mut cache = if changed {
        let cache_result = load_check_cache(&cache_path)?;
        if cache_result.version_invalidated {
            eprintln!("Cache invalidated: compiler version changed. Starting fresh.");
        }
        cache_result.cache
    } else {
        HashMap::new()
    };

    let mut checked = 0usize;
    let mut frontend_total = Duration::ZERO;
    let mut single_frontend_timing: Option<FrontendTiming> = None;

    if changed {
        let mut target_keys = Vec::with_capacity(targets.len());
        let mut target_fingerprints = Vec::with_capacity(targets.len());
        let mut current_metadata: Vec<Option<FileMetadata>> = Vec::with_capacity(targets.len());

        // Track which target indices have changed fingerprints.
        let mut fingerprint_changed_indices: Vec<usize> = Vec::new();

        for (i, target) in targets.iter().enumerate() {
            let key = cache_key(target);
            let fingerprint = file_fingerprint(target)?;

            let unchanged = cache
                .get(&key)
                .map(|entry| entry.fingerprint == fingerprint)
                .unwrap_or(false);
            if !unchanged {
                fingerprint_changed_indices.push(i);
            }

            target_keys.push(key);
            target_fingerprints.push(fingerprint);
            current_metadata.push(None);
        }

        if fingerprint_changed_indices.is_empty() {
            if is_single_target {
                println!(
                    "No changes detected in {}; skipped (--changed).",
                    targets[0].display()
                );
            } else {
                println!(
                    "No changes detected in {} file(s) under {}; skipped (--changed).",
                    targets.len(),
                    path.display()
                );
            }
            if timings {
                eprintln!(
                    "timings check: frontend=0.00ms, total={}",
                    fmt_duration(command_start.elapsed())
                );
            }
            return Ok(());
        }

        // Compute metadata for changed files and identify API changes.
        let mut fingerprint_changed_keys: HashSet<String> = HashSet::new();
        let mut api_changed_keys: HashSet<String> = HashSet::new();

        let meta_results: Vec<(usize, FileMetadata)> = fingerprint_changed_indices
            .par_iter()
            .map(|&idx| compute_file_metadata(&targets[idx]).map(|m| (idx, m)))
            .collect::<miette::Result<Vec<_>>>()?;

        for (idx, meta) in meta_results {
            let cached_api = cache.get(&target_keys[idx]).map(|entry| entry.api_hash);
            if cached_api != Some(meta.api_hash) {
                api_changed_keys.insert(target_keys[idx].clone());
            }
            fingerprint_changed_keys.insert(target_keys[idx].clone());
            current_metadata[idx] = Some(meta);
        }

        // Detect deleted files: cache keys not in current targets.
        let current_target_keys: HashSet<&String> = target_keys.iter().collect();
        let mut deleted_exports: HashSet<String> = HashSet::new();
        let mut deleted_keys: Vec<String> = Vec::new();
        for (cached_key, cached_entry) in &cache {
            if !current_target_keys.contains(cached_key) {
                for export in &cached_entry.exports {
                    deleted_exports.insert(export.clone());
                }
                deleted_keys.push(cached_key.clone());
            }
        }

        // Build a merged view of all metadata for the dep graph:
        // fresh metadata for changed files, cached entries for unchanged files.
        let mut all_entries: HashMap<String, CacheEntry> = HashMap::new();
        for (i, key) in target_keys.iter().enumerate() {
            if let Some(meta) = &current_metadata[i] {
                all_entries.insert(
                    key.clone(),
                    CacheEntry {
                        fingerprint: target_fingerprints[i].clone(),
                        api_hash: meta.api_hash,
                        exports: meta.exports.clone(),
                        external_deps: meta.external_deps.clone(),
                    },
                );
            } else if let Some(cached) = cache.get(key) {
                all_entries.insert(key.clone(), cached.clone());
            }
            // New files without cache entries and without fingerprint changes
            // won't appear here — but they'd be in fingerprint_changed already.
        }

        let graph = DepGraph::build(&all_entries);

        let recheck_keys = compute_invalidation_set(
            &graph,
            &fingerprint_changed_keys,
            &api_changed_keys,
            &deleted_exports,
        );

        // Map recheck keys back to target indices.
        let key_to_idx: HashMap<&String, usize> = target_keys
            .iter()
            .enumerate()
            .map(|(i, k)| (k, i))
            .collect();

        let mut targets_to_check: Vec<usize> = recheck_keys
            .iter()
            .filter_map(|k| key_to_idx.get(k).copied())
            .collect();
        targets_to_check.sort();

        let frontend_results: Vec<(usize, miette::Result<FrontendArtifacts>)> = targets_to_check
            .par_iter()
            .map(|&idx| (idx, frontend(&targets[idx])))
            .collect();

        let mut error_count = 0usize;
        let mut first_error: Option<miette::Report> = None;
        let mut succeeded_indices: Vec<usize> = Vec::new();
        for (idx, result) in frontend_results {
            match result {
                Ok(artifacts) => {
                    checked += 1;
                    frontend_total += artifacts.timing.total();
                    if is_single_target {
                        single_frontend_timing = Some(artifacts.timing);
                    }
                    succeeded_indices.push(idx);

                    // Compute metadata for files we check but hadn't computed yet.
                    if current_metadata[idx].is_none() {
                        if let Ok(meta) = compute_file_metadata(&targets[idx]) {
                            current_metadata[idx] = Some(meta);
                        }
                    }
                }
                Err(report) => {
                    error_count += 1;
                    if is_single_target {
                        first_error = Some(report);
                    } else {
                        eprintln!("{:?}", report);
                    }
                }
            }
        }

        if error_count > 0 {
            // Still update cache for successful files before returning error.
            for &idx in &succeeded_indices {
                let meta = current_metadata[idx].as_ref();
                let (api_hash, exports, external_deps) = match meta {
                    Some(m) => (m.api_hash, m.exports.clone(), m.external_deps.clone()),
                    None => (0, Vec::new(), Vec::new()),
                };
                cache.insert(
                    target_keys[idx].clone(),
                    CacheEntry {
                        fingerprint: target_fingerprints[idx].clone(),
                        api_hash,
                        exports,
                        external_deps,
                    },
                );
            }
            for key in &deleted_keys {
                cache.remove(key);
            }
            let _ = save_check_cache(&cache_path, &cache);

            if is_single_target {
                return Err(first_error.unwrap());
            } else {
                return Err(miette::miette!(
                    "Found errors in {} of {} file(s) under {}.",
                    error_count,
                    targets.len(),
                    path.display()
                ));
            }
        }

        let skipped = targets.len() - checked;
        let api_change_count = api_changed_keys.len();
        if is_single_target {
            println!("No errors found in {}", targets[0].display());
        } else if api_change_count > 0 || !deleted_exports.is_empty() {
            println!(
                "No errors found in {} checked file(s) ({} skipped) under {}; {} API change(s) triggered downstream rechecks.",
                checked,
                skipped,
                path.display(),
                api_change_count
            );
        } else {
            println!(
                "No errors found in {} checked file(s) ({} skipped) under {}.",
                checked,
                skipped,
                path.display()
            );
        }

        // Update cache for all checked files.
        for &idx in &targets_to_check {
            let meta = current_metadata[idx].as_ref();
            let (api_hash, exports, external_deps) = match meta {
                Some(m) => (m.api_hash, m.exports.clone(), m.external_deps.clone()),
                None => (0, Vec::new(), Vec::new()),
            };
            cache.insert(
                target_keys[idx].clone(),
                CacheEntry {
                    fingerprint: target_fingerprints[idx].clone(),
                    api_hash,
                    exports,
                    external_deps,
                },
            );
        }

        // Remove deleted entries from cache.
        for key in &deleted_keys {
            cache.remove(key);
        }

        save_check_cache(&cache_path, &cache)?;
    } else {
        let results: Vec<miette::Result<FrontendArtifacts>> = targets
            .par_iter()
            .map(|target| frontend(target))
            .collect();

        let mut error_count = 0usize;
        let mut first_error: Option<miette::Report> = None;
        for result in results {
            match result {
                Ok(artifacts) => {
                    checked += 1;
                    frontend_total += artifacts.timing.total();
                    if is_single_target {
                        single_frontend_timing = Some(artifacts.timing);
                    }
                }
                Err(report) => {
                    error_count += 1;
                    if is_single_target {
                        first_error = Some(report);
                    } else {
                        eprintln!("{:?}", report);
                    }
                }
            }
        }

        if error_count > 0 {
            if is_single_target {
                return Err(first_error.unwrap());
            } else {
                return Err(miette::miette!(
                    "Found errors in {} of {} file(s) under {}.",
                    error_count,
                    targets.len(),
                    path.display()
                ));
            }
        }

        if is_single_target {
            println!("No errors found in {}", targets[0].display());
        } else {
            println!(
                "No errors found in {} file(s) under {}.",
                checked,
                path.display()
            );
        }
    }

    if timings {
        if is_single_target {
            if let Some(timing) = single_frontend_timing {
                print_frontend_timings("check", timing);
            }
            eprintln!(
                "timings check: frontend={}, total={}",
                fmt_duration(frontend_total),
                fmt_duration(command_start.elapsed())
            );
        } else {
            let skipped = targets.len().saturating_sub(checked);
            eprintln!(
                "timings check: files={}, checked={}, skipped={}, frontend={}, total={}",
                targets.len(),
                checked,
                skipped,
                fmt_duration(frontend_total),
                fmt_duration(command_start.elapsed())
            );
        }
    }

    Ok(())
}

fn collect_check_targets(path: &Path) -> miette::Result<Vec<PathBuf>> {
    if path.is_file() {
        return Ok(vec![path.to_path_buf()]);
    }

    if !path.is_dir() {
        return Err(miette::miette!(
            "check target must be a file or directory: {}",
            path.display()
        ));
    }

    let mut files = Vec::new();
    collect_mj_files_recursively(path, &mut files)?;
    files.sort();

    if files.is_empty() {
        return Err(miette::miette!(
            "No .mj files found under {}",
            path.display()
        ));
    }

    Ok(files)
}

fn collect_mj_files_recursively(dir: &Path, out: &mut Vec<PathBuf>) -> miette::Result<()> {
    let entries = fs::read_dir(dir)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read directory {}", dir.display()))?;

    for entry in entries {
        let entry = entry.into_diagnostic()?;
        let path = entry.path();
        if path.is_dir() {
            collect_mj_files_recursively(&path, out)?;
        } else if path.is_file() && path.extension().is_some_and(|ext| ext == "mj") {
            out.push(path);
        }
    }

    Ok(())
}

fn fmt_duration(duration: Duration) -> String {
    format!("{:.2}ms", duration.as_secs_f64() * 1000.0)
}

fn print_frontend_timings(command: &str, timing: FrontendTiming) {
    eprintln!(
        "timings {} frontend: read={}, lex={}, parse={}, sem={}",
        command,
        fmt_duration(timing.read),
        fmt_duration(timing.lex),
        fmt_duration(timing.parse),
        fmt_duration(timing.sem)
    );
}
