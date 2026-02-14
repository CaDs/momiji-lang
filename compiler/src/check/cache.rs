use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::UNIX_EPOCH;

use miette::{IntoDiagnostic, WrapErr};

use crate::parser::ast::TypeKind;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FileFingerprint {
    pub mtime_nanos: u128,
    pub size: u64,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CacheEntry {
    pub fingerprint: FileFingerprint,
    pub api_hash: u64,
    pub exports: Vec<String>,
    pub external_deps: Vec<String>,
}

/// Result of parsing a file once and extracting both API hash and dependency info.
pub struct FileMetadata {
    pub api_hash: u64,
    pub exports: Vec<String>,
    pub external_deps: Vec<String>,
}

pub fn check_cache_path() -> PathBuf {
    if let Ok(dir) = std::env::var("MOMIJI_CACHE_DIR") {
        return PathBuf::from(dir).join("check-cache.tsv");
    }

    std::env::current_dir()
        .unwrap_or_else(|_| PathBuf::from("."))
        .join(".momiji")
        .join("check-cache.tsv")
}

pub fn cache_key(file: &Path) -> String {
    let abs = if file.is_absolute() {
        file.to_path_buf()
    } else if let Ok(cwd) = std::env::current_dir() {
        cwd.join(file)
    } else {
        file.to_path_buf()
    };

    abs.to_string_lossy().to_string()
}

pub fn file_fingerprint(file: &Path) -> miette::Result<FileFingerprint> {
    let metadata = fs::metadata(file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to stat source file: {}", file.display()))?;

    let modified = metadata.modified().into_diagnostic().wrap_err_with(|| {
        format!(
            "Failed to read modification timestamp for source file: {}",
            file.display()
        )
    })?;
    let modified = modified
        .duration_since(UNIX_EPOCH)
        .into_diagnostic()
        .wrap_err_with(|| {
            format!(
                "Source file has an invalid modification timestamp: {}",
                file.display()
            )
        })?;

    Ok(FileFingerprint {
        mtime_nanos: modified.as_nanos(),
        size: metadata.len(),
    })
}

/// Parse a file once and extract both its API hash and dependency info.
pub fn compute_file_metadata(file: &Path) -> miette::Result<FileMetadata> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use miette::{NamedSource, Report};

    use crate::check::deps::{extract_file_info, external_deps};
    use crate::lexer::Scanner;
    use crate::parser::ast::Item;
    use crate::parser::Parser as MojiParser;

    let source = fs::read_to_string(file)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read source file: {}", file.display()))?;

    let file_name = file
        .file_name()
        .map(|f| f.to_string_lossy().to_string())
        .unwrap_or_else(|| file.display().to_string());

    let tokens = Scanner::new(&source).scan_all().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;
    let ast = MojiParser::new(tokens).parse().map_err(|e| {
        Report::new(e).with_source_code(NamedSource::new(file_name.clone(), source.clone()))
    })?;

    // API hash from function signatures
    let mut hasher = DefaultHasher::new();
    for item in &ast.items {
        let Item::Function(function) = item;
        function.name.hash(&mut hasher);
        function.params.len().hash(&mut hasher);
        for param in &function.params {
            param.name.hash(&mut hasher);
            type_kind_fingerprint(&param.ty.kind).hash(&mut hasher);
        }
        match &function.return_type {
            Some(annotation) => type_kind_fingerprint(&annotation.kind).hash(&mut hasher),
            None => "Unit".hash(&mut hasher),
        }
    }
    let api_hash = hasher.finish();

    // Dependency info
    let info = extract_file_info(&ast);
    let mut exports: Vec<String> = info.exports.iter().cloned().collect();
    exports.sort();
    let ext_deps = external_deps(&info);

    Ok(FileMetadata {
        api_hash,
        exports,
        external_deps: ext_deps,
    })
}

pub fn type_kind_fingerprint(kind: &TypeKind) -> String {
    match kind {
        TypeKind::Named(name) => format!("N:{name}"),
        TypeKind::Nullable(inner) => format!("Q:{}", type_kind_fingerprint(inner)),
        TypeKind::Array(inner) => format!("A:{}", type_kind_fingerprint(inner)),
    }
}

pub fn load_check_cache(path: &Path) -> miette::Result<HashMap<String, CacheEntry>> {
    let mut cache = HashMap::new();
    if !path.exists() {
        return Ok(cache);
    }

    let contents = fs::read_to_string(path)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to read check cache at {}", path.display()))?;

    for line in contents.lines() {
        let columns: Vec<&str> = line.split('\t').collect();
        if columns.len() < 3 {
            continue;
        }
        let key = columns[0];
        let Ok(mtime_nanos) = columns[1].parse::<u128>() else {
            continue;
        };
        let Ok(size) = columns[2].parse::<u64>() else {
            continue;
        };
        let api_hash = columns
            .get(3)
            .and_then(|raw| raw.parse::<u64>().ok())
            .unwrap_or(0);

        // Columns 4 and 5 are exports_csv and deps_csv (added in Phase 2).
        // Old 4-column caches will have empty lists, triggering a cold-start recheck.
        let exports = columns
            .get(4)
            .filter(|s| !s.is_empty())
            .map(|s| s.split(',').map(String::from).collect())
            .unwrap_or_default();
        let external_deps = columns
            .get(5)
            .filter(|s| !s.is_empty())
            .map(|s| s.split(',').map(String::from).collect())
            .unwrap_or_default();

        cache.insert(
            key.to_string(),
            CacheEntry {
                fingerprint: FileFingerprint { mtime_nanos, size },
                api_hash,
                exports,
                external_deps,
            },
        );
    }

    Ok(cache)
}

pub fn save_check_cache(path: &Path, cache: &HashMap<String, CacheEntry>) -> miette::Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .into_diagnostic()
            .wrap_err_with(|| {
                format!(
                    "Failed to create check cache directory {}",
                    parent.display()
                )
            })?;
    }

    let mut entries: Vec<_> = cache.iter().collect();
    entries.sort_by(|a, b| a.0.cmp(b.0));

    let mut out = String::new();
    for (key, value) in entries {
        out.push_str(key);
        out.push('\t');
        out.push_str(&value.fingerprint.mtime_nanos.to_string());
        out.push('\t');
        out.push_str(&value.fingerprint.size.to_string());
        out.push('\t');
        out.push_str(&value.api_hash.to_string());
        out.push('\t');
        out.push_str(&value.exports.join(","));
        out.push('\t');
        out.push_str(&value.external_deps.join(","));
        out.push('\n');
    }

    fs::write(path, out)
        .into_diagnostic()
        .wrap_err_with(|| format!("Failed to write check cache at {}", path.display()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    fn temp_cache_file(contents: &str) -> NamedTempFile {
        let mut f = NamedTempFile::new().unwrap();
        f.write_all(contents.as_bytes()).unwrap();
        f
    }

    #[test]
    fn load_old_4_column_format() {
        let f = temp_cache_file("/path/a.mj\t1000\t200\t999\n");
        let cache = load_check_cache(f.path()).unwrap();
        let entry = cache.get("/path/a.mj").unwrap();
        assert_eq!(entry.fingerprint.mtime_nanos, 1000);
        assert_eq!(entry.fingerprint.size, 200);
        assert_eq!(entry.api_hash, 999);
        assert!(entry.exports.is_empty());
        assert!(entry.external_deps.is_empty());
    }

    #[test]
    fn round_trip_with_exports_and_deps() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("cache.tsv");

        let mut cache = HashMap::new();
        cache.insert(
            "/path/a.mj".to_string(),
            CacheEntry {
                fingerprint: FileFingerprint {
                    mtime_nanos: 1000,
                    size: 200,
                },
                api_hash: 42,
                exports: vec!["add".to_string(), "sub".to_string()],
                external_deps: vec!["mul".to_string()],
            },
        );

        save_check_cache(&path, &cache).unwrap();
        let loaded = load_check_cache(&path).unwrap();
        assert_eq!(cache, loaded);
    }
}
