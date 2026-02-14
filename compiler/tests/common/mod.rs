#![allow(dead_code)]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Debug)]
pub struct CliResult {
    pub status: i32,
    pub stdout: String,
    pub stderr: String,
}

pub fn fixture_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("programs")
        .join(name)
}

pub fn run_cli(args: &[String]) -> CliResult {
    let cache_dir = test_cache_dir();
    run_cli_with_cache(args, cache_dir)
}

pub fn run_cli_with_cache(args: &[String], cache_dir: &Path) -> CliResult {
    let output = Command::new(env!("CARGO_BIN_EXE_momiji"))
        .env("MOMIJI_CACHE_DIR", cache_dir)
        .args(args)
        .output()
        .expect("failed to execute momiji binary");

    CliResult {
        status: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    }
}

pub fn run_check_fixture(name: &str) -> CliResult {
    let file = fixture_path(name);
    run_cli(&["check".to_string(), file.display().to_string()])
}

pub fn run_check_fixture_with_timings(name: &str) -> CliResult {
    let file = fixture_path(name);
    run_cli(&[
        "check".to_string(),
        file.display().to_string(),
        "--timings".to_string(),
    ])
}

pub fn run_check_fixture_changed(name: &str) -> CliResult {
    let file = fixture_path(name);
    run_cli(&[
        "check".to_string(),
        file.display().to_string(),
        "--changed".to_string(),
    ])
}

pub fn run_check_path(path: &Path, changed: bool, cache_dir: &Path) -> CliResult {
    let mut args = vec!["check".to_string(), path.display().to_string()];
    if changed {
        args.push("--changed".to_string());
    }
    run_cli_with_cache(&args, cache_dir)
}

pub fn run_interp_fixture(name: &str) -> CliResult {
    let file = fixture_path(name);
    run_cli(&[
        "run".to_string(),
        file.display().to_string(),
        "--backend".to_string(),
        "interp".to_string(),
    ])
}

pub fn run_cranelift_fixture(name: &str) -> CliResult {
    let file = fixture_path(name);
    run_cli(&[
        "run".to_string(),
        file.display().to_string(),
        "--backend".to_string(),
        "cranelift".to_string(),
    ])
}

pub fn build_fixture(name: &str, output_path: &Path) -> CliResult {
    let file = fixture_path(name);
    run_cli(&[
        "build".to_string(),
        file.display().to_string(),
        "--output".to_string(),
        output_path.display().to_string(),
    ])
}

pub fn unique_temp_path(prefix: &str) -> PathBuf {
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("time drift")
        .as_nanos();
    std::env::temp_dir().join(format!("{}_{}_{}", prefix, std::process::id(), nanos))
}

pub fn run_executable(path: &Path) -> CliResult {
    let output = Command::new(path)
        .output()
        .expect("failed to run built executable");

    CliResult {
        status: output.status.code().unwrap_or(-1),
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
    }
}

pub fn cleanup_file(path: &Path) {
    let _ = fs::remove_file(path);
}

pub fn write_text_file(path: &Path, contents: &str) {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("failed to create parent directory");
    }
    fs::write(path, contents).expect("failed to write file");
}

pub fn normalize_text(text: &str) -> String {
    let mut normalized = text.replace("\r\n", "\n");

    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    normalized = normalized.replace(manifest_dir, "<MANIFEST_DIR>");

    let tmp_dir = std::env::temp_dir().display().to_string();
    normalized = normalized.replace(&tmp_dir, "<TMP_DIR>");
    normalized = normalized.replace("/private/tmp", "<TMP_DIR>");
    normalized = normalized.replace("/tmp", "<TMP_DIR>");
    normalized = normalized.replace("/var/folders", "<TMP_DIR>");

    let mut lines = Vec::new();
    for line in normalized.lines() {
        if line.starts_with("ld: warning: no platform load command found") {
            continue;
        }

        if line.starts_with("Compiled to ") {
            lines.push("Compiled to <TEMP_EXE>".to_string());
            continue;
        }

        lines.push(line.to_string());
    }

    let mut out = lines.join("\n");
    if !out.is_empty() {
        out.push('\n');
    }
    out
}

fn test_cache_dir() -> &'static PathBuf {
    static CACHE_DIR: OnceLock<PathBuf> = OnceLock::new();
    CACHE_DIR.get_or_init(|| unique_temp_path("momiji_test_cache"))
}

pub fn snapshot_view(result: &CliResult) -> String {
    format!(
        "status: {}\n--- stdout ---\n{}--- stderr ---\n{}",
        result.status,
        normalize_text(&result.stdout),
        normalize_text(&result.stderr)
    )
}

pub fn normalized_program_output(result: &CliResult, backend: &str) -> String {
    let mut lines = Vec::new();
    for line in normalize_text(&result.stdout).lines() {
        if backend == "cranelift" && line.starts_with("Compiled to ") {
            continue;
        }
        if line.trim().is_empty() {
            continue;
        }
        lines.push(line.to_string());
    }

    let mut out = lines.join("\n");
    if !out.is_empty() {
        out.push('\n');
    }
    out
}
