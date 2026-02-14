mod common;

use common::{
    run_check_fixture, run_check_fixture_changed, run_check_fixture_with_timings, run_check_path,
    snapshot_view, unique_temp_path, write_text_file,
};
use insta::assert_snapshot;
use std::fs;

#[test]
fn check_valid_hello() {
    let result = run_check_fixture("hello.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("check_valid_hello", snapshot_view(&result));
}

#[test]
fn check_valid_arrays() {
    let result = run_check_fixture("arrays_index.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("check_valid_arrays", snapshot_view(&result));
}

#[test]
fn check_valid_function_calls() {
    let result = run_check_fixture("function_calls.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("check_valid_function_calls", snapshot_view(&result));
}

#[test]
fn check_valid_branch_returns() {
    let result = run_check_fixture("branch_returns_ok.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("check_valid_branch_returns", snapshot_view(&result));
}

#[test]
fn check_missing_return_diagnostic() {
    let result = run_check_fixture("missing_return.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_missing_return", snapshot_view(&result));
}

#[test]
fn check_missing_return_branch_diagnostic() {
    let result = run_check_fixture("missing_return_branch.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_missing_return_branch", snapshot_view(&result));
}

#[test]
fn check_unreachable_diagnostic() {
    let result = run_check_fixture("unreachable.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_unreachable", snapshot_view(&result));
}

#[test]
fn check_unreachable_after_if_returns_diagnostic() {
    let result = run_check_fixture("unreachable_after_if_returns.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_unreachable_after_if_returns", snapshot_view(&result));
}

#[test]
fn check_type_mismatch_diagnostic() {
    let result = run_check_fixture("type_mismatch.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_type_mismatch", snapshot_view(&result));
}

#[test]
fn check_wrong_arity_diagnostic() {
    let result = run_check_fixture("wrong_arity.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_wrong_arity", snapshot_view(&result));
}

#[test]
fn check_reassign_is_valid() {
    let result = run_check_fixture("reassign_ok.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("check_reassign_is_valid", snapshot_view(&result));
}

#[test]
fn check_undefined_variable_diagnostic() {
    let result = run_check_fixture("undefined_var.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_undefined_var", snapshot_view(&result));
}

#[test]
fn check_undefined_function_diagnostic() {
    let result = run_check_fixture("undefined_function.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("check_undefined_function", snapshot_view(&result));
}

#[test]
fn check_timings_output() {
    let result = run_check_fixture_with_timings("hello.mj");
    assert_eq!(result.status, 0);
    assert!(result.stderr.contains("timings check frontend:"));
    assert!(result.stderr.contains("timings check: frontend="));
}

#[test]
fn check_changed_skips_unchanged_file() {
    let first = run_check_fixture_changed("hello.mj");
    assert_eq!(first.status, 0);
    assert!(first.stdout.contains("No errors found in"));

    let second = run_check_fixture_changed("hello.mj");
    assert_eq!(second.status, 0);
    assert!(second.stdout.contains("No changes detected in"));
    assert!(second.stdout.contains("skipped (--changed)"));
}

#[test]
fn check_directory_reports_summary() {
    let workspace = unique_temp_path("momiji_check_dir");
    let cache_dir = unique_temp_path("momiji_check_dir_cache");
    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n",
    );
    write_text_file(
        &workspace.join("sub").join("b.mj"),
        "def mul(a: Int, b: Int) -> Int\n  return a * b\nend\n",
    );

    let result = run_check_path(&workspace, false, &cache_dir);
    assert_eq!(result.status, 0);
    assert!(result.stdout.contains("No errors found in 2 file(s) under"));

    let _ = fs::remove_dir_all(&workspace);
    let _ = fs::remove_dir_all(&cache_dir);
}

#[test]
fn check_changed_rechecks_downstream_on_api_change() {
    // Two independent files (no cross-file calls).
    // With precise invalidation, API change in one file should NOT recheck the other.
    let workspace = unique_temp_path("momiji_check_api");
    let cache_dir = unique_temp_path("momiji_check_api_cache");
    let a_path = workspace.join("a.mj");
    let b_path = workspace.join("b.mj");

    write_text_file(
        &a_path,
        "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n",
    );
    write_text_file(
        &b_path,
        "def mul(a: Int, b: Int) -> Int\n  return a * b\nend\n",
    );

    // First run: cold start, both files are new = API changed.
    let first = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(first.status, 0);
    assert!(first.stdout.contains("2 checked file(s)"));
    assert!(first.stdout.contains("API change(s)"));

    // Second run: nothing changed.
    let second = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(second.status, 0);
    assert!(second.stdout.contains("No changes detected in 2 file(s)"));

    // Body-only change in a.mj: only a.mj rechecked, b.mj skipped.
    write_text_file(
        &a_path,
        "def add(a: Int, b: Int) -> Int\n  temp = a + b\n  return temp\nend\n",
    );
    let third = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(third.status, 0);
    assert!(third.stdout.contains("1 checked file(s) (1 skipped)"));
    assert!(!third.stdout.contains("API change(s)"));

    // Signature change in a.mj: with precise invalidation, b.mj is independent,
    // so only a.mj should be rechecked.
    write_text_file(
        &a_path,
        "def add(a: Int, b: Int, c: Int) -> Int\n  return a + b + c\nend\n",
    );
    let fourth = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(fourth.status, 0);
    assert!(fourth.stdout.contains("1 checked file(s) (1 skipped)"));
    assert!(fourth.stdout.contains("API change(s)"));

    let _ = fs::remove_dir_all(&workspace);
    let _ = fs::remove_dir_all(&cache_dir);
}

#[test]
fn check_changed_precise_independent_files() {
    // 3 independent files. Change A's API → only A rechecked, B and C skipped.
    let workspace = unique_temp_path("momiji_check_precise");
    let cache_dir = unique_temp_path("momiji_check_precise_cache");

    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n",
    );
    write_text_file(
        &workspace.join("b.mj"),
        "def sub(a: Int, b: Int) -> Int\n  return a - b\nend\n",
    );
    write_text_file(
        &workspace.join("c.mj"),
        "def mul(a: Int, b: Int) -> Int\n  return a * b\nend\n",
    );

    // Cold start: all 3 checked.
    let first = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(first.status, 0);
    assert!(first.stdout.contains("3 checked file(s)"));

    // No changes.
    let second = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(second.status, 0);
    assert!(second.stdout.contains("No changes detected in 3 file(s)"));

    // Change A's API: only A rechecked since B and C are independent.
    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int, c: Int) -> Int\n  return a + b + c\nend\n",
    );
    let third = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(third.status, 0);
    assert!(third.stdout.contains("1 checked file(s) (2 skipped)"));
    assert!(third.stdout.contains("API change(s)"));

    let _ = fs::remove_dir_all(&workspace);
    let _ = fs::remove_dir_all(&cache_dir);
}

#[test]
fn check_changed_body_only_no_downstream() {
    // Two independent files. Body-only change in A → only A rechecked.
    let workspace = unique_temp_path("momiji_check_body");
    let cache_dir = unique_temp_path("momiji_check_body_cache");

    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n",
    );
    write_text_file(
        &workspace.join("b.mj"),
        "def mul(a: Int, b: Int) -> Int\n  return a * b\nend\n",
    );

    // Cold start.
    let first = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(first.status, 0);

    // Body-only change in a.mj.
    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int) -> Int\n  temp = a + b\n  return temp\nend\n",
    );
    let second = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(second.status, 0);
    assert!(second.stdout.contains("1 checked file(s) (1 skipped)"));
    assert!(!second.stdout.contains("API change(s)"));

    let _ = fs::remove_dir_all(&workspace);
    let _ = fs::remove_dir_all(&cache_dir);
}

#[test]
fn check_changed_new_file() {
    // Start with one file, add a second → second gets checked, first stays skipped.
    let workspace = unique_temp_path("momiji_check_new");
    let cache_dir = unique_temp_path("momiji_check_new_cache");

    write_text_file(
        &workspace.join("a.mj"),
        "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n",
    );

    // First run: 1 file checked.
    let first = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(first.status, 0);

    // Add a new independent file.
    write_text_file(
        &workspace.join("b.mj"),
        "def mul(a: Int, b: Int) -> Int\n  return a * b\nend\n",
    );
    let second = run_check_path(&workspace, true, &cache_dir);
    assert_eq!(second.status, 0);
    // Only the new file should be checked; a.mj is unchanged and independent.
    assert!(second.stdout.contains("1 checked file(s) (1 skipped)"));

    let _ = fs::remove_dir_all(&workspace);
    let _ = fs::remove_dir_all(&cache_dir);
}
