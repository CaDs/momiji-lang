mod common;

use common::{
    build_fixture, cleanup_file, normalized_program_output, run_cli, run_executable, snapshot_view,
    unique_temp_path,
};
use insta::assert_snapshot;

#[test]
fn build_and_execute_hello() {
    let exe = unique_temp_path("momiji_build_hello");

    let build = build_fixture("hello.mj", &exe);
    assert_eq!(build.status, 0);

    let run = run_executable(&exe);
    assert_eq!(run.status, 0);
    assert_eq!(
        normalized_program_output(&run, "interp"),
        "Hello, Momiji!\n"
    );

    cleanup_file(&exe);
}

#[test]
fn build_and_execute_arithmetic() {
    let exe = unique_temp_path("momiji_build_arithmetic");

    let build = build_fixture("arithmetic.mj", &exe);
    assert_eq!(build.status, 0);

    let run = run_executable(&exe);
    assert_eq!(run.status, 0);
    assert_eq!(normalized_program_output(&run, "interp"), "50\n32\n20\n");

    cleanup_file(&exe);
}

#[test]
fn build_and_execute_function_calls() {
    let exe = unique_temp_path("momiji_build_function_calls");

    let build = build_fixture("function_calls.mj", &exe);
    assert_eq!(build.status, 0);

    let run = run_executable(&exe);
    assert_eq!(run.status, 0);
    assert_eq!(normalized_program_output(&run, "interp"), "30\n");

    cleanup_file(&exe);
}

#[test]
fn build_respects_output_path() {
    let exe = unique_temp_path("momiji_build_output");

    let build = build_fixture("conditionals.mj", &exe);
    assert_eq!(build.status, 0);
    assert!(exe.exists(), "expected executable at {}", exe.display());

    cleanup_file(&exe);
}

#[test]
fn build_rejects_interp_backend() {
    let file = common::fixture_path("hello.mj");
    let result = run_cli(&[
        "build".to_string(),
        file.display().to_string(),
        "--backend".to_string(),
        "interp".to_string(),
    ]);

    assert_ne!(result.status, 0);
    assert_snapshot!("build_rejects_interp_backend", snapshot_view(&result));
}
