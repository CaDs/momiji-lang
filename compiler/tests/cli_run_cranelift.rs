mod common;

use common::{normalized_program_output, run_cranelift_fixture, snapshot_view};
use insta::assert_snapshot;

#[test]
fn run_cranelift_hello() {
    let result = run_cranelift_fixture("hello.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("run_cranelift_hello", snapshot_view(&result));
}

#[test]
fn run_cranelift_arithmetic() {
    let result = run_cranelift_fixture("arithmetic.mj");
    assert_eq!(result.status, 0);
    assert_eq!(
        normalized_program_output(&result, "cranelift"),
        "50\n32\n20\n"
    );
}

#[test]
fn run_cranelift_comparisons() {
    let result = run_cranelift_fixture("comparisons.mj");
    assert_eq!(result.status, 0);
    assert_eq!(
        normalized_program_output(&result, "cranelift"),
        "1\n0\n1\n1\n1\n0\n"
    );
}

#[test]
fn run_cranelift_conditionals() {
    let result = run_cranelift_fixture("conditionals.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "cranelift"), "1\n2\n");
}

#[test]
fn run_cranelift_while_loop() {
    let result = run_cranelift_fixture("while_loop.mj");
    assert_eq!(result.status, 0);
    assert_eq!(
        normalized_program_output(&result, "cranelift"),
        "1\n2\n3\n4\n5\n"
    );
}

#[test]
fn run_cranelift_function_calls() {
    let result = run_cranelift_fixture("function_calls.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "cranelift"), "30\n");
}

#[test]
fn run_cranelift_branch_returns() {
    let result = run_cranelift_fixture("branch_returns_ok.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "cranelift"), "10\n20\n");
}

#[test]
fn run_cranelift_arrays_index() {
    let result = run_cranelift_fixture("arrays_index.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "cranelift"), "20\n");
}

#[test]
fn run_cranelift_ranges() {
    let result = run_cranelift_fixture("ranges.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "cranelift"), "3\n5\n6\n");
}

#[test]
fn run_cranelift_for_loop_unsupported() {
    let result = run_cranelift_fixture("for_loop.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!("run_cranelift_for_unsupported", snapshot_view(&result));
}

#[test]
fn run_cranelift_string_index_unsupported() {
    let result = run_cranelift_fixture("string_index_codegen_fail.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!(
        "run_cranelift_string_index_unsupported",
        snapshot_view(&result)
    );
}

#[test]
fn run_cranelift_float_array_unsupported() {
    let result = run_cranelift_fixture("float_array_codegen_fail.mj");
    assert_ne!(result.status, 0);
    assert_snapshot!(
        "run_cranelift_float_array_unsupported",
        snapshot_view(&result)
    );
}
