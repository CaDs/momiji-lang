mod common;

use common::{normalized_program_output, run_interp_fixture, snapshot_view};
use insta::assert_snapshot;

#[test]
fn run_interp_hello() {
    let result = run_interp_fixture("hello.mj");
    assert_eq!(result.status, 0);
    assert_snapshot!("run_interp_hello", snapshot_view(&result));
}

#[test]
fn run_interp_arithmetic() {
    let result = run_interp_fixture("arithmetic.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "50\n32\n20\n");
}

#[test]
fn run_interp_comparisons() {
    let result = run_interp_fixture("comparisons.mj");
    assert_eq!(result.status, 0);
    assert_eq!(
        normalized_program_output(&result, "interp"),
        "true\nfalse\ntrue\ntrue\ntrue\nfalse\n"
    );
}

#[test]
fn run_interp_conditionals() {
    let result = run_interp_fixture("conditionals.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "1\n2\n");
}

#[test]
fn run_interp_while_loop() {
    let result = run_interp_fixture("while_loop.mj");
    assert_eq!(result.status, 0);
    assert_eq!(
        normalized_program_output(&result, "interp"),
        "1\n2\n3\n4\n5\n"
    );
}

#[test]
fn run_interp_function_calls() {
    let result = run_interp_fixture("function_calls.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "30\n");
}

#[test]
fn run_interp_branch_returns() {
    let result = run_interp_fixture("branch_returns_ok.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "10\n20\n");
}

#[test]
fn run_interp_arrays_index() {
    let result = run_interp_fixture("arrays_index.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "20\n");
}

#[test]
fn run_interp_ranges() {
    let result = run_interp_fixture("ranges.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "3\n5\n6\n");
}

#[test]
fn run_interp_for_loop() {
    let result = run_interp_fixture("for_loop.mj");
    assert_eq!(result.status, 0);
    assert_eq!(normalized_program_output(&result, "interp"), "1\n2\n3\n");
}
