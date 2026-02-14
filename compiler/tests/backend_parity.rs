mod common;

use common::{normalized_program_output, run_cranelift_fixture, run_interp_fixture};

fn assert_backend_parity(program: &str) {
    let interp = run_interp_fixture(program);
    assert_eq!(interp.status, 0, "interp failed for {}", program);

    let cranelift = run_cranelift_fixture(program);
    assert_eq!(cranelift.status, 0, "cranelift failed for {}", program);

    let interp_out = normalized_program_output(&interp, "interp");
    let cranelift_out = normalized_program_output(&cranelift, "cranelift");

    assert_eq!(interp_out, cranelift_out, "output mismatch for {}", program);
}

#[test]
fn parity_hello() {
    assert_backend_parity("hello.mj");
}

#[test]
fn parity_arithmetic() {
    assert_backend_parity("arithmetic.mj");
}

#[test]
fn parity_conditionals() {
    assert_backend_parity("conditionals.mj");
}

#[test]
fn parity_while_loop() {
    assert_backend_parity("while_loop.mj");
}

#[test]
fn parity_function_calls() {
    assert_backend_parity("function_calls.mj");
}

#[test]
fn parity_branch_returns() {
    assert_backend_parity("branch_returns_ok.mj");
}

#[test]
fn parity_arrays_index() {
    assert_backend_parity("arrays_index.mj");
}

#[test]
fn parity_ranges() {
    assert_backend_parity("ranges.mj");
}

#[test]
fn parity_reassign_print() {
    assert_backend_parity("reassign_print.mj");
}

#[test]
fn parity_struct_basic() {
    assert_backend_parity("struct_basic.mj");
}

#[test]
fn parity_struct_in_function() {
    assert_backend_parity("struct_in_function.mj");
}

#[test]
fn parity_match_literal() {
    assert_backend_parity("match_literal.mj");
}

#[test]
fn parity_match_variable() {
    assert_backend_parity("match_variable.mj");
}

#[test]
fn parity_match_in_function() {
    assert_backend_parity("match_in_function.mj");
}
