# Momiji Implementation Plan (Handoff Document)

Last updated: 2026-02-13
Scope: `momiji-lang` workspace (primarily `compiler/` crate)

## 1. Purpose

This document captures:
- What has been implemented so far.
- Why key decisions were made.
- What is left, in execution order.
- How another agent can continue work safely and quickly.

Primary product intent is from `docs/lang-spec.md`, but implementation is an MVP subset focused on correctness, diagnostics, and compile speed.

## 2. Product Direction (Current)

Current direction (explicitly chosen):
- Prioritize fast compile/check loops and predictable tooling.
- Keep syntax readable and simple (Ruby/Crystal-inspired), but avoid expensive language machinery early.
- De-prioritize blocks/closures for now to avoid compile-time and implementation complexity.

Practical interpretation:
- Incremental checks and compiler observability are first-class.
- Semantic correctness and test resilience are mandatory gates.
- New language features should be staged only after fast/robust core loops are stable.

## 3. Current Architecture Snapshot

Pipeline:
1. Lexing (`compiler/src/lexer`)
2. Parsing (`compiler/src/parser`)
3. Semantic lowering + type validation to typed semantic IR (`compiler/src/sem`)
4. Backend execution:
- Interpreter backend (`compiler/src/runtime/interpreter.rs`)
- Cranelift backend (`compiler/src/codegen/cranelift.rs`)

CLI entrypoint:
- `compiler/src/main.rs`
- Commands: `check`, `run`, `build`
- Backends: `interp`, `cranelift`

Important implementation status:
- Shared semantic IR is the source of truth for both backends.
- `check` now supports file and directory targets.
- `check --changed` supports metadata + API fingerprint cache.
- `--timings` exists on `check`, `run`, `build`.

## 4. Decision Log (Why)

### D1. Mutable-by-default variables
Decision:
- Variables are mutable by default (spec-aligned).
- `let mut` is retained as a compatibility alias, not semantic requirement.

Why:
- Aligns with intended language ergonomics.
- Removes friction and contradictory semantics.
- Simplifies runtime and semantic layers.

Impact:
- Immutable-variable diagnostics were removed.
- AST/SemIR symbol mutability flags were removed.

### D2. Compile speed first, language breadth second
Decision:
- Focused on `check` fast paths, caching, timing visibility, and test resilience before adding advanced features.

Why:
- User priority is very fast iteration on larger codebases.
- Reduces risk of accumulating hard-to-debug performance regressions.

Impact:
- Implemented `--timings`, `--changed`, directory check.
- Added conservative API-change invalidation behavior.

### D3. Conservative correctness over aggressive optimization
Decision:
- On API fingerprint changes, re-check all files in the target set.

Why:
- Dependency graph is not implemented yet.
- Avoid false negatives while still gaining body-only incremental wins.

Impact:
- Body-only edits can be cheap.
- API edits are safe but broad.

### D4. Testing as a release gate
Decision:
- Every behavior change requires direct tests, backend parity where relevant, snapshots for diagnostics.

Why:
- Prevent regressions while iterating quickly.
- Keep confidence high during architecture changes.

Impact:
- Test suite expanded significantly and now covers CLI/backend semantics in depth.

## 5. What Has Been Completed

## Milestone A: Semantic IR and frontend rigor
Status: Done

Delivered:
- Semantic lowering is core validation path.
- Return-path diagnostics include missing-return and unreachable-code detection.
- Backends consume shared semantic model.

Relevant files:
- `compiler/src/sem/lower.rs`
- `compiler/src/sem/ir.rs`
- `compiler/src/main.rs`

## Milestone B: Cleanup and dead code reduction
Status: Done

Delivered:
- Removed immutable-variable machinery across AST/SemIR/runtime/error paths.
- Simplified symbol tables and assignment behavior.
- Removed obsolete immutable reassign fixture/snapshot.

Relevant files:
- `compiler/src/parser/ast.rs`
- `compiler/src/sem/ir.rs`
- `compiler/src/runtime/interpreter.rs`
- `compiler/src/errors/mod.rs`
- `compiler/tests/programs/reassign_ok.mj`

## Milestone C: High-impact test expansion
Status: Done

Delivered:
- Integration harness and fixture-driven CLI tests.
- Snapshot diagnostics for `check` and backend unsupported paths.
- Backend parity tests for shared behavior.

Current suite size:
- 87 tests passing (`cargo test` from `compiler/`).

Relevant files:
- `compiler/tests/cli_check.rs`
- `compiler/tests/cli_run_interp.rs`
- `compiler/tests/cli_run_cranelift.rs`
- `compiler/tests/backend_parity.rs`
- `compiler/tests/common/mod.rs`
- `compiler/tests/snapshots/*`

## Milestone D: Compile/check observability
Status: Done

Delivered:
- `--timings` for `check`, `run`, `build`.
- Frontend phase timing breakdown (read/lex/parse/sem).

Relevant files:
- `compiler/src/main.rs`

## Milestone E: Incremental check fast path (single file)
Status: Done

Delivered:
- `check --changed` skips unchanged files using fingerprint cache.

Relevant files:
- `compiler/src/main.rs`

## Milestone F: Directory check + API fingerprint invalidation
Status: Done

Delivered:
- `check <dir>` recursively checks `.mj` files.
- Cache now includes API fingerprint (function signatures).
- Body-only edit: only changed files are checked.
- API edit: conservative downstream re-check of full target set.

Relevant files:
- `compiler/src/main.rs`
- `compiler/tests/cli_check.rs`
- `compiler/tests/common/mod.rs`

## 6. Known Gaps / Constraints

Language gaps intentionally not implemented yet:
- Modules/imports/visibility.
- Struct/class/property system.
- Match/when.
- Interfaces/generics/enums.
- Concurrency model.
- ARC/ownership model.

Backend feature gaps:
- Cranelift still has explicit unsupported behavior covered by tests (e.g. some loop/string/array paths).

Incremental check limitation:
- API-change invalidation is currently coarse (all files in target set).

## 7. Next Milestones (Recommended)

## Milestone G: Precise dependency-aware invalidation
Priority: Highest

Goal:
- Replace coarse “API changed -> recheck all files” with targeted downstream invalidation.

Plan:
1. Introduce per-file symbol export/import metadata in cache.
2. Build file-level dependency graph during check for directory targets.
3. On API hash change, recheck only impacted dependents (transitively).

Acceptance criteria:
- Body-only changes recheck one file.
- API changes only recheck changed file + reachable dependents.
- No correctness regressions in diagnostics.

Likely touchpoints:
- `compiler/src/main.rs`
- New cache/dependency module under `compiler/src/` (recommended extraction from main)
- `compiler/tests/cli_check.rs` additional dependency graph tests

## Milestone H: Parallel frontend checking
Priority: High

Goal:
- Parallelize lex/parse/sem for independent file checks.

Plan:
1. Add worker pool (or `rayon`) for frontend execution.
2. Keep deterministic output ordering (sort by file path).
3. Keep error formatting stable for snapshots.

Acceptance criteria:
- Multi-file check throughput improves on >4 file targets.
- Output and diagnostics remain deterministic.

## Milestone I: Cache format/version hardening
Priority: High

Goal:
- Avoid stale caches after compiler/language changes.

Plan:
1. Add explicit cache version header.
2. Include invalidation key fields (compiler version, language version, backend-affecting flags).
3. Handle incompatible cache with clean reset path.

Acceptance criteria:
- Safe behavior across upgrades.
- Clear user-facing message on cache invalidation.

## Milestone J: Diagnostics aggregation for directory mode
Priority: Medium

Goal:
- Better UX for large checks.

Plan:
1. Aggregate errors per file.
2. Print a summary table at end (checked/skipped/errors).
3. Optional fail-fast flag in future.

Acceptance criteria:
- Multi-file error runs are easier to navigate.
- Existing single-file diagnostics remain unchanged.

## Milestone K: Language feature growth (gated)
Priority: After G/H/I

Candidate order (recommended):
1. Minimal module/import model.
2. Struct and field access semantics.
3. Pattern matching (`match`).

Constraint:
- Each feature must preserve incremental-check performance goals and include backend parity tests.

## 8. Agent Handoff: How To Continue Safely

## Quick start
- Read `docs/lang-spec.md` section 1.0 for MVP scope.
- Read `compiler/src/main.rs` for CLI and check pipeline.
- Run `cargo test` from `compiler/` before changes.

## Testing contract
From `CONTRIBUTING.md`:
- Behavior changes require tests at impacted layers.
- Cross-backend behavior requires coverage in both backends.
- Diagnostics changes require snapshot updates.

## Useful test entry points
- Incremental check behavior: `compiler/tests/cli_check.rs`
- Backend behavior parity: `compiler/tests/backend_parity.rs`
- CLI execution paths: `compiler/tests/cli_run_interp.rs`, `compiler/tests/cli_run_cranelift.rs`, `compiler/tests/cli_build.rs`

## High-risk areas
- `compiler/src/main.rs` is currently handling many concerns (CLI, timing, cache, traversal).
- Cache correctness depends on stable keying and deterministic traversal.
- Error ordering may become nondeterministic once parallelism is introduced.

## Refactor recommendation before large additions
- Extract check/cache logic from `main.rs` into dedicated modules:
  - `compiler/src/check/mod.rs`
  - `compiler/src/check/cache.rs`
  - `compiler/src/check/deps.rs`

This will reduce regression risk for milestones G/H/I.

## 9. Current Exit Criteria for “Compiler Core Stable”

Proposed conditions before large language feature growth:
- Dependency-aware incremental check implemented and tested.
- Parallel check path implemented with deterministic output.
- Cache format versioned and robust.
- `cargo test` remains green with broad parity coverage.

## 10. Summary

The project is no longer in “raw MVP only” state; it now has:
- Stronger semantic validation,
- Better test resilience,
- Practical compile/check observability,
- First incremental check infrastructure.

The next leg should focus on making incremental checks precise and scalable (dependency-aware + parallel), then proceed to language feature growth.
