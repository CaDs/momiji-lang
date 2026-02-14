# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

Do not add Co-Authored-By trailers to commit messages.


## Project Overview

Momiji is a new programming language with a Rust-based compiler. It combines Ruby-inspired syntax (`def`/`end` blocks, expressive keywords) with Go-like fast incremental compilation, ARC-based memory safety, and fiber/channel concurrency. The spec lives in `docs/lang-spec.md`. The compiler is an MVP subset focused on correctness, diagnostics, and compile speed.

## Build and Test Commands

All compiler commands run from `compiler/`:

```bash
cargo build                          # Build the compiler
cargo test                           # Run full test suite (unit + integration + snapshots)
cargo test <test_name>               # Run a single test by name
cargo test --test cli_check          # Run one integration test file
cargo insta review                   # Review/approve snapshot changes (requires cargo-insta)
```

The compiled `momiji` binary:
```bash
momiji run examples/hello.mj                    # Run with interpreter (default)
momiji run examples/hello.mj --backend cranelift # Run via Cranelift native compilation
momiji build examples/hello.mj -o hello          # Compile to native executable (requires clang for linking)
momiji check examples/hello.mj                   # Type check only
momiji check examples/ --changed --timings       # Incremental directory check with timing
```

**IMPORTANT**: The CLI command is `momiji`, NOT `moji`.

## Architecture

The compiler pipeline flows through four stages, each in its own module:

1. **Lexer** (`compiler/src/lexer/`) - Tokenizes `.mj` source. `Scanner` produces `Token` vectors.
2. **Parser** (`compiler/src/parser/`) - Produces an untyped AST (`ast.rs`). Currently only supports `Item::Function`.
3. **Semantic lowering** (`compiler/src/sem/`) - `SemLowerer` transforms AST into typed `SemProgram` IR (`ir.rs`), validating types, return paths, and reachability. This is the single source of truth consumed by both backends.
4. **Backends** - Two execution backends consume `SemProgram`:
   - **Interpreter** (`compiler/src/runtime/interpreter.rs`) - Tree-walking evaluator, default for `run`
   - **Cranelift** (`compiler/src/codegen/cranelift.rs`) - Native code generation via Cranelift, used by `build` and `run --backend cranelift`. Links with `clang`.

Supporting modules:
- `compiler/src/types/types.rs` - Type enum (`Int`, `Float`, `Bool`, `String`, `Array`, `Nullable`, etc.) and type-level operations
- `compiler/src/errors/mod.rs` - All error types (`LexerError`, `TypeError`, `RuntimeError`, `CodegenError`) using `miette` diagnostics

CLI entrypoint (`compiler/src/main.rs`) handles commands, timing (`--timings`), and an incremental check cache (`--changed`) using file fingerprints + API signature hashing.

## Testing

Tests live in `compiler/tests/` and use fixture programs from `compiler/tests/programs/*.mj`:

- **`cli_check.rs`** - `momiji check` diagnostics (valid programs, type errors, missing returns, etc.)
- **`cli_run_interp.rs`** / **`cli_run_cranelift.rs`** - Backend-specific execution tests
- **`cli_build.rs`** - Native compilation tests
- **`backend_parity.rs`** - Asserts both backends produce identical output for shared programs
- **`common/mod.rs`** - Test harness with helpers (`run_cli`, `fixture_path`, `snapshot_view`, `normalize_text`)

Snapshot files are in `compiler/tests/snapshots/` and use the `insta` crate. Diagnostics changes require snapshot updates.

**Testing policy** (from CONTRIBUTING.md):
- Behavior changes require tests at each affected layer
- Cross-backend features need parity tests in both interpreter and Cranelift
- Diagnostics regressions must be protected with snapshots
- New fixtures go in `compiler/tests/programs/`

## Current Language Features (MVP)

Implemented: functions, variables (mutable by default), `let`/assignment, `if`/`else`, `while`, `for`/ranges, arrays with indexing, basic types (`Int`, `Float`, `Bool`, `String`), arithmetic/comparison/logical operators, `print`/`println` builtins, function calls with parameters and return values.

Not yet implemented: modules/imports, structs/classes, pattern matching, interfaces/generics/enums, concurrency, ARC/ownership.

## Key Design Decisions

- **Variables are mutable by default** (spec-aligned). `let mut` is a compatibility alias, not a semantic requirement.
- **Compile speed first, language breadth second**. Incremental check infrastructure and diagnostics quality are prioritized over new language features.
- **Conservative correctness**: When an API signature changes during `check --changed`, all files in the target set are rechecked (no dependency graph yet).

## Spec Reference

`docs/lang-spec.md` is the authoritative language specification. Key syntax conventions when writing Momiji code or extending the parser:
- `def`/`end` blocks, `pub` visibility, `property` for fields
- `@` prefix for instance variable access
- `match`/`when` with `=>` arms
- `?` suffix for nullable types, `?` operator for Result propagation
- `module A::B` declarations, `import Std::Collections.(Array, Map)`
- File extension: `.mj`, directory module file: `mod.mj`
