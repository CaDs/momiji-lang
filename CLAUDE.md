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
```

Snapshot approval: `cargo-insta` is not installed. When new snapshots are created, rename `.snap.new` files to `.snap` after verifying the output is correct:
```bash
for f in compiler/tests/snapshots/*.snap.new; do mv "$f" "${f%.new}"; done
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
2. **Parser** (`compiler/src/parser/`) - Produces an untyped AST (`ast.rs`). Top-level items: `Item::Function` and `Item::Struct`.
3. **Semantic lowering** (`compiler/src/sem/`) - `SemLowerer` transforms AST into typed `SemProgram` IR (`ir.rs`) using three-pass lowering: (1) register struct signatures, (2) register function signatures, (3) lower function bodies. Validates types, return paths, and reachability. This is the single source of truth consumed by both backends.
4. **Backends** - Two execution backends consume `SemProgram`:
   - **Interpreter** (`compiler/src/runtime/interpreter.rs`) - Tree-walking evaluator, default for `run`
   - **Cranelift** (`compiler/src/codegen/cranelift.rs`) - Native code generation via Cranelift, used by `build` and `run --backend cranelift`. Links with `clang`.

Supporting modules:
- `compiler/src/types/types.rs` - Type enum (`Int`, `Float`, `Bool`, `String`, `Array`, `Struct`, `Nullable`, etc.) and type-level operations
- `compiler/src/errors/mod.rs` - All error types (`LexerError`, `ParseError`, `TypeError`, `RuntimeError`, `CodegenError`) using `miette` diagnostics
- `compiler/src/check/` - Incremental check: cache with version header (`cache.rs`), dependency graph and API hash invalidation (`deps.rs`), directory-mode error aggregation (`mod.rs`)

CLI entrypoint (`compiler/src/main.rs`) handles commands, timing (`--timings`), and an incremental check cache (`--changed`) using file fingerprints + API signature hashing.

### Adding a new language feature

New features typically touch all of these files in order:
1. `parser/ast.rs` - Add AST node types (Stmt variant, supporting structs)
2. `parser/parser.rs` - Parse the new syntax
3. `sem/ir.rs` - Add Sem IR types
4. `sem/lower.rs` - Lower AST to typed IR with validation
5. `runtime/interpreter.rs` - Interpret the new IR node
6. `codegen/cranelift.rs` - Compile the new IR node to native code
7. `types/checker.rs` - Handle new Stmt/Item variants (exhaustiveness)
8. `check/deps.rs` - Handle new Stmt variants in `collect_calls_stmt`

## Testing

Tests live in `compiler/tests/` and use fixture programs from `compiler/tests/programs/*.mj`:

- **`cli_check.rs`** - `momiji check` diagnostics (valid programs, type errors, missing returns, etc.)
- **`cli_run_interp.rs`** / **`cli_run_cranelift.rs`** - Backend-specific execution tests
- **`cli_build.rs`** - Native compilation tests
- **`backend_parity.rs`** - Asserts both backends produce identical output for shared programs
- **`common/mod.rs`** - Test harness with helpers (`run_cli`, `fixture_path`, `snapshot_view`, `normalized_program_output`)

Snapshot files are in `compiler/tests/snapshots/` and use the `insta` crate. Diagnostics changes require snapshot updates. The test harness normalizes paths (`<MANIFEST_DIR>`, `<TMP_DIR>`) and strips linker warnings for cross-platform reproducibility.

**Testing policy** (from CONTRIBUTING.md):
- Behavior changes require tests at each affected layer
- Cross-backend features need parity tests in both interpreter and Cranelift
- Diagnostics regressions must be protected with snapshots
- New fixtures go in `compiler/tests/programs/`

## Current Language Features (MVP)

Implemented: functions, variables (mutable by default), `let`/assignment, `if`/`else`/`elsif`, `while`, `for`/ranges, arrays with indexing, structs with `property` fields and `.new()` constructors, `match`/`when` pattern matching (literal, variable binding, wildcard patterns), basic types (`Int`, `Float`, `Bool`, `String`), arithmetic/comparison/logical operators, `print`/`puts` builtins, function calls with parameters and return values.

Not yet implemented: modules/imports, interfaces/generics/enums, concurrency, ARC/ownership. String comparison in Cranelift backend is pointer-based (only works for same literal); string `match` is interpreter-only for now.

## Key Design Decisions

- **Variables are mutable by default** (spec-aligned). `let mut` is a compatibility alias, not a semantic requirement.
- **Compile speed first, language breadth second**. Incremental check infrastructure and diagnostics quality are prioritized over new language features.
- **Dependency-aware cache invalidation**: API signature hashing detects when a function/struct's public interface changes and rechecks dependents. Body-only edits skip downstream files.
- **Directory mode error aggregation**: When checking a directory, all files are processed and all errors reported (not fail-fast). Cache is updated for successful files even when some fail.
- **Structs in Cranelift**: Heap-allocated as flat I64 pointers with 8 bytes per field at `field_index * 8` offset.

## Spec Reference

`docs/lang-spec.md` is the authoritative language specification. Key syntax conventions when writing Momiji code or extending the parser:
- `def`/`end` blocks, `pub` visibility, `property` for struct fields
- `struct Name ... end` with `property name : Type` fields, `StructName.new(args)` constructor
- `match`/`when` with `=>` arms, `_` wildcard, variable binding patterns
- `@` prefix for instance variable access (not yet implemented)
- `?` suffix for nullable types, `?` operator for Result propagation (not yet implemented)
- `module A::B` declarations, `import Std::Collections.(Array, Map)` (not yet implemented)
- File extension: `.mj`, directory module file: `mod.mj`
