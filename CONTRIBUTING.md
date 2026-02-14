# Contributing

## Testing Policy

Momiji uses a strict feature gate for test coverage.

- Any change that adds or alters language behavior must include tests for each affected layer.
- If a feature impacts both execution backends, add coverage for both interpreter and Cranelift.
- Diagnostics regressions must be protected with snapshots.
- New fixtures should be added under `compiler/tests/programs/`.

## Required Checks Before Merge

Run from `compiler/`:

```bash
cargo test
```

This runs unit tests, integration tests, and snapshot assertions.
