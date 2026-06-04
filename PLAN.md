# Runtime Evaluation Cleanup Plan

## Current State

`expression_ref` is still expected at the parser/model-generation boundary, but
evaluation should continue moving toward `Runtime::Exp` and `closure`.

Completed so far:

- Added temporary runtime-returning APIs on `context_ref`, `reg_heap`, and
  `context_ptr`, using `_code` / `_closure` suffixes with comments that these
  suffixes should be removed once legacy APIs go away.
- Migrated obvious MCMC/SMC scalar evaluation callers to runtime values.
- Removed `reg_heap::expression_at()` from the live evaluator loops in
  `src/computation/machine/evaluate.cc`.
- Added `TODO.md` to capture delayed cleanup work.

## Remaining Hotspots

1. `context_ref` and `reg_heap` still expose legacy evaluation APIs returning
   `expression_ref`: `evaluate_*`, `get_reg_value*`, and `evaluate_program`.
   These should remain only as compatibility wrappers while callers migrate.

2. `context_ptr` still has legacy internals and callers. The runtime-facing
   methods exist, but `operator[]`, `size()`, and legacy `head()` still need to
   avoid `expression_at()` / `legacy_exp()`. Tree, parameter, MCMC, and SMC
   call sites should move to `value_code()`, `set_code()`, and
   `list_to_vector_code()`.

3. Non-evaluator `reg_heap::expression_at()` uses remain in graph-register
   helpers, interchangeables, assertions, comparison/debug code, and legacy API
   wrappers.

4. `Runtime::to_expression_ref()` and `closure::legacy_exp()` should eventually
   be limited to parser/model-generation/debug-display boundaries.

## Proposed Next Steps

1. Replace `context_ptr` internals that inspect evaluated values through
   `expression_at()` or `legacy_exp()` with direct `Runtime::Exp` inspection.

2. Convert a focused group of model/tree call sites from `context_ptr::value()`
   and `set_value()` to `value_code()` and `set_code()`.

3. Convert SMC list consumers that call `context_ptr::list_to_vector()` to
   `list_to_vector_code()` where the values are immediately used as runtime
   scalars or runtime object values.

4. Add runtime equivalents for any missing small helpers encountered during
   migration, but avoid broad renaming until the legacy APIs are unused.

5. After each batch, build `src/bali-phy/bali-phy` from
   `../build/gcc-16-debug-O` and commit logically separate changes with `jj`.
