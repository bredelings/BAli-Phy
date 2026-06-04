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
- Reworked `context_ptr` internals so `operator[]`, `size()`, and legacy
  `head()` inspect `Runtime::Exp` rather than `expression_at()` /
  `legacy_exp()`.
- Migrated a focused group of model/tree `context_ptr` scalar/object callers to
  `value_code()` / `set_code()`.
- Migrated SMC haplotype-index lists from `list_to_vector()` to
  `list_to_vector_code()`.
- Added `TODO.md` to capture delayed cleanup work.

## Remaining Hotspots

1. `context_ref` and `reg_heap` still expose legacy evaluation APIs returning
   `expression_ref`: `evaluate_*`, `get_reg_value*`, and `evaluate_program`.
   These should remain only as compatibility wrappers while callers migrate.

2. `context_ptr` still has legacy callers. The runtime-facing methods exist and
   the internals are runtime-based, but tree, parameter, MCMC, and SMC call
   sites still need to keep moving to `value_code()`, `set_code()`, and
   `list_to_vector_code()`. The largest remaining SMC list consumers are
   haplotypes, panel, and sites arrays whose helper functions still take
   `EVector` / `expression_ref`.

3. Non-evaluator `reg_heap::expression_at()` uses remain in graph-register
   helpers, interchangeables, assertions, comparison/debug code, and legacy API
   wrappers.

4. `Runtime::to_expression_ref()` and `closure::legacy_exp()` should eventually
   be limited to parser/model-generation/debug-display boundaries.

## Proposed Next Steps

1. Add runtime-oriented helpers in SMC for haplotype/panel/site vectors, then
   migrate `haplotypes_ptr`, `panel_ptr`, and `sites_ptr` away from
   `list_to_vector()`.

2. Continue migrating direct model/tree call sites that still use
   `context_ptr::value()` only because a legacy-returning helper has not yet
   been split.

3. Replace non-evaluator `reg_heap::expression_at()` call sites where they are
   assertions or comparisons rather than explicit legacy display/API paths.

4. Add runtime equivalents for any missing small helpers encountered during
   migration, but avoid broad renaming until the legacy APIs are unused.

5. After each batch, build `src/bali-phy/bali-phy` from
   `../build/gcc-16-debug-O` and commit logically separate changes with `jj`.
