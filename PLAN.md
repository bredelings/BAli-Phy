# Runtime Evaluation Cleanup Plan

## Goal

Keep `expression_ref` available where it is still the natural representation:
parsing, model/code generation, legacy display/debug output, and temporary
compatibility wrappers. Move evaluation itself, and the evaluation-facing API,
toward `Runtime::Exp` and `closure`.

The important distinction is not "delete every `expression_ref`", but "avoid
requiring evaluated values to pass through `expression_ref` unless crossing a
legacy boundary".

## Current State

Completed so far:

- Added temporary runtime-returning APIs on `context_ref`, `reg_heap`, and
  `context_ptr`, using `_code` suffixes where legacy expression APIs still
  occupy the original names.
- Migrated obvious MCMC/SMC scalar evaluation callers to runtime values.
- Removed `reg_heap::expression_at()` from the live evaluator loops in
  `src/computation/machine/evaluate.cc`.
- Reworked `context_ptr` internals so `operator[]`, `size()`, and legacy
  `head()` inspect `Runtime::Exp` rather than `expression_at()` /
  `legacy_exp()`.
- Migrated a focused group of model/tree `context_ptr` scalar/object callers to
  `value_code()` / `set_code()`.
- Added `Runtime::RVector` conversions analogous to `EVector`, and migrated SMC
  haplotype-index lists to `list_to_vector_code()`.
- Made `reg_heap::get_reg_value_in_context()` and
  `reg_heap::evaluate_program()` return closures directly, with
  `context_ref` legacy wrappers calling `legacy_exp()` at the API boundary.
  This removed the temporary `*_closure` duplicate names.
- Removed simple `context_ref` uses of `reg_heap::expression_at()` from
  `evaluate_reg()` and `reg_is_modifiable()`; these now use evaluated closures
  or runtime-code predicates directly.
- Replaced non-evaluator `expression_at()` uses in MCMC, Prelude IORef, and
  reroot assertions with runtime-code predicates.
- Added `TODO.md` to capture delayed cleanup work.

## Evaluation Core

The core reduction paths in `evaluate.cc` now inspect `Runtime::Exp` directly.
Remaining `expression_ref` mentions in that file are declarations for legacy or
debug/display helpers.

This means the evaluator-core scope has shrunk. The remaining work is less
about reduction mechanics and more about public evaluation APIs and legacy
compatibility layers.

## Evaluation-Facing API Hotspots

1. `context_ref` still exposes legacy evaluation APIs returning
   `expression_ref`: `evaluate_*`, `get_reg_value*`, `get_modifiable_value`,
   `get_expression`, and `evaluate_program`. Runtime-returning `_code` methods
   exist beside many of these, and several legacy wrappers now delegate through
   closures/runtime code.

2. `reg_heap` still exposes legacy evaluation APIs returning `expression_ref`
   in the program unsharing path: `unshare_and_evaluate_program` and
   `unshare_regs2`. The simpler value/program APIs now return closures
   directly.

3. `reg_heap::expression_at()` remains as a compatibility accessor over
   `closure::legacy_exp()`. It is no longer in evaluator loops, and the simple
   `context_ref` and assertion/check call sites have been removed. Remaining
   live uses are graph-register comparison/debug code, graph display naming,
   and the accessor itself.

4. `closure::legacy_exp()` and `Runtime::to_expression_ref()` remain necessary
   adapters. Long term they should be used at parser/model-generation/display
   boundaries and by explicitly legacy APIs, not by runtime-native evaluation.

## Caller Migration Hotspots

1. `context_ptr` still has legacy callers. The internals are runtime-based, but
   tree, parameter, MCMC, and SMC call sites should keep moving to
   `value_code()`, `set_code()`, and `list_to_vector_code()`.

2. SMC haplotype/panel/site arrays still flow through `EVector` /
   `expression_ref` helper functions. They are good candidates for
   runtime-oriented helper splits after the evaluation API wrappers are clearer.

3. Some model/tree helpers still intentionally return `expression_ref`. These
   should be split only where a runtime-native consumer exists; model generation
   and display paths can stay legacy for now.

## Next Steps

1. Continue making legacy `reg_heap` APIs delegate to closure/runtime-returning
   APIs where possible. This reduces duplicated evaluation code and confines
   `legacy_exp()` conversion to the wrapper edge.

2. Add missing closure/runtime-returning variants for program unsharing paths
   (`unshare_regs2`, `unshare_and_evaluate_program`). Treat this as the next
   larger evaluator-facing API batch because the returned value must remain
   valid across reroot cleanup and token mutation.

3. Convert `context_ref` legacy APIs into thin wrappers over `_code` or
   closure-returning APIs. The easy `evaluate_reg()` / `reg_is_modifiable()`
   cases are done; remaining cases should be handled where lifetime and
   reference-return behavior are clear. Rename to `_legacy` only if the churn is
   modest; otherwise keep temporary suffix comments current.

4. Decide whether `regs_maybe_different_value()` should compare
   `Runtime::Exp` directly or keep using legacy expression equality for now.
   Graph display can stay legacy longer because it intentionally prints
   expression-shaped output.

5. Continue caller migration in focused batches: remaining `context_ptr`
   callers, then SMC helper functions that still require `EVector` /
   `expression_ref`.

6. After each code batch, build `src/bali-phy/bali-phy` from
   `../build/gcc-16-debug-O`, run the relevant focused tests, and commit
   logically separate changes with `jj`.
