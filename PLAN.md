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
- Removed `reg_heap::expression_at()` entirely. Legacy display code now calls
  `closure_at(...).legacy_exp()` directly at the display boundary.
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
- Reworked `regs_maybe_different_value()` to inspect `Runtime::Exp` directly
  while preserving its narrow equal-integer shortcut.
- Changed the program unsharing helpers (`unshare_regs2` and
  `unshare_and_evaluate_program`) to return `void`; `evaluate_program()` is now
  the closure-returning API for program results.
- Removed the `Runtime::rpair_first/second(expression_ref)` overloads; legacy
  EVector callers now unpack `RPair` explicitly, while runtime callers use the
  `Runtime::Exp` overloads.
- Audited `closure::legacy_exp()` callers and converted the easy evaluator-side
  cases: lambda-as-object diagnostics now print runtime code, IORef assertions
  check runtime constructors, `apply_op` reports runtime code for non-lambdas,
  and `reg_heap::set_C()` checks pinned runtime `RegRef`s without converting
  the closure to `expression_ref`.
- Peeled off some `show_graph.cc` work: graph reachability now discovers
  `GCObject` dependencies through `Runtime::ObjectValue`, graph/factor-graph
  record detection and record edge extraction inspect `Runtime::Exp`, and
  modifiable/small-constant decisions use runtime predicates. Label rendering
  remains an expression/display boundary.
- Restored `deindexify(const closure&)` to return `expression_ref` through the
  expression-level `deindexify(...)`, with closure environment registers
  supplied as `reg_var`s. The name should mean "undo indexify", not "resolve a
  runtime closure environment".
- Removed the unused `context_ref::recursive_evaluate_reg()` and
  `recursive_evaluate_head()` helpers. They were the only non-display pressure
  for the misleading runtime closure-env `deindexify` behavior.
- Migrated `context_ref`'s public evaluation API in `context.H` to runtime
  values: `evaluate_*`, `perform_*`, `get_reg_value`,
  `get_modifiable_value(s)`, `get_expression`, and `evaluate_program` now
  return `Runtime::Exp` / `Runtime::RVector` instead of `expression_ref` /
  `EVector`. The temporary `_code` duplicates for these APIs were removed, and
  remaining legacy callers bridge explicitly with `Runtime::to_expression_ref(...)`
  where needed.
- Added `TODO.md` to capture delayed cleanup work.

## Evaluation Core

The core reduction paths in `evaluate.cc` now inspect `Runtime::Exp` directly.
Evaluator exception formatting crosses to expression-level display transforms
when it calls `deindexify(...)`, `untranslate_vars(...)`, and `unlet(...)`.
That is intentional for now: these functions are named as inverses of Core /
graph normalization passes, and the current implementations operate on
expression syntax.

This means the evaluator-core scope has shrunk. The remaining work is less
about reduction mechanics and more about public evaluation APIs and legacy
compatibility layers.

## Evaluation-Facing API Hotspots

1. `context_ref` no longer declares expression-returning evaluation APIs in
   `context.H`. Its public evaluation methods now return `Runtime::Exp` or
   `Runtime::RVector`, and the old `_code` disambiguators have been removed.
   `context.cc` still contains explicit legacy bridges at display/debug
   boundaries, but callers of the public API no longer receive
   `expression_ref` from `context_ref`.

2. `reg_heap` no longer has evaluator-facing APIs returning `expression_ref`
   except graph display/debug helpers. Program unsharing is side-effect only,
   and `evaluate_program()` returns the resulting closure.

3. Graph display remains a legacy-expression boundary for label formatting.
   Graph structure and simple predicates inspect `Runtime::Exp`, but
   `untranslate_vars`, `subst_reg_vars`, `unlet`, and `map_symbol_names` remain
   expression-level display transforms. There is no
   `reg_heap::expression_at()` compatibility accessor anymore.

4. `closure::legacy_exp()` and `Runtime::to_expression_ref()` remain necessary
   adapters. Long term they should be used at parser/model-generation/display
   boundaries and by explicitly legacy APIs, not by runtime-native evaluation.
   `Runtime::atomic_value()` and `Runtime::e_op_value()` still take
   `expression_ref` because they bridge legacy literal/constructor values into
   runtime values.

## Inverse Preprocess Pipeline

The forward Core-to-runtime preprocessing pipeline is:

1. `graph_normalize(...)`
2. `runtime_indexify(...)`
3. `Runtime::trim_normalize(...)`
4. `capture_local_reg_refs(...)` / `translate_refs(...)`

The reverse pipeline should run in the opposite order, without routing through
`expression_ref`:

1. untranslate runtime register references back to runtime names where the
   register is known as a global identifier;
2. `Runtime::trim_unnormalize(...)`;
3. `runtime_deindexify(...)`, converting de Bruijn indexed `Runtime::Exp` back
   to named `Core::Exp<>`;
4. graph/let unnormalization only where it is genuinely the inverse of a Core
   normalization pass.

Tests should check both stage-level round trips and the end-to-end result.
Stage-level tests make binder and trim bugs easier to localize; final tests
should compare normalized meaning, not raw Core syntax. A good final assertion
is that re-running the forward runtime preparation on recovered Core produces
the same runtime expression as the normalized original. Exact raw Core equality
is only appropriate for deliberately normalized test inputs with predictable
fresh names.

## `legacy_exp()` Audit

The remaining `legacy_exp()` callers fall into a few buckets:

1. `context_ref`'s public evaluation wrappers have moved to runtime returns.
   Remaining context-side bridges to `expression_ref` are explicit calls to
   `Runtime::to_expression_ref(...)` for display/debug formatting.

2. Graph/debug display deliberately crosses to `expression_ref` for label
   formatting after `deindexify(...)`. Runtime graph traversal and predicates
   should stay runtime-native, but display cleanup should not redefine
   `deindexify` as a closure environment resolver.

3. `closure::print()` is still an expression-facing compatibility helper.
   `deindexify(const closure&)` is also an expression-facing compatibility
   helper because it undoes `indexify` into named/register-variable syntax.

4. Runtime serialization tests intentionally compare runtime code with the
   cached legacy expression view.

5. `IntMap::restrictKeysToVector` still uses an internal `makeEVector`
   operation that materializes an `EVector`. This is a good candidate for a
   focused `RVector` migration, but changing it should be considered together
   with callers that expect the legacy vector representation.

## Caller Migration Hotspots

1. `context_ptr` still has legacy callers. The internals are runtime-based, but
   it still exposes legacy helpers in `param.H`: `value()`, `set_value()`,
   `head()`, and `list_to_vector()`. Runtime alternatives exist for most of
   these (`value_code()`, `set_code()`, `head_code()`,
   `list_to_vector_code()`), so this is now the next evaluation-facing API
   surface to shrink.

2. Builtin data structures still expose evaluated values as legacy containers.
   Small candidates are `EMaybe` in Prelude/Range/Matrix and
   `IntMap::restrictKeysToVector`, which still materializes an `EVector`
   through an internal `makeEVector` operation. Larger candidates are
   `Vector.cc`, `Distribution.cc`, `PopGen.cc`, and SMC helper paths.

3. SMC haplotype/panel/site arrays still flow through `EVector` /
   `expression_ref` helper functions. Some call sites already use
   `Runtime::Exp` for scalar evaluation, but many sequence/read/haplotype
   helpers still accept `EVector`. These should move in focused groups after
   the smaller vector/maybe fronts establish the pattern.

4. Display/debug bridges remain explicit. `evaluate.cc` diagnostics,
   `closure::print()`, graph labels, compact graph display, and some graph head
   formatting are expression-shaped compatibility boundaries.

5. Model/code generation remains intentionally expression-based. Occurrences
   in `models/code-generation.*`, `models/logger.cc`, and parsed/model AST
   utilities are outside the evaluator cleanup unless a runtime-native consumer
   appears.

## Current Scan

Recent scan results:

- `context.H` has no textual `expression_ref` declarations and no temporary
  context `_code` APIs.
- Evaluator-core reduction code uses runtime expressions. Exception formatting
  intentionally crosses to expression-level display transforms.
- `param.H` / `param.cc` is the main evaluation-adjacent header still exposing
  legacy expression helpers through `context_ptr` and `param::ref()`.
- `closure.H` still owns `legacy_exp()` and its expression cache. This is the
  explicit compatibility bridge for display/debug output.
- `runtime/ast` still contains conversion bridges:
  `atomic_value(const expression_ref&)`, `e_op_value(const expression_ref&)`,
  and `to_expression_ref(...)`.
- `show_graph.cc` is runtime-native for graph structure and predicates, but
  expression-based for label formatting and compact display helpers.
- Large `EVector` surfaces remain in likelihood/substitution and several
  builtins. These are evaluated-value containers, but they are broader than the
  evaluator API itself and should be migrated incrementally.

## Next Steps

1. Clean up `context_ptr` in small pieces: migrate remaining callers of
   `value()`, `set_value()`, `head()`, and `list_to_vector()` to runtime
   variants, then remove or rename the legacy helpers.

2. Add `Runtime::RMaybe` and migrate the small `EMaybe` builtin surface
   (`Prelude`, `Range`, `Matrix`) to runtime-native optional values.

3. Convert `IntMap::restrictKeysToVector` / `makeEVector` to return
   `Runtime::RVector`, with explicit legacy bridging only for callers that
   still require `EVector`.

4. Continue SMC vector migration in narrow groups: replace
   `context_ptr::list_to_vector()` uses with `list_to_vector_code()` and convert
   helper signatures from `EVector` to `Runtime::RVector` where the helper only
   needs scalar vector access.

5. Decide whether `Runtime::atomic_value()` / `Runtime::e_op_value()` should
   move out of `runtime/ast` into an explicit legacy conversion module, or
   whether callers should first migrate away from expression_ref values.

6. Implement the non-`expression_ref` inverse preprocess path in narrow pieces:
   runtime untranslation of global registers, runtime trim unnormalization,
   runtime-to-Core deindexing, and then any Core graph/let unnormalization that
   is needed for display or diagnostics.

7. Keep graph/display conversion boundaries explicit. Do not add runtime
   `unlet` / `subst_reg_vars` clones unless the project first defines a true
   runtime-to-Core deindexing story; otherwise these names should continue to
   refer to Core/expression normalization inverses.

8. After each code batch, build `src/bali-phy/bali-phy` from
   `../build/gcc-16-debug-O`, run the relevant focused tests, and commit
   logically separate changes with `jj`.
