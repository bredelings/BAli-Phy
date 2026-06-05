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
- Removed `reg_heap::expression_at()` entirely. Display code now crosses through
  runtime-to-Core diagnostic deindexing at the display boundary instead of
  asking evaluation for an `expression_ref`.
- Reworked `context_ptr` internals so `operator[]`, `size()`, and `head()`
  inspect `Runtime::Exp` rather than `expression_at()` / `legacy_exp()`.
- Migrated a focused group of model/tree `context_ptr` scalar/object callers to
  runtime values and `set_code()`.
- Added `Runtime::RVector` conversions analogous to `EVector`, and migrated SMC
  haplotype-index lists to runtime vectors.
- Made `reg_heap::get_reg_value_in_context()` and
  `reg_heap::evaluate_program()` return closures directly. This removed the
  temporary `*_closure` duplicate names.
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
  `EVector`. The temporary `_code` duplicates for these APIs were removed.
- Added `TODO.md` to capture delayed cleanup work.
- Added the first non-`expression_ref` inverse-preprocess transforms:
  `Runtime::untranslate_vars(...)`, `deindexify(...)`, and
  `unprepare_for_translation(...)`. These reverse runtime global
  register translation, runtime trim normalization, and runtime de Bruijn
  indexing back to `Core::Exp<>` without going through `expression_ref`.
- Added runtime tests that start from Core, run the forward runtime preparation
  path, reverse back to Core, and then re-run the forward path to check
  normalized runtime equivalence. The tests also check the stage-level
  `deindexify` / `indexify` round trip and builtin operation
  recovery.
- Made `deindexify(...)` useful for diagnostic output from open runtime
  closures as well as Core-origin expressions. Direct runtime `RegRef(r)` is
  represented as the Core variable `<r>`, a free closure `IndexVar` resolved
  through `Env` is represented as `[r]`, malformed free indices are represented
  as `[?i]`, and runtime-only values such as `ObjectValue` become Core
  variables prefixed with `@`.
- Added `Core::RuntimeOnly` for runtime diagnostic terms that cannot honestly be
  represented as real Core variables. Runtime-only display values now remain
  visibly non-serializable instead of pretending to be user variables.
- Replaced `show_graph.cc`'s expression-ref display infrastructure with the
  runtime-to-Core inverse path. Graph labels now use `deindexify(...)`,
  `Core`-level `unlet(...)`, and diagnostic variable substitution.
- Removed `closure::legacy_exp()` and its expression cache. `closure::print()`
  now prints through `deindexify(...)`.
- Removed `param::ref(...)` and the expression-ref `param` constructor.
- Converted `data_partition` CLV/alignment accessors and a `TreeInterface`
  `IntMap` scan to consume runtime values directly.
- Converted simplifier constant folding to build and inspect `Runtime::Exp`
  constants directly.
- Added `Runtime::RMaybe` and migrated the small `EMaybe` builtin surface in
  `Prelude`, `Range`, and `Matrix`.
- Changed `Runtime::Exp(Object*)` construction to normalize boxed primitive
  objects (`String`, `Integer`, and `constructor`) into native runtime nodes
  without routing through `expression_ref`.
- Deleted the runtime expression-ref conversion shims:
  `Runtime::to_expression_ref(...)`,
  `Runtime::atomic_value(expression_ref)`, and
  `Runtime::e_op_value(expression_ref)`.
- Removed the remaining temporary `context_ptr::list_to_vector_code()` suffix;
  callers now use the undecorated runtime-returning `list_to_vector()`.
- Removed the dead expression-ref indexed-let compatibility layer:
  `indexed_let_expression(...)`, `parse_indexed_let_expression(...)`, the old
  expression-ref `deindexify(...)`, and expression-ref trim normalization
  helpers. Runtime-backed `indexify(...)` / `deindexify(...)` and runtime trim
  normalization remain.
- Removed the old `Let : Operation` marker and `type_constant::let2_type`.
  Runtime let reduction uses typed `Runtime::Let` and `let_op` directly.
- Deleted the unused `expression/runtime_views.H` header and stale includes
  that only supported the old expression-ref runtime-view layer.
- Moved runtime-facing Bool constructor names (`bool_true_name` and
  `bool_false_name`) out of `computation/expression` and into
  `computation/haskell/ids.*`. Runtime Bool checks now refer to those symbols
  instead of hard-coding `Data.Bool.True` / `Data.Bool.False`.
- Added `Runtime::has_constructor(...)` and converted evaluation-facing
  constructor checks in graph registration, Prelude IORef handling, JSON
  conversion, alignment Bool conversion, and `TreeInterface` to use the runtime
  helper directly.
- Removed stale expression includes from the main evaluator/operation files and
  from builtin implementation files that no longer construct expression
  `index_var`, `constructor`, Bool, list, or `expression_ref` values. Builtins
  now construct runtime closures with `Runtime::IndexVar` and
  `Runtime::Constructor` where applicable.
- Removed temporary `runtime_` disambiguators where the old expression-ref
  APIs no longer exist. The Core-to-runtime preprocess entry points are now
  `prepare_for_translation(...)`, `indexify(...)`, `deindexify(...)`, and
  `unprepare_for_translation(...)`; closure and operation-argument slot APIs
  now use `n_slots()`, `slot(...)`, `reg_for_slot(...)`, `n_args()`, and
  `reg_for_code(...)` directly.
- Moved `modifiable` and `interchangeable` operation definitions and runtime
  predicates from `computation/expression` to `computation/runtime`. Evaluator,
  graph, and builtin code no longer include expression headers just to access
  `is_modifiable(...)` or `is_interchangeable(...)`.

## Evaluation Core

The core reduction paths in `evaluate.cc` now inspect `Runtime::Exp` directly.
Evaluator exception formatting starts from `deindexify(...)` and then
uses Core-shaped display transforms such as `untranslate_vars(...)` and
`unlet(...)`. That is intentional for now: these functions are named as
inverses of Core / graph normalization passes, and the current implementations
operate on Core-shaped diagnostic syntax.

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

3. Graph display is no longer an `expression_ref` boundary for evaluated
   values. It deindexifies runtime closures into Core-shaped diagnostic syntax
   and then applies Core display transforms such as `unlet(...)` and
   `map_symbol_names(...)`.

4. `closure::legacy_exp()` and the runtime conversion shims
   `Runtime::to_expression_ref(...)`, `Runtime::atomic_value(expression_ref)`,
   and `Runtime::e_op_value(expression_ref)` are gone. If a future caller needs
   a bridge back to `expression_ref`, prefer a narrow local conversion with an
   explicit legacy name and a documented owner rather than recreating a general
   runtime API.

## Inverse Preprocess Pipeline

The forward Core-to-runtime preprocessing pipeline is:

1. `graph_normalize(...)`
2. `indexify(...)`
3. `Runtime::trim_normalize(...)`
4. `capture_local_reg_refs(...)` / `translate_refs(...)`

The reverse pipeline should run in the opposite order, without routing through
`expression_ref`:

1. `Runtime::untranslate_vars(...)`: untranslate runtime register references
   back to runtime names where the
   register is known as a global identifier;
2. `Runtime::trim_unnormalize(...)`;
3. `deindexify(...)`, converting de Bruijn indexed `Runtime::Exp` back
   to named `Core::Exp<>`;
4. graph/let unnormalization only where it is genuinely the inverse of a Core
   normalization pass.

`deindexify(...)` has two related uses. For runtime expressions that
actually came from Core preprocessing, stage-level tests should check round
trips such as `deindexify(...)` followed by `indexify(...)`,
and final tests should compare normalized meaning rather than raw Core syntax.
For open runtime closures, the same function produces Core-shaped diagnostic
syntax rather than closed, re-compilable Core. This mirrors the old
`expression_ref` display behavior, but uses synthetic `Core::Var` names instead
of embedding untyped `index_var` / `reg_var` nodes.

## Runtime Shim Audit

The removal-first pass found no remaining evaluator-side callers of
`closure::legacy_exp()`, `Runtime::to_expression_ref(...)`,
`Runtime::atomic_value(expression_ref)`, or
`Runtime::e_op_value(expression_ref)`. The remaining expression conversion
helper named `to_expression_ref(...)` is the Core-constant helper in
`computation/expression/convert.*`; it is not a runtime evaluation shim.

The remaining direct `expression_ref` appearances near evaluation are narrower:

1. `closure.H` keeps deleted constructors taking `expression_ref` as compile
   guards. This is a defensive API choice, not a runtime conversion path.

2. Model/tree construction still has expression-based entry points, especially
   `TreeInterface` initialization and model/code generation. These are
   pre-evaluation or model-generation boundaries.

3. `IntMap::restrictKeysToVector` still uses an internal `makeEVector`
   operation that materializes an `EVector`. This is a good candidate for a
   focused `RVector` migration, but changing it should be considered together
   with callers that expect the legacy vector representation.

The old expression-ref indexed-let marker is no longer present. Similar marker
cleanup candidates are now `Apply` as an operation object used to represent
runtime function application heads, expression `lambda2`, expression `Trim`,
and expression `constructor`.

## Caller Migration Hotspots

1. `context_ptr` is now runtime-based at the API level: `value()`, `head()`,
   `set_value(...)`, and `list_to_vector()` use `Runtime::Exp` /
   `Runtime::RVector`. The next cleanup here is naming and ownership, not
   replacing expression-returning duplicates.

2. Builtin data structures still expose evaluated values as legacy containers.
   The small `EMaybe` builtin surface has moved to `Runtime::RMaybe`.
   `IntMap::restrictKeysToVector` still materializes an `EVector` through an
   internal `makeEVector` operation. Larger candidates are `Vector.cc`,
   `Distribution.cc`, `PopGen.cc`, and SMC helper paths.

3. SMC haplotype/panel/site arrays still flow through `EVector` /
   `expression_ref` helper functions. Some call sites already use
   `Runtime::Exp` for scalar evaluation, but many sequence/read/haplotype
   helpers still accept `EVector`. These should move in focused groups after
   the smaller vector/maybe fronts establish the pattern.

4. Display/debug bridges remain explicit, but they are Core-shaped rather than
   expression-ref-shaped where runtime values are involved. `evaluate.cc`
   diagnostics, `closure::print()`, graph labels, compact graph display, and
   graph head formatting should continue to go through the runtime inverse
   preprocess path.

5. Model/code generation remains intentionally expression-based. Occurrences
   in `models/code-generation.*`, `models/logger.cc`, and parsed/model AST
   utilities are outside the evaluator cleanup unless a runtime-native consumer
   appears.

## Current Scan

Recent scan results:

- `context.H` has no textual `expression_ref` declarations and no temporary
  context `_code` APIs.
- Evaluator-core reduction code uses runtime expressions. Exception formatting
  intentionally crosses through runtime-to-Core diagnostic deindexing and
  Core-shaped display transforms.
- `param.H` / `param.cc` no longer exposes `param::ref()` or a
  `list_to_vector_code()` duplicate. It still uses `get_code` / `set_code`
  names for runtime values because those names describe stored closure code.
- `closure.H` no longer has `legacy_exp()` or an expression cache. It only
  mentions `expression_ref` in deleted constructors that block accidental
  conversion.
- `runtime/ast` no longer declares or defines expression-ref conversion shims.
- `show_graph.cc` is runtime-native for graph structure and uses
  runtime-to-Core diagnostic deindexing for labels.
- `let2_type` and the old `Let : Operation` object are gone.
- Expression-ref trim/indexed-let/deindexify helpers that depended on `let2`
  are gone. Runtime-backed trim/indexify/deindexify is the active path.
- Runtime constructors are split from expression constructors:
  `Runtime::Constructor`, `Runtime::ConstructorPattern`, and
  `Runtime::ConstructorApp` store `Runtime::ConstructorTag` name/arity data
  instead of the expression-side `constructor : Object`. Runtime constructor
  producers now pass names and arities directly, and runtime boolean checks use
  `ConstructorTag` overloads.
- Large `EVector` surfaces remain in likelihood/substitution and several
  builtins. These are evaluated-value containers, but they are broader than the
  evaluator API itself and should be migrated incrementally.
- Expression include pressure in evaluation-facing code is now narrower:
  `evaluate.cc`, `operations.cc`, `machine/args.cc`, and most builtin
  implementation files no longer include expression headers. The
  modifiable/interchangeable runtime operations have moved to `runtime/`. The
  remaining scanned non-predicate expression includes are `param.H`'s public
  `reg_var` constructor, `TreeInterface.cc`'s model/tree expression entry
  points, and `Runtime::GlobalVar` / graph heap use of expression `var`.

## Next Steps

1. Convert `IntMap::restrictKeysToVector` / `makeEVector` to return
   `Runtime::RVector`, with explicit legacy bridging only for callers that
   still require `EVector`.

2. Remove runtime leakage of old expression marker objects. First replace
   `param.cc::head_code()` returning `Apply()` for
   `Runtime::FunctionApply` with a runtime-native representation or remove the
   caller need for a synthetic head value. Then replace `evaluate.cc`'s static
   `Apply` object with direct use of `apply_op`.

3. Evaluate whether expression-side `constructor` can stop deriving from
   `Object`. This would block accidental constructor-as-object runtime
   conversions, but it is broader than the runtime split because
   `expression_ref` still represents parsed/legacy constructors as object heads.
   A viable change probably needs either a dedicated expression constructor node
   or a clearly named legacy wrapper.

4. Continue SMC vector migration in narrow groups: convert helper signatures
   from `EVector` to `Runtime::RVector` where the helper only needs scalar
   vector access.

5. Audit `TreeInterface` and `models/parameters` for remaining
   evaluation-time `expression_ref` use. Keep model-generation inputs as
   expression-based, but avoid converting evaluated tree/model properties back
   through expression syntax.

6. Continue the non-`expression_ref` inverse preprocess path in narrow pieces:
   runtime untranslation of global registers, runtime trim unnormalization, and
   runtime-to-Core deindexing are in place. The remaining work is Core
   graph/let unnormalization and then migrating diagnostics to use the
   Core-shaped runtime deindexify path where that improves the display
   boundary.

8. Keep graph/display conversion boundaries explicit. Do not add runtime
   `unlet` / `subst_reg_vars` clones unless there is a concrete runtime-stage
   consumer. For display, prefer the existing runtime-to-Core deindexing path
   followed by Core transforms.

9. After each code batch, build `src/bali-phy/bali-phy` from
   `../build/gcc-16-debug-O`, run the relevant focused tests, and commit
   logically separate changes with `jj`.
