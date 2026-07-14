# Distinguish recursive and non-recursive Core bindings

## Goal

Represent binding scope explicitly in Core and Runtime so that a proven
non-recursive binding is distinguishable from a possibly recursive group.

This is a prerequisite for demand analysis and for eventually replacing a
demanded non-recursive let with a case.  It should also let graph preparation
avoid recursive-environment setup for administrative bindings that are known
to be non-recursive.

The central rule is:

> A producer emits `NonRec` only when construction proves that the binder is
> not in scope in its RHS.  Otherwise it emits a conservative `Rec`.
> Occurrence analysis may refine `Rec` groups using liveness and dependency
> information, but it never repairs an invalid `NonRec`.

There is no general `Decls -> Binds` classifier pass or public constructor
that guesses recursion.

## Review conclusions

Review of the first plan against the current code established the following
constraints.

1. `Core::NonRec` is not useful until `Core::Let` can represent it and every
   Core consumer implements its scope rule.  Constructors cannot land
   honestly before that conversion.
2. Converting Core while leaving Runtime in the old recursive-only form would
   require either rejecting `Core::NonRec` during indexification or adding a
   temporary de Bruijn compatibility encoding.  Neither is desirable.  The
   conservative Core and Runtime representation migrations should therefore
   be one atomic, behavior-preserving commit.
3. That representation commit changes serialized Core unfoldings and Runtime
   prepared code.  It must increment `compiled_module_cache_format`; deleting
   one development cache is not a format transition.
4. The current pre-optimization metadata dependency is
   `DFunUnfolding::free_vars`.  `CoreUnfolding`s for ordinary local bindings
   are installed by `export_small_decls` after optimization; builtin
   unfoldings are not members of the local declaration group.  Preserve DFun
   dependencies and document this lifecycle assumption.  Do not add a generic
   unfolding dependency API until another pre-optimization metadata form
   actually needs one.
5. A dead node cannot share an SCC with a live node under the current
   reachability algorithm: an edge from a live node makes the target live.
   Tests should cover dead disconnected SCCs and dead predecessors, not claim
   that deleting a dead node exposes a smaller live SCC.
6. Top-level `Binds` are needed before occurrence precision can survive
   optimizer iterations, but they do not need to be part of the initial
   representation conversion.  They belong in the occurrence-analysis
   commit.

## Design analysis

### Should binding scope be explicit?

Yes.  `Decls` currently serves both as an unclassified declaration collection
and as an implicit recursive binding group.  That loses information produced
by occurrence analysis and forces every later phase to assume recursive
scope.  A sum type makes the scope rule local and checkable and matches the
operations that consumers already need to perform differently.

The cost is a broad migration: all Core and Runtime traversals must inspect
the variant.  That cost is unavoidable if demand analysis is to rely on the
distinction.

### Why one binder in `NonRec`?

A singleton `NonRec` has one unambiguous scope transition: its RHS is in the
outer scope and its body is in the extended scope.  Several independent or
dependency-ordered bindings can be represented as nested `NonRec` lets.

A simultaneous multi-binding non-recursive form would require an additional
scope and ordering definition in both Core and Runtime without a current
producer that needs it.  Do not add one.

### Why retain `Decls`?

`Decls` remains useful for declaration batches whose recursion has not been
classified, including Haskell desugaring and shared evidence construction.
It has no scope semantics by itself.  A batch acquires semantics only when it
is placed in `Rec`.

### Constructor interface

Provide only constructors that state what the caller knows:

```c++
Exp make_nonrec_let(Decl decl, Exp body);
Exp make_rec_let(Decls decls, Exp body);
Exp make_lets(Binds binds, Exp body);
```

`make_rec_let` returns `body` for an empty batch; a constructed `Rec` is always
nonempty.  `make_lets` takes bindings in outer-to-inner dependency order and
nests them in reverse order around the body.

These helpers are justified because they centralize empty-group handling and
the otherwise error-prone nesting direction.  Do not provide
`make_classified_lets(Decls, body)` or retain `make_let(Decls, body)`: either
would hide a dependency analysis behind a construction API.

### Invariant checks

Do not add a general Core validator.  It would duplicate a scope-aware Core
traversal and either duplicate free-variable analysis or introduce an awkward
Core-to-optimizer dependency.

Assert the `NonRec` invariant locally where a transformation already has RHS
free-variable information, especially occurrence analysis.  Use constructor
assertions for inexpensive structural invariants such as nonempty `Rec`, and
use focused scope, substitution, and indexification tests for the remaining
coverage.  Reconsider a general validator only if localized checks prove
insufficient during implementation.

Every new function or lambda longer than three lines must have a one- or
two-line comment immediately before it explaining its purpose.

### Occurrence-analysis ownership

Occurrence analysis already computes liveness, dependencies, SCCs, and loop
breakers.  Making it emit `Bind` uses those existing results and avoids a
second SCC pass with a weaker dependency graph.

Loop breakers are selected only after an SCC has been classified as cyclic.
They are not an input to recursion classification.

### Conservative Runtime producers

Existing direct `Runtime::Let` producers were indexed for recursive scope and
must initially become explicit `Runtime::Rec` without changing their payloads.

`Runtime::apply_env_function` remains `Rec`.  Add a brief non-ideal note at
that site: its function reference and arguments are shifted for one recursive
group; converting it requires a deliberate nested-`NonRec` re-encoding and
removal of those shifts.

## Target Core representation

Add binding variants parameterized by the existing annotations:

```c++
template <typename NoteV, typename NoteE>
struct NonRec
{
    Decl<NoteV, NoteE> decl;
};

template <typename NoteV, typename NoteE>
struct Rec
{
    Decls<NoteV, NoteE> decls;
};

template <typename NoteV, typename NoteE>
using Bind = std::variant<NonRec<NoteV, NoteE>, Rec<NoteV, NoteE>>;

template <typename NoteV, typename NoteE>
using Binds = std::vector<Bind<NoteV, NoteE>>;

template <typename NoteV, typename NoteE>
struct Let
{
    Bind<NoteV, NoteE> bind;
    Exp<NoteV, NoteE> body;
};
```

Core dumps use `let` for `NonRec` and `letrec` for `Rec`.

### Core scope rules

For `NonRec x = rhs in body`, `x` is absent from the scope of `rhs` and
present in the scope of `body`.

For `Rec {x1 = rhs1; ...; xn = rhsn} in body`, every binder is in scope in
every RHS and in `body`.

### Core invariants

1. `NonRec` contains exactly one declaration.
2. Its binder does not occur free in its RHS.
3. `Rec` is nonempty.
4. Binders within a `Rec` are distinct.
5. `Rec` is conservative and need not be one minimal SCC.
6. `Binds` are ordered from outer bindings to inner bindings.
7. A transformation emits `NonRec` only when it preserves or proves the
   non-recursive scope rule.

### Core traversal rules

Free variables are:

```text
FV(NonRec x = rhs in body)
    = FV(rhs) union (FV(body) - {x})

FV(Rec {xi = rhsi} in body)
    = (FV(body) union union_i FV(rhsi)) - {xi}
```

For substitution and renaming:

- `NonRec`: transform the RHS with the incoming environment, then remove or
  rename the binder only for the body.
- `Rec`: rename and bind all binders first, remove all old binders from the
  substitution, and transform every RHS and the body in that recursive
  environment.

Apply the same distinction to annotation conversion, symbol mapping, Core
comparison and serialization, plain and annotated free-variable operations,
the occurrence analyzer, simplifier, set-levels, float-out, graph display,
module renaming, graph normalization, indexification, and every utility that
reconstructs a `Let`.

## Target Runtime representation

Mirror the distinction in Runtime:

```c++
struct NonRec
{
    Exp rhs;
};

struct Rec
{
    std::vector<Exp> rhss;
};

using Bind = std::variant<NonRec, Rec>;

struct Let
{
    Bind bind;
    Exp body;
};
```

### Runtime depth rules

For `NonRec`, traverse the RHS at `depth` and the body at `depth + 1`.

For `Rec`, traverse every RHS and the body at `depth + rhss.size()`.

Apply these rules in:

- Core-to-Runtime indexification and its exact deindexification inverse;
- free-index collection and shifting in `runtime/ast.cc`;
- trim variable collection, remapping, normalization, and unnormalization in
  `runtime/trim.cc`;
- local-register capture and reference translation in
  `machine/translate.cc`;
- graph-register traversal in `machine/graph_register.cc`; and
- any Runtime invariant check that interprets de Bruijn depth.

Shape-only traversals still need variant handling without changing depth.
These include printing, equality, serialization, `check_no_reg_refs`, Runtime
untranslation in `preprocess.cc`, and modifiability/interchangeability checks.

Delete the old `binds` field during the conversion and use compiler errors,
followed by `rg '\.binds|->binds' src/computation`, to find remaining users.
Do not retain a compatibility accessor.

### Runtime execution

For `Runtime::NonRec`, `let_op`:

1. creates the RHS closure using the old environment;
2. allocates one register;
3. stores the RHS closure in that register;
4. appends the register to the environment; and
5. continues with the body in the extended environment.

For `Runtime::Rec`, retain placeholder-first behavior:

1. allocate every register;
2. extend the environment with all registers;
3. create every RHS closure in the recursive environment; and
4. continue with the body.

## Occurrence analysis

### Existing `NonRec`

The enclosing-let traversal first analyzes the body and obtains its free
variables.  For one `NonRec`:

1. remove the binder from body usage and copy its occurrence information;
2. if the binder is non-exported and unused, drop the binding without
   analyzing its lazy RHS;
3. otherwise occurrence-analyze the RHS in the outer environment;
4. merge the RHS free variables into the enclosing free-variable set; and
5. emit one `NonRec` with the annotated binder.

There is no separate `droppable` predicate or helper: an unused local lazy
binding is not evaluated, while an exported binder is retained.

In debug builds, assert that the binder is not free in the analyzed RHS.  A
failure is a producer bug and must not be repaired by changing the binding to
`Rec`.

### Existing `Rec`

Replace `occurrence_analyze_decls` with a recursive-group operation returning
`Occ::Binds`:

```c++
Occ::Binds occurrence_analyze_rec(const Module&, const Core::Rec<>&,
                                  std::set<Occ::Var>& free_vars);
```

The operation:

1. seeds liveness from enclosing-body uses and exported binders;
2. occurrence-analyzes only reachable RHSs;
3. merges RHS references and `DFunUnfolding::free_vars` into the scope graph;
4. computes SCCs of the live graph;
5. emits an acyclic singleton without a self-edge as `NonRec`;
6. identifies cyclic singleton and multi-node SCCs as `Rec`;
7. selects loop breakers and simplification order only inside those cyclic
   SCCs; and
8. drops disconnected dead components.

Keep the current live-subgraph optimization.  Add a comment at the DFun
dependency lookup explaining that ordinary local `CoreUnfolding`s are
installed only after optimization; if that lifecycle changes, their free
variables must be added before SCC classification.

### Top-level optimizer representation

Use `Core::Binds` and `Occ::Binds` between optimizer passes.

`Module::optimize` remains a boundary from and to `Core::Decls` for now:

1. wrap the input batch in one conservative `Core::Rec`;
2. retain `Binds` through occurrence analysis, simplification, set-levels,
   and float-out; and
3. flatten once after the final optimizer pass, immediately before existing
   top-level renaming, unfolding export, and independent Runtime preparation.

With optimization disabled, the top-level batch can pass directly through as
the existing unclassified `Decls`.  Correctness must not depend on occurrence
refinement.

## Simplifier and full laziness

### Simplifying `NonRec`

1. Use body occurrence information to decide pre-inlining.
2. A pre-inline suspension captures the incoming substitution; it does not
   contain a mapping for the binder itself.
3. Otherwise simplify the RHS with the incoming substitution and in-scope
   set.
4. Keep floats from the RHS outside the retained binding and include those
   float binders in the environment used for the body.
5. Rename or bind the `NonRec` binder only after the RHS.
6. Apply post-inlining to the simplified RHS when allowed.
7. If retained, emit one `NonRec` and extend the body substitution and
   in-scope set.

### Simplifying `Rec`

1. Rename and bind every binder before simplifying any RHS.
2. Simplify every RHS in the recursive environment.
3. Preserve loop-breaker annotations and recursive unfoldings.
4. Return one conservative `Rec`; a later occurrence pass may split it if
   simplification removed dependencies.

`FloatLet` carries one `Bind`, and `SimplFloats` carries ordered `Binds`.
Appending floats must not merge adjacent `NonRec` bindings into `Rec`.

Set-levels and float-out preserve each variant.  A `NonRec` RHS is assigned a
level in the outer environment, then its binder is added for the body.  A
`Rec` group computes its group level and processes all RHSs in the recursive
environment.  Floated `NonRec` bindings retain dependency order.  A
transformation that cannot prove non-recursion emits `Rec`.

## Producer classifications

The initial representation commit converts every producer to explicit `Rec`
to preserve behavior.  Later commits improve only the following proven
producers.

### Remain `Rec`

- Haskell declaration batches emitted by `desugar.cc` because reliable source
  `RecFlag` information is not preserved through the current AST pipeline;
- shared evidence batches installed by `Core::WrapLet`, because the shared
  `Decls` may grow after wrapper construction;
- instance dictionary/evidence batches in `typecheck/instance.cc`;
- an existing `Rec` reconstructed by any optimizer or graph-normalization
  traversal; and
- any batch whose classification would require a new dependency analysis.

`WrapLet` continues storing `Decls` and wraps the final collection in `Rec`
when applied.  It must not classify a still-growing collection.

### Proven `NonRec`

- the fresh single bindings in `Core::unpack_cpp_string`, `Core::error`, and
  `Core::unsafePerformIO`, whose RHSs are built before the fresh binder enters
  scope;
- fresh single case-scrutinee and pattern-result bindings in
  `desugar/desugar-case.cc` where the RHS was constructed in the outer scope;
- fresh bindings introduced by set-levels for a maximal free expression;
- simplifier-generated beta and float bindings when the existing simplifier
  environment proves that the RHS was processed before the binder was added;
  and
- graph-normalization bindings for application heads, arguments, case
  scrutinees, constructor arguments, and builtin operands.

Do not convert a multi-declaration desugaring or evidence call site merely
because its current test input happens to be acyclic.

### Graph normalization

Change `graph_normalize_lift` to return ordered `Core::Binds` with one
`NonRec` per fresh administrative binder.  Concatenation preserves evaluation
scope order: bindings needed by an earlier normalized expression remain
outside bindings that may refer to them.

When graph normalization visits an existing let, preserve its variant and
normalize a `NonRec` RHS in the outer scope or all `Rec` RHSs in recursive
scope.  There is no occurrence pass after graph normalization, so it must not
discard known `NonRec` information.

### Direct Runtime producers

After end-to-end Runtime `NonRec` tests pass, convert
`context_ref::perform_expression` in a separate commit.  Construct one
`Runtime::NonRec` and remove its current `shift_free_indices(E, 1)` in the same
change.  Test both returned-value equivalence and contingent-effect lifetime.

Leave `Runtime::apply_env_function` as the documented conservative `Rec`.

## Commit sequence

Items 1-5 are separate `jj` commits, and each must build before the next item
is started.  Final verification is not a separate commit unless it finds a
defect that requires its own fix.  If implementation exposes a design
problem, stop after the last buildable commit and revise the remaining
specified sequence.

### 1. Represent binding scope explicitly without changing producer semantics

- Add Core and Runtime `NonRec`, `Rec`, and `Bind`; add Core `Binds`.
- Change both `Let` nodes to store `Bind`.
- Add explicit Core constructors, printers, comparison, and serialization.
- Update every Core and Runtime consumer with both scope rules.
- Convert every existing Core and Runtime construction site to explicit
  `Rec`, including graph normalization and direct Runtime producers.
- Update indexification, deindexification, and `let_op` for both variants.
- Increment `compiled_module_cache_format` once for the combined serialized
  Core and Runtime layout change.  Do not add a transition decoder.
- Do not add a feature-specific Core test executable.  Use standard Haskell
  and integration tests as producers begin emitting `NonRec`; keep any direct
  C++ probes in disposable `jj` changes.
- Extend `runtime/ast-serialization-test.cc` with Runtime depth, execution,
  and serialization tests for both variants.
- Delete the old fields and ambiguous `make_let`; do not add compatibility
  constructors or accessors.

This commit should have no intended generated-code change: all production
bindings remain recursive.

### 2. Preserve occurrence classification through the optimizer

- Add explicit `NonRec` and `Rec` handling to occurrence analysis.
- Make recursive-group SCC classification emit `Occ::Binds` and select loop
  breakers only for cyclic SCCs.
- Convert top-level occurrence, simplifier, set-levels, float-out, and float
  containers to ordered `Binds`.
- Wrap the incoming top-level `Decls` once and flatten only at the final
  `Module::optimize` boundary.
- Assert the `NonRec` invariant in occurrence analysis using the RHS free
  variables it already computes.
- Check liveness, SCC, DFun, simplifier, and float ordering through standard
  tests and compiler dumps; do not retain implementation-probing executables.

This is the first commit in which ordinary acyclic desugared bindings reach
Runtime as `NonRec`, so it exercises the representation introduced by commit
1 rather than creating parallel unused infrastructure.

### 3. Emit precise graph-normalization bindings

- Make graph-normalization lifting helpers return ordered `Binds`.
- Emit fresh administrative bindings as `NonRec`.
- Preserve existing let variants while recursively normalizing.
- Add a round-trip test showing that several administrative Core `NonRec`
  bindings remain `Runtime::NonRec` with correct de Bruijn indices.

### 4. Classify other proven Core producers

- Convert the listed fresh single-binding helpers and desugar-case sites to
  `make_nonrec_let`.
- Make set-levels and simplifier-generated fresh bindings use `NonRec` only at
  the points where their existing environment order proves the invariant.
- Keep Haskell batches, evidence, dictionaries, and uncertain generated
  groups as explicit `Rec`.
- Add a focused test for each newly converted producer category.

### 5. Convert `perform_expression`

- Replace its recursive single-binding Runtime let with `Runtime::NonRec`.
- Remove `shift_free_indices(E, 1)` in the same commit.
- Add returned-value and contingent-effect lifetime regression tests.

### Final validation and performance review

- Use `rg` to confirm there is no ambiguous `make_let`, old `Let::decls`, old
  Runtime `Let::binds`, or untagged Core/Runtime let construction.
- Inspect `--dump-ds` and `--dump-opt` for ordinary, self-recursive, mutually
  recursive, evidence-heavy, and graph-normalized examples.
- Review the final diff independently for scope-order errors.
- Run the complete focused and integration test set below.
- Compare baseline and candidate performance before beginning demand analysis.

## Tests

### Core scope and invariants

Test:

1. a `NonRec` whose RHS refers to an outer variable with the same source name
   but a different Core identity;
2. singleton self-recursive and mutually recursive `Rec` groups;
3. substitution into a `NonRec` RHS without removing an outer reference;
4. substitution into a `Rec` RHS with all group binders protected;
5. plain and annotated free-variable formulas for both variants;
6. occurrence analysis detects an invalid self-referential `NonRec` in debug
   builds; and
7. printing and serialization distinguish `let` from `letrec`.

### Occurrence analysis

Test:

1. live conservative `Rec {x = 1}` becomes `NonRec x = 1`;
2. a self-edge remains `Rec`;
3. a mutual cycle remains one `Rec`;
4. an acyclic dependency chain becomes dependency-ordered `NonRec`s;
5. an unused acyclic binding is dropped without analyzing its RHS;
6. a disconnected dead recursive SCC is dropped;
7. a dead predecessor that references a live binding is dropped without
   changing the live binding's classification;
8. every dependency of a live recursive SCC remains live;
9. a DFun dependency remains alive and well-scoped;
10. exported bindings remain live; and
11. loop breakers occur only in cyclic `Rec` groups.

### Optimizer

Test:

1. pre- and post-inlining of `NonRec`;
2. recursive simplification with all group binders in scope;
3. floats from a `NonRec` RHS remain outside its binder;
4. ordered `NonRec` floats are not combined into `Rec`;
5. full laziness preserves each scope rule and binding order; and
6. repeated occurrence/simplifier iterations preserve explicit forms.

### Runtime

Extend the Runtime AST test coverage for:

1. Core-to-Runtime indexification and deindexification round trips for both
   variants;
2. outer-variable shadowing that black-holes if a `NonRec` RHS is interpreted
   in recursive scope;
3. self-recursive and mutually recursive Runtime lets;
4. free-index collection, shifting, remapping, trimming, capture, and
   translation at both depths;
5. an unused lazy `NonRec` RHS not being evaluated;
6. printing, equality, and serialization round trips for both variants; and
7. `perform_expression` value and contingent-lifetime behavior.

### Haskell and integration coverage

Add focused cases under `tests/haskell/CoreNonRec/` where behavior is awkward
to construct in the C++ tests.  Use `NoImplicitPrelude` where possible.

Cover:

- ordinary local bindings and nested shadowing;
- recursive functions, recursive data, and mutual recursion;
- pattern bindings that generate several Core declarations;
- local evidence and typeclass dictionaries;
- `trcall` wrappers;
- optimized and unoptimized compilation; and
- graph-normalized applications with several administrative bindings.

Add each test with the commit that implements its behavior, so it does not
need `xfail`.  If a test is intentionally committed before its implementation,
mark it `xfail` until the implementing commit is folded into it.

## Build and verification

Use the out-of-source GCC 16 debug-optimized build throughout:

```bash
nice -n10 ninja -C ../build/gcc-16-debug-O -j7
```

Run the Runtime tests after commit 1, occurrence and optimizer checks after
commit 2, and standard producer-specific tests with commits 3-5.

Before declaring the work complete, run at least:

```bash
meson test -C ../build/gcc-16-debug-O \
  "bali-phy:runtime AST serialization" --print-errorlogs

meson test -C ../build/gcc-16-debug-O \
  "bali-phy:bali-phy 5d +A 50" --print-errorlogs
```

Also run `tests/haskell/CoreNonRec/` and the producer-specific integration
tests added by commits 3-5.

## Performance check

After correctness is complete, compare the parent baseline and final
candidate with alternating runs of:

```text
bali-phy 25-muscle.fasta -Inone --seed=0 --iter=300
```

Collect task-clock, cycles, instructions, branches, and branch misses.  Treat
instruction count as the primary stable metric and task-clock as supporting
evidence.  Investigate any repeatable instruction-count regression before
starting demand analysis.

Compare Core dumps for `sampleSequenceNative`.  This milestone does not
perform demanded-let-to-case conversion, but bindings eligible for that future
conversion must now be visibly and correctly `NonRec`.

## Acceptance criteria

The milestone is complete when:

1. Every Core and Runtime let explicitly contains `NonRec` or `Rec`.
2. No ambiguous `Let(Decls, body)`, `make_let(Decls, body)`, or compatibility
   accessor remains.
3. Unknown declaration batches are explicitly conservative `Rec` groups.
4. Occurrence analysis emits ordered `NonRec`s for live acyclic components,
   retains cyclic `Rec`s, and assigns loop breakers only inside cycles.
5. Optimizer passes exchange `Binds` and do not flatten between passes.
6. `NonRec` scope is correct in substitution, free-variable analysis,
   simplification, set-levels, float-out, indexification, trimming, capture,
   translation, and Runtime execution.
7. Known graph-normalization bindings reach Runtime as `NonRec`.
8. Recursive and unoptimized programs retain their previous behavior.
9. The compiled-module cache format is incremented and current artifacts
   regenerate without a compatibility decoder.
10. Focused tests and `bali-phy 5d +A 50` pass in
    `build/gcc-16-debug-O`.
11. The benchmark has no repeatable instruction-count regression.

## Deliberate non-goals

This milestone does not implement:

- a general pure Core binding classifier;
- a Haskell AST `Decls`/`Bind` refactor or reliable source `RecFlag`
  propagation;
- demand analysis or strict-let-to-case conversion;
- call-arity analysis or eta expansion;
- permanent occurrence annotations on `Core::Var`;
- simultaneous multi-binding Runtime `NonRec`;
- conversion of `Runtime::apply_env_function`; or
- a compatibility decoder for old Core or Runtime cache layouts.

Those changes can build on the explicit and tested scope distinction
established here.
