# Distinguish recursive and non-recursive Core bindings

## Goal

Change Core and Runtime lets so that they explicitly distinguish a proven
non-recursive binding from a possibly recursive binding group.

This is a prerequisite for demand analysis and for eventually replacing a
demanded non-recursive let with a case.  It should also let graph preparation
avoid allocating recursive environments for administrative bindings that are
known to be non-recursive.

The central design rule is:

> A producer emits `NonRec` only when it knows that the binder is not in scope
> in the RHS.  Otherwise it emits a conservative `Rec`.  Occurrence analysis
> refines `Rec` groups using liveness and dependency information.

There is deliberately no general, separate `Decls -> Binds` classifier pass.

## Reviewed conclusion

An initial version of this plan proposed a pure dependency classifier that
would run independently of occurrence analysis.  Review against GHC and the
current bali-phy optimizer changed that design:

1. Loop breakers are not an input to classifying a group.  SCCs are formed
   first, and loop breakers are selected only within the surviving cyclic
   SCCs.
2. Liveness is part of the useful classification operation.  It determines
   which acyclic bindings disappear and whether a cyclic component is needed.
3. Scope dependencies can include more than the ordinary executable RHS.  In
   GHC they also include rules and stable unfoldings.  Bali-phy currently adds
   DFun-unfolding dependencies.
4. Running a second, pure SCC pass before occurrence analysis would duplicate
   work and could disagree with the richer scope graph used by occurrence
   analysis.
5. GHC's occurrence analyzer refines `Rec` to `NonRec` or smaller `Rec`
   groups, but does not repair an incorrectly generated `NonRec` by turning it
   into `Rec`.  Potentially recursive generated bindings must therefore start
   as `Rec`.

The implementation should follow this model.  A dependency sorter may still
exist for a particular producer, such as evidence desugaring, but it is not a
general Core phase or public `make_classified_lets` API.

## Relationship to GHC

GHC represents both top-level and local bindings as:

```haskell
data Bind b
    = NonRec b (Expr b)
    | Rec [(b, Expr b)]
```

Its desugarer uses source `RecFlag` information:

- a recursive source group becomes one conservative `Rec`;
- a non-recursive source group becomes one or more dependency-ordered
  `NonRec` bindings.

The occurrence analyzer then processes every `Rec` group by:

1. occurrence-analyzing the RHSs and associated metadata;
2. constructing scope-dependency edges;
3. computing SCCs;
4. emitting an acyclic singleton as `NonRec`;
5. retaining a cyclic SCC as `Rec`;
6. dropping dead components using body usage; and
7. selecting loop breakers inside retained cyclic SCCs.

GHC occasionally performs a specialized SCC sort at a construction boundary.
For example, desugared evidence bindings are sorted after their Core RHSs are
available.  This is producer-specific and does not replace occurrence
analysis.

Bali-phy does not yet preserve reliable `RecFlag` information through the
Haskell AST and desugarer.  The safe initial behavior is therefore to emit
`Rec` for unclassified desugared batches and let occurrence analysis split
them.  A later Haskell `Bind` refactor can allow the desugarer to emit more
`NonRec` bindings directly without changing the Core design.

## Current representation and behavior

Core currently has:

```c++
struct Decl
{
    Var x;
    Exp body;
};

struct Decls: std::vector<Decl> {};

struct Let
{
    Decls decls;
    Exp body;
};
```

Every `Core::Let` therefore has recursive scope:

- all binders are removed from the substitution before visiting any RHS;
- free-variable analysis removes all binders from the union of every RHS and
  the body;
- indexification pushes all binders before translating any RHS;
- the runtime allocates every register before constructing any RHS closure.

Occurrence analysis already does most of the difficult grouping work.  It
computes reachability, constructs a reference graph, finds SCCs, chooses loop
breakers for cyclic components, and returns a sequence of declaration groups.
The missing information is that each returned group is still represented as
plain `Decls`, so the distinction between an acyclic singleton and a recursive
component is lost.

Runtime `Let` similarly contains only `vector<Exp> binds`, and all runtime
utilities interpret every RHS and the body at a de Bruijn depth extended by
the complete binding group.

## Target Core representation

Add binding variants parameterized by the existing variable and expression
annotations:

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
using Bind = std::variant<
    NonRec<NoteV, NoteE>,
    Rec<NoteV, NoteE>
>;

template <typename NoteV, typename NoteE>
using Binds = std::vector<Bind<NoteV, NoteE>>;

template <typename NoteV, typename NoteE>
struct Let
{
    Bind<NoteV, NoteE> bind;
    Exp<NoteV, NoteE> body;
};
```

`Decls` remains useful as an unclassified declaration collection.  It should
not contain a recursion flag and should not imply a scope rule.

### Scope semantics

For:

```text
let x = rhs in body
```

represented by `NonRec`, `x` is not in scope in `rhs` and is in scope in
`body`.

For:

```text
letrec {x1 = rhs1; ...; xn = rhsn} in body
```

represented by `Rec`, every `xi` is in scope in every `rhsj` and in `body`.

### Invariants

1. `NonRec` contains exactly one declaration.
2. Its binder does not occur free in its RHS.
3. `Rec` is nonempty.
4. `Rec` is conservative: its members are not required always to form one
   minimal executable SCC.
5. Binders within a `Rec` are distinct.
6. An ordered `Binds` is in dependency order, from outer bindings to inner
   bindings.
7. Compiler transformations never turn an uncertain binding into `NonRec`.

Add a debug Core validator for these structural and scoping invariants.  Run
it after desugaring and after each optimizer pass while the implementation is
being stabilized.

### Constructors

Provide explicit constructors:

```c++
Exp make_nonrec_let(Decl decl, Exp body);
Exp make_rec_let(Decls decls, Exp body);
Exp make_lets(Binds binds, Exp body);
```

`make_rec_let` returns `body` for an empty `Decls`; otherwise it constructs one
conservative `Rec`.

Do not provide a public `make_classified_lets(Decls, body)`.  The old
`make_let(Decls, body)` may temporarily map explicitly to `make_rec_let` during
migration, but it should be removed so that callers must state what they know.

Core printers and dumps should use visibly different syntax, such as `let`
for `NonRec` and `letrec` for `Rec`.

## Producer rules

Audit every place that constructs a Core let and classify it according to how
the declarations were produced.

### Proven `NonRec`

Use `NonRec` when construction itself proves that the binder is fresh and its
RHS was built in the outer scope.  Expected examples include:

- a single administrative binding introduced for an application head;
- a binding introduced for one application or builtin argument;
- a fresh binding introduced for a case scrutinee;
- a beta-reduction or simplifier float whose RHS was already simplified in
  the outer environment; and
- other single fresh bindings produced after the final occurrence pass.

When several such bindings are produced, retain them as ordered `NonRec`
bindings rather than merging them into one `Rec` for convenience.

### Conservative `Rec`

Use `Rec` for:

- a desugared Haskell declaration batch whose source recursion information is
  not reliably available;
- a shared evidence `Decls` installed by `Core::WrapLet`;
- a generated declaration group that might acquire self-reference through a
  rule, unfolding, or later rewrite;
- an existing recursive binding preserved by a transformation; and
- any call site for which proving `NonRec` would require a new dependency
  analysis.

`Core::WrapLet` currently holds a shared declaration collection that may grow
after wrapper creation.  Keep storing the `Decls`, and install it as a
conservative `Rec` when the wrapper is applied.  Do not classify it when the
wrapper object is constructed.

### Optional producer-specific sorting

If retaining a large evidence group as `Rec` has a measurable cost before the
next occurrence pass, add an evidence-specific SCC sort after the evidence
RHSs are complete.  Such a helper should document all dependency sources it
uses and return `Binds`.  It must not become an implicit constructor used for
arbitrary `Decls`.

## Core operations

Every generic Core traversal must distinguish the two scope rules.

### Free variables

Implement:

```text
FV(NonRec x = rhs in body)
    = FV(rhs) union (FV(body) - {x})

FV(Rec {xi = rhsi} in body)
    = (FV(body) union union_i FV(rhsi)) - {xi}
```

Update both plain free-variable operations and annotated `FV::Exp`
construction.

### Substitution and renaming

For `NonRec`:

1. apply the incoming substitution to the RHS;
2. remove or rename the binder only for the body; and
3. never expose the new binder in the RHS in-scope set.

For `Rec`:

1. rename and bind every binder first;
2. remove every old binder from the substitution; and
3. use the recursive environment for every RHS and the body.

Apply the same distinction to symbol mapping, annotation conversion, Core
equality/order/serialization, pretty printing, inliner traversals, and any
utility that reconstructs a `Let`.

## Occurrence analysis owns `Rec` refinement

Refactor occurrence analysis to accept and return `Bind`/`Binds` rather than
using `Decls` as an implicit group marker.

### Existing `NonRec`

Occurrence analysis must not turn `NonRec` into `Rec`.

For a normal non-recursive binding:

1. analyze the body and obtain the binder's occurrence information;
2. drop the binding without analyzing its lazy RHS if the binder is dead and
   droppable;
3. otherwise analyze the RHS in the outer environment;
4. tag the binder with its occurrence information; and
5. return one `NonRec`.

In debug builds, assert that the binder is not free in the RHS.  A failure is
a producer or transformation bug, not something occurrence analysis should
silently repair.

### Existing `Rec`

Adapt the current `occurrence_analyze_decls` algorithm into an operation such
as:

```c++
vector<Occ::Bind>
occurrence_analyze_rec(const Module&, const Core::Rec&, free_var_set&);
```

Its responsibilities are:

1. Seed liveness from uses in the enclosing body and from exported binders.
2. Occurrence-analyze reachable RHSs.
3. Add every dependency required to keep the binding and its metadata in
   scope.  Preserve the current DFun-unfolding handling and audit whether any
   other attached metadata contributes free variables.
4. Compute SCCs of the live scope graph.
5. Drop unreachable components.
6. Emit an acyclic singleton without a self-edge as `NonRec`.
7. Emit a cyclic singleton or multi-node SCC as `Rec`.
8. Only for a cyclic SCC, construct the loop-breaker graph, select loop
   breakers, and order the declarations for simplification.

The exact order in which the implementation discovers liveness and computes
SCCs may continue to differ from GHC.  Bali-phy currently discovers the live
reachable subgraph before SCC analysis, avoiding work on dead RHSs.  Preserve
that optimization if tests establish the same scoping and dead-code behavior.

The important separation is logical rather than a separate compiler pass:

```text
RHS occurrence information and scope dependencies
        -> live graph
        -> SCC classification
        -> NonRec or Rec
        -> loop breakers within Rec
```

### Top-level bindings

Use `Core::Binds` as the optimizer's top-level internal representation, like
GHC's `CoreProgram`.  The current flat top-level `Decls` can enter optimization
as one conservative `Rec`.  Occurrence analysis then produces ordered
top-level `Bind`s.

The compiled-module boundary may flatten the final bindings temporarily,
because top-level runtime definitions are stored independently by resolved
global name.  Do not flatten between optimizer passes.

With optimization disabled, it is acceptable for uncertain groups to remain
conservative `Rec`; correctness must not depend on occurrence refinement.
Known producer-generated `NonRec` bindings should still retain their form.

## Simplifier

Replace the single `simplify_decls` path with explicit non-recursive and
recursive paths.

### Simplifying `NonRec`

1. Use occurrence information to decide whether to pre-inline.
2. If pre-inlining, record a suspended substitution containing the incoming
   environment; the binder is not in its own RHS environment.
3. Otherwise simplify the RHS with the incoming substitution and in-scope
   set.
4. Keep floats from that RHS outside the new binding.
5. Choose or rename the binder for the body only.
6. Consider post-inlining after simplifying the RHS.
7. If retained, return one `NonRec` and extend the body environment.

### Simplifying `Rec`

1. Rename and bind every binder before simplifying any RHS.
2. Simplify every RHS in the recursive environment.
3. Preserve loop breakers and recursive unfoldings.
4. Return a conservative `Rec`; allow the next occurrence pass to split it if
   simplification removed dependencies.

Change `SimplFloats` and `FloatLet` to carry `Bind`/`Binds`.  Do not merge
adjacent `NonRec` floats into a `Rec`.  This removes the current FIXME that
prevents the simplifier from splitting declaration groups because it cannot
tell whether they are recursive.

## Level setting and full laziness

Update set-levels and float-out so that:

- a `NonRec` binder is absent while assigning levels to its RHS and present
  in its body;
- every `Rec` binder is present while assigning levels to every RHS;
- floated bindings retain their `NonRec` or `Rec` form;
- dependency order between floated `NonRec` bindings is preserved;
- a transformation that cannot prove the resulting group non-recursive emits
  `Rec`; and
- optimizer passes continue to exchange `Binds`, not `Decls` groups whose
  meaning must be inferred.

Full laziness may preserve a conservative `Rec` even after moving or deleting
dependencies.  The following occurrence pass can refine it.

## Graph normalization

Graph normalization runs after ordinary optimization, so there may be no
later occurrence pass to repair its output.

It must therefore:

- preserve the `Bind` form of existing lets;
- traverse a `NonRec` RHS in the outer scope;
- traverse every `Rec` RHS in the recursive scope;
- represent fresh bindings introduced for heads, arguments, scrutinees, and
  builtin operands as ordered `NonRec` bindings when construction proves
  independence from their own binder; and
- use `Rec` for any collected batch whose dependencies are uncertain.

Prefer returning `Binds` from graph-normalization lifting helpers instead of
collecting fresh declarations into a `Decls` that is later installed as one
recursive group.

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

A runtime `NonRec` has one RHS.  Avoid adding a simultaneous multi-binding
non-recursive form unless a concrete runtime producer needs it; ordered Core
`NonRec` bindings can be represented by nested runtime lets.

### Indexification

For Core `NonRec`:

1. indexify the RHS using the old variable stack;
2. push the binder;
3. indexify the body; and
4. pop the binder.

For Core `Rec`:

1. push all binders;
2. indexify every RHS and the body using the extended stack; and
3. pop all binders.

Deindexification must be the exact inverse.

### Runtime traversals

Audit every traversal that currently adds `binds.size()` to its depth.  This
includes:

- free-index collection;
- shifting and remapping;
- trim normalization and unnormalization;
- local-register capture;
- global/register translation;
- runtime invariant checks;
- graph-register traversal;
- printing, comparison, and serialization.

For `NonRec`, traverse the RHS at `depth` and the body at `depth + 1`.  For
`Rec`, traverse every RHS and the body at `depth + rhss.size()`.

### Runtime execution

For runtime `NonRec`, `let_op` must:

1. construct the RHS closure using the old environment;
2. allocate a register;
3. store the RHS closure in that register;
4. append the register to the environment; and
5. continue with the body using the extended environment.

For `Rec`, retain the current placeholder-first behavior:

1. allocate all registers;
2. extend the environment with all of them;
3. construct every RHS closure in that recursive environment; and
4. continue with the body.

This distinction is the eventual foundation for translating a demanded
`NonRec` let to a case.  No corresponding transformation is valid for an
arbitrary `Rec`.

### Existing direct Runtime producers

During the representation migration, mark every existing direct
`Runtime::Let` constructor as `Rec`.  Their de Bruijn indices were constructed
for recursive scope, and merely changing the tag would change their meaning.

After runtime `NonRec` is tested, review them individually:

- `context_ref::perform_expression` can become `NonRec`, but its current
  `shift_free_indices(E, 1)` must be removed at the same time.  Add a focused
  equivalence and contingent-effect test.
- `Runtime::apply_env_function` currently shifts its function reference by
  the number of argument bindings and makes all argument expressions RHSs of
  one recursive let.  Leave it as explicit `Rec` initially.  Re-encoding it
  as nested `NonRec` bindings requires deliberate per-argument shifting and
  is separate work.

## Implementation sequence

Keep the representation changes separate from subsequent demand-analysis
work.  The intended landing sequence is:

### 1. Add Core binding types and validators

- Add `Core::NonRec`, `Core::Rec`, `Core::Bind`, and `Core::Binds`.
- Add explicit constructors and printing.
- Add the Core scoping validator and focused unit tests.
- Do not change `Core::Let` producers yet.

### 2. Convert Core and optimizer consumers conservatively

- Change `Core::Let` to contain `Bind`.
- Update free variables, substitution, conversions, serialization, printers,
  inliner traversals, simplifier, set-levels, float-out, and graph display.
- Mark every existing construction site `Rec` so this step preserves current
  semantics.
- Make optimizer top-level containers use `Binds`, while occurrence analysis
  may temporarily emit only `Rec`.

### 3. Add Runtime binding variants conservatively

- Change Runtime `Let` to contain Runtime `Bind`.
- Update every de Bruijn and register traversal.
- Update indexification/deindexification for both variants.
- Add the non-recursive `let_op` path.
- Mark every old direct Runtime let as `Rec`.
- Treat the Runtime serialization change as a deliberate cache-format break:
  delete the development cache instead of adding a permanent transition
  decoder or test.

### 4. Make occurrence analysis emit precise bindings

- Split the existing `occurrence_analyze_decls` entry point into `NonRec` and
  `Rec` handling.
- Preserve the current live-subgraph optimization where correct.
- Emit `NonRec` for live acyclic singletons and `Rec` for cyclic SCCs.
- Select loop breakers only after a component is known to be cyclic.
- Preserve `Bind` information through subsequent simplifier iterations.

### 5. Improve producer precision

- Replace ambiguous `make_let` calls with explicit `make_nonrec_let` or
  `make_rec_let`.
- Make known fresh administrative bindings `NonRec`.
- Keep Haskell declaration batches and shared evidence batches conservative
  until occurrence analysis.
- Make graph-normalization lift helpers return ordered `Binds`.
- Convert `perform_expression` only together with its de Bruijn adjustment and
  tests.

### 6. Remove migration bridges and review

- Remove any `Let(Decls, body)` compatibility constructor.
- Remove the ambiguous `make_let(Decls, body)` helper.
- Audit every Core and Runtime let construction site.
- Run debug validation after every optimizer pass.
- Inspect `--dump-ds` and `--dump-opt` output for both recursive and
  non-recursive examples.
- Review the final diff independently for scope errors before starting demand
  analysis.

## Tests

### Core representation and scoping

Test:

1. a valid `NonRec` whose RHS refers to an outer variable with the same source
   name but a different Core identity;
2. rejection of a `NonRec` whose RHS refers to its own binder;
3. singleton `Rec` self-recursion;
4. mutual recursion;
5. substitution into a `NonRec` RHS without deleting the outer reference;
6. substitution into a `Rec` RHS with all group binders protected; and
7. free-variable formulas for both forms.

### Occurrence analysis

Test:

1. conservative `Rec {x = 1}` becomes `NonRec x = 1` when live;
2. a self-edge remains `Rec`;
3. a mutual cycle remains one `Rec`;
4. an acyclic dependency chain becomes dependency-ordered `NonRec`s;
5. an unused acyclic binding is dropped;
6. an entirely dead recursive SCC is dropped;
7. a needed member retains every dependency in its recursive SCC;
8. deleting a dead declaration can expose a smaller live component;
9. DFun-unfolding dependencies remain alive and well-scoped; and
10. loop breakers are assigned only in cyclic `Rec` groups.

### Optimizer

Test:

1. pre- and post-inlining of `NonRec`;
2. recursive simplification with all binders in scope;
3. floats from a `NonRec` RHS stay outside its binder;
4. ordered `NonRec` floats are not accidentally combined into `Rec`;
5. full laziness preserves scoping; and
6. repeated occurrence/simplifier iterations retain explicit binding forms.

### Runtime

Test:

1. Core-to-Runtime indexification and deindexification round trips;
2. an outer-variable shadowing example that diverges or black-holes if a
   `NonRec` RHS is mistakenly treated as recursive;
3. self-recursive and mutually recursive runtime lets;
4. free-index collection, shifting, remapping, trimming, and register capture
   at the two different depths;
5. an unused lazy `NonRec` RHS is not evaluated;
6. serialization round trips for both variants; and
7. `perform_expression` retains its effect and contingent-lifetime behavior
   after conversion.

### Integration

Compile and run examples containing:

- ordinary local bindings;
- recursive functions and recursive data;
- mutual recursion;
- pattern bindings that generate multiple Core declarations;
- typeclass dictionaries and local evidence;
- `trcall` wrappers;
- optimized and unoptimized compilation; and
- graph-normalized applications with multiple administrative bindings.

## Build and performance checks

Build with no more than seven jobs and reduced scheduling priority:

```bash
nice -n10 ninja -C BUILD_DIR -j7
```

After correctness tests, compare the relevant baseline and candidate using:

```text
bali-phy 25-muscle.fasta -Inone --seed=0 --iter=300
```

Collect at least:

- task-clock;
- cycles;
- instructions;
- branches; and
- branch misses.

Use alternating baseline and candidate runs.  Treat instruction count as the
primary stable metric and task-clock as supporting evidence.  Investigate any
repeatable instruction-count regression before proceeding to demand analysis.

Also compare Core dumps for `sampleSequenceNative`.  This change alone is not
expected to perform demanded-let-to-case conversion, but the lets that a
future demand pass may convert must now be visibly and correctly `NonRec`.

## Acceptance criteria

The milestone is complete when:

1. Every Core and Runtime let explicitly contains `NonRec` or `Rec`.
2. No ambiguous `Let(Decls, body)` or `make_let(Decls, body)` API remains.
3. Unknown declaration batches are conservatively recursive.
4. Occurrence analysis refines live acyclic components to `NonRec` and assigns
   loop breakers only to cyclic `Rec` components.
5. `NonRec` scoping is correct in substitution, free-variable analysis,
   simplification, indexification, trimming, and runtime execution.
6. Known graph-normalization bindings reach Runtime as `NonRec`.
7. Recursive and unoptimized programs retain their previous behavior.
8. The compiled cache is cleanly regenerated after the Runtime archive change.
9. The standard tests and focused binding tests pass.
10. The benchmark has no repeatable instruction-count regression.

## Deliberate non-goals

This milestone does not implement:

- a general pure Core binding classifier;
- the Haskell `Decls`/`Bind` refactor;
- demand analysis;
- strict-let-to-case conversion;
- call-arity analysis or eta expansion;
- occurrence annotations stored permanently on `Core::Var`;
- a simultaneous multi-binding Runtime `NonRec`; or
- a permanent compatibility decoder for old Runtime AST caches.

Those changes can build on the explicit binding scopes established here.
