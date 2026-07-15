# Call arity and eta expansion

## Goal

Add enough arity information to Core to perform safe eta reduction, infer the
arity of non-recursive right-hand sides, and use whole-program call information
to eta-expand functions without duplicating work.

The optimization pipeline will be:

```text
main simplifier
call-arity analysis
simplifier (consume call arity, infer id arity, eta-expand)
```

The first implementation deliberately does not find recursive call-arity or
RHS-arity fixed points. Recursive groups are still traversed, but they receive
conservative metadata and are not eta-expanded beyond manifest lambdas.

Every new function or lambda longer than three lines must have a one- or
two-line comment immediately before it explaining what it does.

## Review conclusions

Review of the first-stage plan against bali-phy and GHC established these
constraints.

1. Source declaration arity and optimizer id arity are different properties.
   Leave `symbol_info::arity` unchanged. In particular, these definitions all
   have id arity two despite having declaration arities zero, one, and two:

   ```haskell
   f = \x y -> x + y
   f x = \y -> x + y
   f x y = x + y
   ```

2. GHC stores rich `IdInfo` on an `Id`, but explicitly projects only stable
   properties into an interface. Bali-phy should likewise put internal
   `id_info` on `Core::Var` while exporting only `id_arity` through
   `symbol_info`. Do not serialize transient call arity into compiled modules.
3. One-shot information describes a lambda binder, not a top-level function.
   Preserve it on lambda binders inside exported unfoldings; do not add a
   top-level one-shot field to `symbol_info`.
4. General occurrence counts are usage-dependent and remain transient. The
   current strong-loop-breaker behavior remains separate: bali-phy suppresses
   unfoldings for loop breakers instead of exporting occurrence information.
5. The simplifier already has the right binding-completion point for RHS arity,
   eta expansion, unfolding construction, and post-inlining. Call arity must be
   consumed there and cleared before the completed binding leaves the
   simplifier.
6. The current occurrence analyzer performs `\x -> f x` reduction without
   accounting for `seq`. That is unsound and must be removed before eta
   expansion relies on id arity.
7. The call-arity result requires both minimum observed call arity and a
   co-call graph. A per-variable `None/Once/Many` field is insufficient because
   recursion can cause another variable to be called repeatedly only when the
   two are co-called.
8. The existing Boost graph use is directed and SCC-oriented. Call arity needs
   a small private undirected graph with self-edges; adapting the SCC graph
   would expose the wrong operations.
9. No generic Core validator or new native test infrastructure is justified.
   Add focused assertions at existing ownership boundaries and use Haskell
   semantic tests for the new optimizer behavior.
10. Bali-phy Core has no value types or coercions. The first implementation
    therefore cannot apply GHC's `typeArity` cap. It remains safe by deriving
    ordinary id arity from Core structure and deriving extra call arity only
    from type-checked application sites. Revisit the cap if value types or
    coercions are added to Core.

## Design analysis

### Persistent and transient metadata

Introduce one internal record because arity, call arity, and lambda one-shot
information must survive Core rewrites together and must be copied independently
of pass-specific `NoteV` annotations:

```c++
namespace Core
{
enum class one_shot_info
{
    unknown,
    one_shot,
};

struct id_info
{
    int arity = 0;
    int call_arity = 0;
    one_shot_info one_shot = one_shot_info::unknown;
};
}
```

Add `Core::id_info id` to `Core::Var` alongside `NoteV info`. Variable identity,
ordering, and printed Core remain based on name and index; metadata does not
change identity.

This record is justified because all three properties belong to a binder and
must survive conversion between plain and occurrence-annotated Core. Keeping
them in separate maps would duplicate binder lifecycle logic in renaming,
substitution, occurrence analysis, and the simplifier.

Only `arity` and `one_shot` are serialized with Core variables. Deliberately
omit `call_arity` from `id_info::serialize`, so any deserialized value receives
the default zero. This makes the transient boundary structural rather than
depending only on an end-of-pipeline convention.

### Module interface projection

Add a distinct scalar to `symbol_info`:

```c++
int id_arity = 0;
```

Do not add the whole `Core::id_info` record. At module finalization, copy the
optimized top-level binder's `id.arity` into `symbol_info::id_arity`. Imported
arity lookup reads that field. Existing `symbol_info::arity` keeps its current
source- or symbol-category-specific meaning.

This direct scalar is preferable to introducing an `interface_id_info` wrapper
for one property. If strictness, CPR, or another stable id property is later
exported, group the interface properties at that point.

Lambda one-shot information crosses a module boundary only when the lambda is
inside a serialized `CoreUnfolding`; its binder already carries `Core::id_info`.
Call arity never crosses the boundary.

### Canonical local metadata

Copies of the same `Core::Var` may contain older metadata. Replace the
simplifier environment's anonymous pair with a named value:

```c++
struct bound_variable_info
{
    Unfolding unfolding;
    occurrence_info occurrence;
    Core::id_info id;
};
```

This is not a new unfolding store: it retains the existing local unfolding and
occurrence fields and adds the canonical local id metadata. Local references
resolve metadata from this environment; imported references resolve
`id_arity` from `symbol_info`; an unbound variable falls back conservatively to
the metadata carried on the occurrence.

Provide one simplifier lookup operation returning resolved `Core::id_info`.
It is justified because eta reduction, RHS arity, cheapness, and inlining all
need the same local/imported distinction. Do not create a parallel arity-only
environment in the simplifier.

While simplifying a `Rec`, bind every member with id arity zero for all RHSs.
Only install completed metadata after all RHSs are finished. This prevents an
old or partially inferred recursive arity from justifying the unsound rewrite
`f = \x -> f x` to `f = f`.

### RHS arity abstraction

Add `optimization/arity.H` and `optimization/arity.cc`. Use the reduced GHC
abstraction needed by the current Core language:

```c++
enum class arity_cost { cheap, expensive };

struct arity_lambda
{
    arity_cost cost;
    Core::one_shot_info one_shot;
};

struct arity_type
{
    std::vector<arity_lambda> lambdas;
};
```

Each entry describes the work before reaching one prospective lambda and the
one-shot property of that lambda. The safe arity is the longest prefix for
which accumulated work is cheap or the next lambda is one-shot. Thus:

```text
[cheap/unknown, expensive/unknown, cheap/unknown] -> arity 1
[cheap/unknown, expensive/one-shot, cheap/unknown] -> arity 2
```

Do not add GHC's divergence lattice, demand signatures, dictionaries-cheap
policy, casts, join points, or type-arity trimming. Bali-phy has no information
to implement those accurately yet.

The arity module also owns the expression-cheapness operation used by this
analysis, call arity, and the inliner. This avoids leaving the current parallel
`is_work_free` implementation in `inliner.cc`.

### Eta expansion

Eta expansion is performed only while completing a binding in the simplifier,
after its RHS and retained floats are finalized but before constructing its
unfolding or considering post-inlining.

For a target arity greater than manifest arity:

1. Preserve the existing leading lambdas and their binder metadata.
2. Create fresh binders for the missing suffix.
3. Annotate each new binder with the corresponding one-shot information from
   the safe arity type.
4. Apply the old body to the new binders in order.
5. Rebuild the complete lambda prefix.

Only eta-expand `Lambda`, `Let`, and `Case` RHSs initially. Do not expand a
bare variable, constant, constructor application, builtin operation, or
partial application; those wrappers tend to eta-reduce immediately and add no
current optimization opportunity.

Call arity is combined with the RHS arity type in the GHC style: it may mark
later prospective lambdas as safe after the function has received its first
argument, and it may extend a nonempty arity type with expensive one-shot
entries. It cannot turn an empty arity type into evidence that an arbitrary
value is a function.

After choosing the target, set the binder's persistent id arity to that target
and set its call arity to zero. For a recursive binder, use only its manifest
arity and still clear call arity.

### Eta reduction

Occurrence analysis should annotate and classify; it should not perform the
current eta reduction.

The first sound simplifier rule handles only:

```text
\x1 ... xn -> f x1 ... xn  ==>  f
```

Require all of the following:

- `f` is a variable;
- the binders are distinct;
- the arguments are exactly those binders in order;
- none of the binders occurs in `f`;
- resolved id arity of `f` is at least `n`; and
- recursive-group binders resolve to arity zero while their RHSs are being
  simplified.

Do not initially reduce partial-application heads such as
`\x -> f a x`, float lets through the lambda, or use a speculative
"known evaluated" exception. These can be added after the basic rule has
semantic tests involving `seq`.

### Call-arity result and co-call graph

Add `optimization/call-arity.H` and `optimization/call-arity.cc` with one
public entry point over `Core::Binds`. Keep the analysis result and graph
private to the implementation:

```c++
struct call_arity_result
{
    co_call_graph co_calls;
    std::map<Core::Var<>, int> calls;
};
```

The call map contains the minimum number of arguments at every observed call.
Absence means not called. The graph is undirected:

- an edge `{x,y}` means the two may both be called in one execution;
- a self-edge `{x,x}` means `x` may be called more than once;
- lack of an edge is useful information;
- adding edges or lowering an arity is always conservative.

Implement only the operations required by the equations below: union, remove
nodes, neighbors, self-edge query, complete graph, and complete bipartite
edges. Use `std::map<Var,std::set<Var>>`; do not expose a reusable graph API.

All let-bound and top-level variables are interesting in the first version.
Lambda and pattern binders are removed from the interesting set. This is more
work than GHC's type-arity filter, but Core currently lacks the types needed to
apply that filter honestly.

### Call-arity equations

Analyze an expression with an incoming arity: the number of arguments supplied
by its surrounding context.

- `Var x`: record `x` at the incoming arity when it is interesting.
- `Lambda x body`, incoming arity greater than zero: analyze `body` with one
  fewer argument and remove `x`.
- `Lambda x body`, incoming arity zero: analyze the body at zero, remove `x`,
  then replace its co-call graph by a complete graph because the closure may be
  entered repeatedly.
- `Apply f a`: analyze `f` with one additional argument and `a` at zero; union
  the results and add complete bipartite edges between their domains. For a
  trivial argument, conservatively make its result multiply called, matching
  the absence of a separately shared argument thunk during preparation.
- `Case scrut alts`: union alternative results without cross-edges between
  alternatives, because only one alternative executes. Then combine the
  scrutinee result sequentially with the alternatives by adding cross-edges.
- `ConApp`: analyze fields at zero and conservatively combine them as possibly
  co-called; constructor creation itself contributes no call.
- `BuiltinOp`: analyze operands at zero and combine them sequentially because
  the operation may evaluate more than one operand.
- `Constant`: return an empty result.

Combining alternative paths unions graphs and takes the pointwise minimum of
the call maps. Combining sequential expressions additionally adds complete
bipartite edges between their call-map domains. Replacing a result with
"multiply called" makes its graph complete, including self-edges.

For `NonRec v = rhs` followed by `body`:

1. Analyze `body` with `v` interesting.
2. Read `v`'s minimum arity and self-edge from the body result. If absent, use
   arity zero and "not known once".
3. Set `safe_call_arity` to the observed arity when the RHS is cheap or `v` is
   called at most once; otherwise set it to zero.
4. Analyze `rhs` under `safe_call_arity` in the outer interesting-variable
   environment.
5. If `v` may be called repeatedly and `safe_call_arity` is nonzero, make the
   RHS result multiply called. A thunk retained at arity zero remains shared
   and its RHS is evaluated at most once.
6. Add cross-edges between variables called by the RHS and variables co-called
   with `v` in the body.
7. Remove `v` from the outward result and write `safe_call_arity` to its binder.

For `Rec`, set every binder's call arity to zero, analyze each RHS at incoming
arity zero, combine the body and RHS domains into a complete co-call graph,
and remove the group binders from the outward result. This is intentionally
conservative and contains no fixed point.

At top level, process bindings from last to first as nested lets. Seed the
final body with artificial arity-zero, multiply-called uses of every exported
binder. This prevents call-site assumptions about calls from other modules
while allowing unexported top-level non-recursive functions to benefit.

### Pipeline placement

Install call arity at the existing placeholder in `Module::optimize` after the
main simplifier and before demand analysis:

```c++
core_binds = call_arity_analyze(*this, core_binds);
core_binds = simplify_module(opts, fvstate, *this, core_binds);
```

The immediate simplifier is part of the same pipeline change. Do not leave
call-arity annotations for later passes, and do not export them.

## Commit sequence

Each numbered item is one `jj` commit. Create an empty child for the work, keep
the commits separate while testing, and squash a corrective child into its
introducing commit only after the series is complete.

### 1. Add internal id metadata and exported id arity

1. Add `Core::one_shot_info` and `Core::id_info` in a small Core header.
2. Add the non-template `id` field to `Core::Var`; update constructors and
   serialization while leaving equality, ordering, hashing, and printing
   unchanged.
3. Preserve `id` through occurrence annotation/removal, renaming,
   substitution, set-levels, float-out, fresh-name copying, patterns, and every
   Core reconstruction path. Fresh variables start with default metadata.
4. Replace `bound_variable_info` with the named structure retaining unfolding
   and occurrence fields and adding canonical `id` metadata. Update bind and
   rebind operations without changing inlining behavior.
5. Add `symbol_info::id_arity`, serialize it, and increment
   `compiled_module_cache_format` from 3 to 4. Do not modify
   `symbol_info::arity`.
6. In `export_small_decls`, copy each final top-level binder's `id.arity` to
   its symbol. Keep loop-breaker unfolding suppression unchanged.
7. Initialize id arity independently for synthesized values whose Core
   definitions prove it, including `seq`, `coerce`, and constructor wrappers.
   Do not infer variable id arity from the existing `symbol_info::arity` field.
Verification for this commit:

- build `gcc-16-debug-O`;
- run `bali-phy:computation`;
- run `bali-phy:compiled module cache`; and
- run the corresponding Windows computation and cache tests when the existing
  MinGW build is configured.

### 2. Make eta reduction respect id arity

1. Delete `maybe_eta_reduce` and its call from occurrence analysis, leaving
   occurrence analysis behavior-preserving apart from removing the unsound
   rewrite.
2. Add the exact-variable eta-reduction rule to simplifier lambda rebuilding.
   Resolve the head variable's arity through the simplifier environment.
3. During recursive RHS simplification, expose group members with arity zero
   so the rule cannot collapse a recursive function to itself.
4. Add a `NoImplicitPrelude` Haskell regression using `seq` that distinguishes
   an eta-expanded function from an arbitrary possibly-bottoming expression.
Verification for this commit:

- build `gcc-16-debug-O`;
- run `bali-phy:computation`;
- run the new Haskell regression; and
- run `bali-phy:bali-phy 5d +A 50`.

### 3. Infer non-recursive RHS arity and eta-expand in the simplifier

1. Add the reduced arity-type implementation and private transient signature
   environment to `optimization/arity.H` and `.cc`.
2. Implement expression rules for variables, lambdas, applications,
   non-recursive lets, cases, constructors, builtins, and constants. Case
   alternatives use their common safe prefix. Recursive references use only
   already established id arity; there is no recursive fixed point.
3. Implement safe-prefix trimming and call-arity combination. An empty arity
   type remains empty; call arity may only extend a nonempty type.
4. Implement eta expansion with fresh suffix binders and one-shot annotations.
   Keep the function private to the arity/simplifier boundary rather than
   adding a general Core construction API.
5. In `simplify_nonrec`, after finalizing RHS floats, compute its arity type,
   combine the binder's call arity, eta-expand eligible RHS forms, set final id
   arity, and clear call arity. Construct unfoldings and run post-inline only
   after this step.
6. In `simplify_rec_decls`, record manifest lambda arity only, perform no eta
   expansion, clear call arity, and install all completed group metadata after
   every RHS has been simplified.
7. Ensure exported unfoldings contain id arity for nested let binders and
   one-shot information for lambda binders, while top-level export still uses
   `symbol_info::id_arity`.
8. Add `NoImplicitPrelude` semantic tests covering eta expansion through a
   non-recursive let and case, including `seq` observations that would expose
   duplicated work or changed bottom behavior.

Verification for this commit:

- build `gcc-16-debug-O`;
- run `bali-phy:computation`;
- run the arity Haskell tests;
- run `bali-phy:compiled module cache`; and
- run `bali-phy:bali-phy 5d +A 50`.

### 4. Make cheapness and unfolding guidance consume id arity

1. Move the existing expression work-free logic from `inliner.cc` into the
   arity module as the single cheapness implementation used by arity analysis,
   call arity, and inlining.
2. Treat an application as cheap only when resolved id arity proves it is an
   unsaturated function application and all required subexpressions satisfy
   the cheapness rule. Unknown arity remains conservative.
3. Replace all existing `is_work_free` calls immediately; do not retain the old
   implementation as a compatibility path.
4. Generate unfolding guidance after eta expansion. Use the completed binder
   arity for saturation decisions while retaining actual leading lambda
   binders for argument-discount accounting.
5. Keep existing size thresholds and occurrence-based inlining policy
   unchanged. This commit changes only facts made available by id arity.
Verification for this commit:

- build `gcc-16-debug-O`;
- run `bali-phy:computation`;
- run the optimizer Haskell tests; and
- run `bali-phy:bali-phy 5d +A 50`.

### 5. Add non-recursive call arity and activate the pipeline

1. Add the private co-call graph, result operations, expression traversal, and
   `NonRec` transfer function described above.
2. Implement conservative `Rec` traversal with zero call arities and a complete
   outward co-call graph; do not add iteration or SCC-specific behavior.
3. Implement top-level right-to-left traversal seeded by external uses of
   exported binders.
4. Annotate binding occurrences with call arity while preserving all other id
   metadata.
5. Install the pass at `Module::optimize`'s existing call-arity placeholder and
   invoke the consuming simplifier immediately afterward in the same commit.
6. Add a small `NoImplicitPrelude` integration test based on a function whose
   second lambda is hidden behind work, checking semantics for both saturated
   and shared partial calls. Do not retain exact `--dump-opt` output as a test
   fixture; use dumps only as temporary implementation diagnostics.

Verification for this commit:

- build `gcc-16-debug-O`;
- run `bali-phy:computation`;
- run all new arity Haskell tests;
- run `bali-phy:compiled module cache`;
- run `bali-phy:bali-phy 5d +A 50`; and
- run the Windows computation, compiled-module-cache, and new Haskell tests
  under the configured MinGW/Wine build.

## Final series verification

From `/home/bredelings/Devel/bali-phy/jj`, use the existing out-of-source build:

```sh
ninja -C ../build/gcc-16-debug-O
meson test -C ../build/gcc-16-debug-O 'bali-phy:computation' --print-errorlogs
meson test -C ../build/gcc-16-debug-O 'bali-phy:compiled module cache' --print-errorlogs
meson test -C ../build/gcc-16-debug-O 'bali-phy:bali-phy 5d +A 50' --print-errorlogs
```

Run the named Haskell arity directories directly with `tests/run-tests.py` as
they are added. Do not create a standalone optimizer executable. Before
squashing any corrective children, inspect `jj diff` for accidental changes to
existing untracked files and inspect the five commit descriptions to ensure
that each states one logical change.

## Deferred work

- Recursive RHS-arity fixed points.
- Recursive call-arity fixed points and precise recursive co-call propagation.
- Demand/cardinality information combined with call arity.
- Divergence and bottom signatures.
- Typed Core arity caps, coercions, dictionaries, and join points.
- Eta reduction of partial-application heads or through lets/cases.
- Explicit export of strong-loop-breaker occurrence information instead of
  encoding it by suppressing an unfolding.
