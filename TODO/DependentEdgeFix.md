# Dependent edge contingency fix

## Goal

Replace the execution-order-based `evaluate_reg_dependent_{use,force}` API
with an API that explicitly says whether a USE or FORCE edge is contingent on
earlier USE results.  Then migrate every existing caller and remove the old
API.

The immediate performance problem is that `IntMap.toVector` currently records
the first changeable entry as a fixed USE and every later entry as a dependent
USE.  The later entry registers do not depend on the first entry value; they
depend only on the IntMap value.  When that IntMap is fixed, all entry edges
should be fixed even when the entries themselves are changeable.

## Semantics

### Register and edge contingency

A **contingent reg** is a reg whose existence depends on a step.  The machine
records this using `created_by_step`, and `reg_is_contingent()` tests it.

An **edge-contingent USE or FORCE** is an edge whose owning operation would
have been a contingent reg if the fused native traversal had instead been
expressed using ordinary operations and cases.  The fused operation has
removed that intermediate reg, so the edge is recorded on the enclosing step
instead of on the enclosing reg.

For example, the unfused form of a list traversal is conceptually:

```haskell
case xs of
  x : xs' ->
    useOrForce x
    -- continue with xs'
```

If the case USE of `xs` is changeable, operations allocated in the selected
alternative are contingent regs.  Their element and tail edges may be ordinary
fixed edges, but those edges disappear when their owning regs disappear.  A
fused native traversal represents the same lifetime by putting those edges on
the changeable case step.

```text
unfused: contingent operation reg owns a fixed edge
fused:   enclosing changeable step owns an edge-contingent edge
```

Edge contingency is relative to the current real `OperationArgs` boundary.
Even if the current operation reg is contingent, its ordinary edges start as
fixed: that real reg already gives the edges the correct lifetime.  Only
operation boundaries eliminated inside the native operation contribute
`EdgeContingency::contingent`.

### Propagation

A USE accepts an input edge contingency and can return an output edge
contingency:

```text
c_out = c_in OR observed USE result can change
```

The changeability test must use the resolved dependency reg used for USE
bookkeeping, not merely the returned WHNF `value_reg`.

A FORCE accepts `c_in` but produces no `c_out`.  It does not inspect a value to
select a continuation.  A pure constant may require no physical edge, but its
logical edge contingency is still inherited by computations selected from its
parent USE.

Callers combine the output contingencies of every USE whose result determines
either:

* the target register of a later USE or FORCE; or
* whether that later USE or FORCE occurs on a normal successful path.

Mere execution order does not create a dependency.  In particular:

* IntMap entries are siblings controlled by the IntMap value.
* A list element and the next spine observation are siblings controlled by the
  current cons observation.
* A loop bound contributes to edges in iterations whose existence it controls.
* A representation case contributes to operations selected by its successful
  alternatives.
* A validation branch that only throws does not contribute to later edges,
  because it produces no successful retained step.

For a target computed from several values, combine their contingencies with
`operator|`.  No parent graph is required while the machine has only two edge
owners: the operation reg and its result step.

## Final API

The strong enum prevents a raw boolean from being confused with a register or
with some unrelated flag.

```cpp
enum class EdgeContingency : unsigned char
{
    fixed,
    contingent
};

constexpr EdgeContingency operator|(EdgeContingency,
                                    EdgeContingency);

struct UseWithEdgeContingency
{
    int value_reg;
    EdgeContingency edge_contingency;
};

struct ValueWithEdgeContingency
{
    Runtime::Exp value;
    EdgeContingency edge_contingency;
};
```

The ordinary source API remains unchanged, but fixed USE bookkeeping has one
virtual implementation that also returns `c_out`.

```cpp
class OperationArgs
{
public:
    virtual int evaluate_reg_force(int reg) = 0;
    int evaluate_reg_use(int reg);

    int evaluate_reg_force(int reg, EdgeContingency c_in);
    int evaluate_reg_use(int reg, EdgeContingency c_in);

    UseWithEdgeContingency
    evaluate_reg_use_with_edge_contingency(int reg);

    UseWithEdgeContingency
    evaluate_reg_use_with_edge_contingency(int reg,
                                           EdgeContingency c_in);

    UseWithEdgeContingency
    evaluate_slot_use_with_edge_contingency(int slot);

    ValueWithEdgeContingency
    evaluate_slot_to_value_with_edge_contingency(int slot);

protected:
    virtual int evaluate_reg_contingent_force(int reg) = 0;
    virtual int evaluate_reg_contingent_use(int reg) = 0;

    virtual UseWithEdgeContingency
    evaluate_fixed_reg_use_with_edge_contingency(int reg) = 0;
};
```

This is the endpoint API, not an additional virtual interface to install beside
the legacy one.  In the first commit, the new non-virtual overloads dispatch a
contingent input to the existing
`evaluate_reg_dependent_{use,force}` virtuals.  The final cleanup commit renames
that one virtual pair to `evaluate_reg_contingent_{use,force}` and makes it
protected.  At no point do both virtual pairs coexist.

The non-virtual overloads have the following behavior:

```text
evaluate_reg_use(reg)
    -> evaluate_fixed_reg_use_with_edge_contingency(reg).value_reg

evaluate_reg_use(reg, fixed)
    -> evaluate_fixed_reg_use_with_edge_contingency(reg).value_reg

evaluate_reg_use(reg, contingent)
    -> evaluate_reg_contingent_use(reg)

evaluate_reg_force(reg, fixed)
    -> evaluate_reg_force(reg)

evaluate_reg_force(reg, contingent)
    -> evaluate_reg_contingent_force(reg)

evaluate_reg_use_with_edge_contingency(reg, contingent)
    -> {evaluate_reg_contingent_use(reg), contingent}

evaluate_reg_use_with_edge_contingency(reg, fixed)
    -> evaluate_fixed_reg_use_with_edge_contingency(reg)
```

The base overloads dispatch only on `c_in`; each concrete interpreter enforces
its own contingent-edge invariant.

`evaluate_slot_use_with_edge_contingency()` starts with fixed input
contingency because an operation slot has a statically known target relative to
the current operation.  The value helper uses the same rule; a literal slot
that has no register returns `EdgeContingency::fixed`.

The common calls used by operations such as `x + y` remain unchanged:

```cpp
int x = Args.evaluate_slot_use(0);
int y = Args.evaluate_slot_use(1);
```

An IntMap traversal requests `c_out` only for the map, then supplies that value
as `c_in` for entry USEs:

```cpp
auto map_arg = Args.evaluate_slot_to_value_with_edge_contingency(0);
const auto& map = map_arg.value.as_<IntMap>();

for (auto [_, reg]: map)
{
    int value_reg = Args.evaluate_reg_use(reg,
                                          map_arg.edge_contingency);
    // The entry c_out does not control sibling entries.
}
```

A list traversal carries `c_out` only along structural observations:

```cpp
auto xs = Args.evaluate_slot_use_with_edge_contingency(0);

while (is_cons(xs.value_reg))
{
    Args.evaluate_reg_force(element_reg, xs.edge_contingency);
    xs = Args.evaluate_reg_use_with_edge_contingency(
        tail_reg, xs.edge_contingency);
}
```

This single fixed-USE virtual avoids parallel bookkeeping and private workers.
Unevaluated interpreters already test `dep_reg`; the unchangeable interpreter
returns fixed; only already-changeable interpreters add a changeability test.
`UseWithEdgeContingency` is small, but the release benchmark must still detect
whether this simpler interface measurably slows ordinary USEs before any split
hot path is reconsidered.

## Specified implementation plan

The implementation is a sequence of buildable `jj` commits rooted at
`master`.  The temporary old API remains only long enough to migrate callers;
it is marked as migration-only and removed at the end.

### 1. Add the edge-contingency API

Commit: `Add edge contingency to operation arguments`

Modify `src/computation/machine/args.H` and `args.cc` to add the enum, named
result types, `operator|`, overloads, slot/value methods, and protected tracked
fixed-USE virtual.  Replace the virtual one-argument USE with the non-virtual
wrapper shown above.  `operator|` returns contingent if either operand is
contingent; do not add generic wrappers.

Retain `evaluate_reg_dependent_{use,force}` as the sole migration-only
contingent virtual pair and mark it with a `NOTE`; do not add the final renamed
pair yet.  A fixed USE calls the tracked fixed virtual once and either returns
or discards its `c_out`.  A contingent USE/FORCE calls the corresponding legacy
virtual; a tracked contingent USE returns contingent without another test.

Implement `evaluate_slot_use_with_edge_contingency()` by applying the tracked
fixed-register USE to `reg_for_slot(slot)`.  Implement the tracked value helper
by following the control flow of `evaluate_slot_to_value()` rather than calling
it: for a register slot, perform one tracked fixed USE, obtain the closure from
the returned `value_reg`, preserve the unchangeable-result assertion from
`evaluate_reg_to_closure()` and the existing debug rejection of a lambda as an
object, and return its code with the USE contingency.  For a literal slot,
perform the same debug lambda check and return the literal with fixed
contingency.  Thus neither helper evaluates a register twice.

Implement that tracked fixed virtual directly in all five interpreters:

* The two unevaluated interpreters perform their existing fixed-USE evaluation,
  edge recording, `make_changeable()`, and evaluator-2 count adjustments once,
  then return the `value_reg` and
  `M.reg_is_to_changeable(dep_reg)` from the same `EvalResult`.
* `RegOperationArgs1Changeable` calls `incremental_evaluate1()` once, returns
  its `value_reg`, and tests its `dep_reg` without recording another edge.
* `RegOperationArgs2Changeable` uses
  `dep_reg = M.follow_reg_ref_no_force(reg)`, preserves the ordinary validity
  assertion, returns `M.value_for_reg(dep_reg)`, and tests that `dep_reg`; it
  neither calls `incremental_evaluate2()` nor changes edges or counts.
* `RegOperationArgsUnchangeable` calls its existing `evaluate_reg()` once and
  returns fixed.

Do not add private workers.  Keep `used_changeable` and the legacy fallback for
unmigrated callers until the final cleanup commit.  Explicitly contingent calls
are valid only after an earlier USE at this `OperationArgs` boundary has made
the unevaluated interpreter changeable.

Build with:

```sh
nice -n10 ninja -C ../build/gcc-16-debug-O -j11
meson test -C ../build/gcc-16-debug-O bali-phy:computation --print-errorlogs
```

### 2. Migrate IntMap sibling edges

Commit: `Propagate IntMap edge contingency`

Modify these operations in `src/builtins/IntMap.cc`:

| Operation | Controller | Migrated edges |
|---|---|---|
| `restrictKeysToVector` | IntMap USE OR IntSet USE | selected entry USEs |
| `forceAll` | IntMap USE | entry FORCEs |
| `exportIntMap` | IntMap USE | entry USEs |
| `toVector` | IntMap USE | entry USEs |

Evaluate each controller with
`evaluate_slot_to_value_with_edge_contingency()`.  Pass the controller
contingency to each entry operation and discard every entry `c_out`.  Preserve
entry order, pinning, and FORCE versus USE behavior.  Update comments to state
that entries are siblings and never inherit contingency from earlier entry
values.

Extend `tests/haskell/Data/IntMap` to exercise `restrictKeysToVector` and
`Foreign.IntMap.exportIntMapToVector` through the existing
`Foreign.Vector.vectorToList` interface.  Keep `NoImplicitPrelude`.

Before finalizing this commit, inspect controller and entry dispatch with GDB or
temporary inline logging at the existing IntMap and evaluator call sites; do
not add tracing helpers or global counters.  In the Data.IntMap test and a short
debug coalescent run, confirm that a fixed IntMap keeps every entry fixed even
when entries are changeable.  Remove any logging before recording the commit.

Then run the Data.IntMap test without instrumentation and take an intermediate
release coalescent benchmark measurement.  This commit should independently
recover the cost caused by misclassifying constant-map entry edges.

### 3. Migrate native vector and matrix list traversals

Commit: `Propagate native list edge contingency`

In `src/builtins/NativeVector.cc`, change `sized_vector_from_list()` as follows:

* Request the expected-size `c_out`, since size controls whether the first list
  cell and later iterations are inspected.
* After the zero-size return, start the list with
  `Args.evaluate_reg_use_with_edge_contingency(Args.reg_for_slot(1), size_c)`;
  do not add another slot overload.
* Evaluate each element with the current spine contingency and discard its
  `c_out`.
* Evaluate each required tail with tracked USE and carry its `c_out` to the
  next iteration.

In `src/builtins/Matrix.cc`, migrate all overloads of
`matrix_from_vectors()`, `join_vectors()`, and `matrix_from_list()`:

* Change the templated `matrix_from_vectors()` and `join_vectors()` helpers to
  receive their continuing spine as `UseWithEdgeContingency` rather than an
  `int` tail.
* Give each element USE the current spine contingency and give its sibling tail
  the original spine contingency.
* Enter `matrix_from_vectors()` with
  `tail_c | first_vector_c | count_c`; the first tail does not inherit
  `first_vector_c`.  Enter `join_vectors()` with
  `tail_c | first_vector_c`; discard the declared-total `c_out`.
* In `matrix_from_list()`, keep the initial slot USE fixed, then use
  `rows_c | columns_c | spine_c` for each loop element and tail.
* Do not propagate contingencies from same-representation checks or extent
  validation that can only continue normally or throw.

Run `Probability/PrimitiveVectorBuiltins` and
`Numeric/LinearAlgebra/{DenseConstruction,Vector,VectorInputLaziness,
VectorShortInput,ShortInput,MatrixVectorMismatch}`.

With GDB or temporary inline logging, confirm on one changeable spine that
element and tail edges are step-owned but the element result does not feed the
tail contingency.  Remove any logging before recording the commit.

### 4. Migrate boxed and foreign vector traversals

Commit: `Propagate boxed list edge contingency`

Modify these functions in `src/builtins/Vector.cc`:

* `boxedFromList`
* `boxedFromListNDefault`
* `boxedFromIndexedList`
* `boxedReplaceIndexed`
* `boxedAccumIndexed`
* `list_to_vector`
* `list_to_string`

For `boxedFromList`, track only spine USEs and continue retaining head registers
without evaluating them.  Do the same in `boxedFromListNDefault`, but request
the length `c_out` and, after the zero-length return, start the spine with
`Args.evaluate_reg_use_with_edge_contingency(Args.reg_for_slot(2), length_c)`.
Carry `length_c` through later spine USEs; do not add another slot overload.

For `list_to_vector` and `list_to_string`, USE each element with the current
spine contingency, discard its `c_out`, and continue the sibling tail with the
original spine contingency.

For each indexed-association traversal:

* Track the association USE using the current list-spine contingency.
* Use the association `c_out` as the index USE's `c_in`, because the index reg
  is selected from the pair value.
* Keep replacement values lazy and preserve existing roots.
* Continue the list tail with the original spine contingency, not the
  association or index contingency.

Length and bounds checks that only reject invalid input do not contribute to
the successful tail continuation.  Preserve the existing order of evaluation,
rooting, and combine-result forcing.

Run `Data/Vector/{Runtime,AppendStrictness,IndexErrors,ReplicateNegative}` and
`Foreign/RuntimeValue`.

### 5. Migrate categorical element FORCE edges

Commit: `Propagate categorical vector edge contingency`

In `src/builtins/Distribution.cc`, evaluate the boxed probability vector with
`evaluate_slot_use_with_edge_contingency()`.  Inspecting its stored element
registers is a USE of the vector representation, even though each probability
element is only FORCEd.

Pass the vector edge contingency to every element FORCE and request no element
`c_out`.  Preserve element order and the existing sampling buffer.

Run `Probability/CategoricalSampling` and `Probability/CategoricalEmpty`.

### 6. Remove execution-order inference

Commit: `Remove the legacy dependent edge API`

First require that:

```sh
rg 'evaluate_reg_dependent_(use|force)' src/builtins
```

returns no matches.  Then rename the migration-only virtual pair, in place, to
the protected `evaluate_reg_contingent_{use,force}` pair from the final API and
remove the migration note.  Update the non-virtual overloads to dispatch to the
renamed pair.  This is a rename of the sole virtual implementation in each
interpreter, not the introduction of a second pair followed by deletion of the
first.

In both unevaluated interpreters, remove the fallback that chooses a fixed edge
when `used_changeable` is false.  The final contingent primitives always record
on the step and assert that both `used_changeable` and `creator_step` are set.
The changeable classes record on their constructor-supplied step without a
runtime interpreter check; the unchangeable class throws `no_context()` from
both contingent primitives.  The base overloads only dispatch on explicit
`EdgeContingency`.

Keep `used_changeable` only for classifying the operation reduction and for
selecting its creator step.  Update comments in `args.H`, `evaluate.cc`, and
affected builtins so none describe execution order as the source of dependent
edge ownership.  After the removal, require that the same `rg` search over all
of `src` returns no matches.

### 7. Final verification

Build every commit with GCC 16 debug using `nice -n10` and 11 jobs.  At the
series endpoint, run all targeted tests above and at least:

```sh
timeout 240s meson test -C ../build/gcc-16-debug-O \
  'bali-phy:bali-phy 5d +A 50' --print-errorlogs
```

Build with `nice -n10 ninja -C ../build/gcc-16 -j11`.  From
`tests/prob_prog/infer_tree/coalescent/2`, warm the module cache, discard the
first run, and compare `master`, the API-only commit, the IntMap commit, and the
final endpoint with:

```sh
perf stat -e instructions -- "$PROJECT/build/gcc-16/src/bali-phy/bali-phy" \
  -m Model.hs RSV2-25taxa.fasta \
  --iter=1 --name ignore --seed=0

perf stat -e instructions -- "$PROJECT/build/gcc-16/src/bali-phy/bali-phy" \
  -m Model.hs RSV2-25taxa.fasta \
  --iter=50 --name ignore --seed=0
```

Record instruction counts and Work time, and require matching log and tree
hashes where behavior is intended to remain identical.  The IntMap midpoint
separates the expected recovery from later migrations.

Do not add a native test executable.  Any temporary tracing, counters, or
assertions used to diagnose edge ownership must be removed before the series is
complete.

## Completion criteria

* No builtin or public API refers to `evaluate_reg_dependent_use` or
  `evaluate_reg_dependent_force`.
* Ordinary operations retain the one-argument source API and share one tracked
  fixed-USE implementation that enters the target once.
* Constant IntMaps produce fixed sibling entry edges even when entries are
  changeable.
* Changeable IntMaps and list spines produce step-owned edges only for
  operations selected from their observed values.
* Element values do not make sibling IntMap entries or list tails contingent.
* Before temporary tracing is removed, it directly confirms fixed ownership for
  constant-IntMap sibling edges and step ownership for a changeable list spine.
* Debug and release builds succeed, targeted tests pass, and the 5d `+A` test
  passes.
* The controlled coalescent benchmark confirms the expected count-churn
  reduction without changing the retained stochastic trace.
