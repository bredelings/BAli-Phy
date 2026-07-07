# Dependent USE/FORCE Edges

This plan describes how to add USE and FORCE edges whose targets are discovered
while a step is executing.  Fixed USE/FORCE edges remain attached to the source
reg.  Dependent USE/FORCE edges are attached to the retained step.

The goal is to let operations such as case, IntMap traversal, and future
dynamic builtins express dependent demand without allocating an extra heap cell
solely to get a call edge.

## Current Model

The current machine records USE and FORCE edges on `reg::used_forced_regs`.
These edges are fixed for the heap cell: evaluating the same source expression
is expected to perform the same USE and FORCE observations.

`Step` currently records the dependent continuation through `call` and
`call_edge`.  The call edge is special:

* it demands the called reg;
* it contributes to force counts;
* it has a `called_by` back edge for invalidating results;
* it determines the source reg's result through `set_result_for_reg()`.

This means that dependent execution can currently be represented only by
allocating a called cell and calling it.  Some operations would be better
represented by letting the same step discover additional USE/FORCE edges.

## Design Invariants

Fixed dependencies and dependent dependencies have different owners:

* `reg::used_forced_regs` records fixed USE/FORCE edges for a source reg.
* `Step::used_forced_regs` records dependent USE/FORCE edges for a retained
  step.

Dependent USE/FORCE edges may be recorded only after the source reg has become
changeable.  In practice, a fixed USE or an effect makes the source reg
changeable first.  This guarantees that the step will be retained and gives the
dependent edges a stable owner.

The replay order is canonical rather than historical:

1. Replay all fixed USE/FORCE edges on the source reg.
2. If no fixed USE has changed, replay dependent USE/FORCE edges on the step in
   their recorded order.

This is valid under the assumption that fixed edges do not depend on dependent
edges, and that each dependent edge appears after the dependent edges that it
depends on.  We do not require the old runtime observation order to be "all
fixed first, then all dependent"; we only require replaying in that canonical
order to be equivalent.

If replay finds that a USE input changed, replay should short-circuit and
re-execute the operation.  In particular, if a dependent USE changes, do not
replay later old dependent edges.  The new execution will discover and count the
new dependent edge list.

For phase 1, dependency shape must not depend only on the value of a FORCE edge.
If an operation uses a value to decide which later edges to record, then that
earlier observation must be a USE, or we need a future FORCE mode that says
"value changes do not replace the sampled result, but they do invalidate the
dependency shape."

## Representation Changes

Add a dependent edge list to `Step`:

    boost::container::small_vector<use_force_edge, 2> used_forced_regs;

The existing `use_force_edge` type can be reused.  For a USE edge, `target` and
`back_index` have the same meaning as they do for reg-owned USE edges.  For a
FORCE edge, only `mode` and `reg` are meaningful.

Rename the current USE back-edge list:

    reg::used_by -> reg::used_by_reg

and add a new step-owned USE back-edge list:

    reg::used_by_step

The new list can initially have the same shape as the old one:

    boost::container::small_vector<std::pair<int,int>, 2> used_by_step;

where the pair is `(step_index, edge_index)`.  A step-owned USE back edge
invalidates `steps[s].source_reg`, but only when `prog_steps[source_reg] == s`.
This differs from a reg-owned USE edge, which directly invalidates the source
reg's step.

Keeping separate back-edge lists is clearer than adding an owner tag to the
existing list.  The traversal code has a little duplication, but the owner
semantics differ enough that the duplication should pay for itself.

## Edge Recording APIs

Keep the existing fixed-edge APIs:

    void set_used_reg(int source_reg, int observed_reg);
    int set_forced_reg(int source_reg, int observed_reg);

Add step-owned APIs:

    void set_step_used_reg(int step, int observed_reg);
    int set_step_forced_reg(int step, int observed_reg);

The new APIs should mirror the current ref-following and assertion behavior of
`set_used_reg()` and `set_forced_reg()`, with additional assertions:

* `steps[step].source_reg > 0`;
* `prog_steps[steps[step].source_reg] == step` while recording into the current
  root step;
* `reg_is_changeable(steps[step].source_reg)`;
* the step is not already marked as decremented in `prog_unshare`.

The last two assertions encode the phase-1 lifetime rule: no dependent edge is
recorded on a temporary step that may be reclaimed as an ordinary pure
reduction.

If an operation computes an edge list from pure data before the source reg has
become changeable, those edges should not use the step-owned APIs.  They are
still fixed for this heap cell and should be recorded through the existing
reg-owned APIs.  For example, forcing every field of a pure `IntMap` can record
ordinary fixed FORCE edges on the reg.  If reading the `IntMap` required a fixed
USE and made the source reg changeable, then the field FORCE edges become
dependent step edges.

## OperationArgs Changes

Add explicit dependent-demand APIs to `OperationArgs`:

    virtual int evaluate_reg_dependent_force(int);
    virtual int evaluate_reg_dependent_use(int);

and convenience wrappers:

    std::optional<int> evaluate_code_dependent_force(const Runtime::Exp&);
    std::optional<int> evaluate_code_dependent_use(const Runtime::Exp&);
    int evaluate_slot_dependent_force(int);
    int evaluate_slot_dependent_use(int);

The existing `evaluate_reg_force()` and `evaluate_reg_use()` keep their current
meaning: they record fixed operation dependencies.

Class-specific behavior:

* `RegOperationArgs1Unevaluated`: dependent APIs evaluate like the fixed APIs
  but record on `Step::used_forced_regs`.  They assert that `make_changeable()`
  has already been called or that the source reg is already changeable.
* `RegOperationArgs1Changeable`: dependent APIs evaluate recursively and record
  on the current step.  Fixed APIs remain non-recording because fixed
  dependencies should already be present.
* `RegOperationArgs2Unevaluated`: dependent APIs record on the step and
  increment force counts for newly recorded step edges, just as new call edges
  are counted when the step is installed.
* `RegOperationArgs2Changeable`: when re-executing an invalid step, dependent
  APIs record the new step-local edge list.  Fixed APIs keep the current
  "already replayed" behavior.
* `RegOperationArgsUnchangeable`: dependent APIs behave like the existing
  unchangeable use/force APIs and throw `no_context()` if they encounter a
  changeable value.

The first builtins converted should call the dependent APIs only after a fixed
USE or effect has made the operation changeable.

## Replay and Retention

`force_regs_check_same_inputs(int r)` should become the central replay routine
for both edge lists:

1. Assert `reg_is_changeable(r)`.
2. Replay fixed `regs[r].used_forced_regs`.
3. If any fixed USE has a different result, return `false` immediately.
4. Replay `steps[prog_steps[r]].used_forced_regs` in order.
5. If any dependent USE has a different result, return `false` immediately.
6. Return `true` if all USE inputs match.

FORCE edges demand their child but do not affect `same_inputs`.

This short-circuit behavior is important.  It avoids evaluating old dependent
edges that are no longer on the new path after the first changed dependent USE.
When the old step is not retained, the re-executed operation discovers and
counts the new dependent edges.

`force_reg_no_call()` and `force_reg_with_call()` should also force dependent
step edges when the source reg has a retained step.  Conceptually, forcing a
valid changeable reg must force:

* fixed USE/FORCE edges on the reg;
* dependent USE/FORCE edges on the step;
* the step call, if present.

## Force Counts

Treat step-owned USE/FORCE edges like `Step::call` for force-count purposes.
The group is:

    Step::used_forced_regs + Step::call

The main rule is that counts for this group are incremented and decremented at
the same places where call counts are currently incremented and decremented.

Affected paths:

* installing a new retained step in `incremental_evaluate2_unevaluated_()`;
* installing a replacement step in `incremental_evaluate2_changeable_()`;
* tentatively decrementing invalid steps in
  `decrement_calls_from_invalid_steps()`;
* decrementing old steps bumped into the child token;
* decrementing steps attached to zero-count regs;
* revalidating an invalid step when `step_edges_decremented_bit` is set.

The existing `step_edges_decremented_bit` should cover the whole group, not just
`Step::call`.  If an invalid step is retained, replay must re-add counts for
both dependent step edges and the call.  If the step is replaced, the old counts
stay decremented and the new step records and increments its own edges.

It will probably be worth adding small internal helpers:

    increment_step_demand_counts(int step);
    decrement_step_demand_counts(int step);
    replay_step_demand_counts_if_decremented(int r, int step);

These helpers should be introduced only as part of the first call-site
conversion, not as unused infrastructure.

## Invalidation

Result-change invalidation currently scans:

* `reg::called_by`, which invalidates callers' results;
* `reg::used_by`, which invalidates users' steps.

After this change, it should scan:

* `reg::called_by`;
* `reg::used_by_reg`;
* `reg::used_by_step`.

For `used_by_step`, invalidation should do:

    int source = steps[step].source_reg;
    if (prog_steps[source] == step)
        unshare_step(source);

This check matters because old steps may remain allocated in non-root tokens.
A back edge from such a step should not invalidate a different step currently
mapped at the same source reg.

Both `invalidate_regs2()` and `find_unshared_regs()` in `reroot.cc` need this
update, because both have local `do_result_changed()` logic.

## Clearing and Destruction

`clear_back_edges_for_reg()` should remove fixed USE backrefs from
`used_by_reg`, as it does today for `used_by`.

`clear_back_edges_for_step()` should remove step-owned USE backrefs from
`used_by_step`, clear `Step::used_forced_regs`, and then clear call and created
reg edges as it does today.

Do not clear `Step::used_forced_regs` merely when `note_step_not_in_root()` is
called.  Bumped steps can remain alive in child tokens for rerooting.  Step
edges should be removed when the step is destroyed or reclaimed, not when it
leaves the current root.

`check_back_edges_cleared_for_step()` should assert that the step has no
dependent edges after clearing.

## GC

Dynamic step edges are not necessarily reachable through a closure environment,
so GC must mark all targets of live `Step::used_forced_regs`, both USE and
FORCE.

For fixed reg-owned edges, the current GC explicitly marks FORCE edges but not
USE edges.  This appears to rely on the invariant that fixed USE targets are
also reachable through the source closure or environment.  Keep this behavior
initially, but document the invariant near the GC loop and add debug assertions
where practical.

If that invariant becomes false, fixed USE edges should also be marked by GC.
That would be conservative for memory but would change retention behavior, so it
should be a deliberate change.

## Debug Checks and Display

Update `check_force_counts()` to include `Step::used_forced_regs` for live
mapped steps, in the same way it includes calls.

Update `check_used_regs()` to validate:

* fixed USE backrefs in `used_by_reg`;
* dependent USE backrefs in `used_by_step`;
* step edge targets are not free;
* step-owned edges point back to a valid step edge;
* a mapped step with dependent edges belongs to a changeable source reg.

Update helper views:

* `used_regs_for_reg()`;
* `forced_regs_for_reg()`.

Either make these include both fixed and dependent edges for the mapped step, or
add explicit fixed/step variants and update graph display code accordingly.  For
debugging, the graph should show step-owned dynamic edges differently from fixed
reg-owned edges if that is easy.

## Initial Conversion

The first conversion should be a dependent-FORCE operation whose dynamic edge
shape is simple.  `IntMap.forceAll` is a good candidate:

* it discovers a variable set of regs from an `IntMap`;
* it records FORCE edges, not USE edges;
* it does not use forced values to decide which later edges to record.

The builtin should still perform the ordinary fixed evaluation of the map value
first.  If that fixed evaluation makes the source reg changeable, then field
FORCE edges should be recorded as dependent step edges.  If the map value is
pure and the source reg is not retained, then the field FORCE edges should
continue to be recorded as reg-owned fixed FORCE edges.

A later conversion can handle a dependent-USE case, such as a direct version of
`restrictKeysToVector`.  That should wait until the step-edge invalidation and
short-circuit replay behavior has been tested with dependent FORCE.

## Affected Code

Primary files:

* `src/computation/machine/graph_register.H`
  * add `Step::used_forced_regs`;
  * rename `used_by` to `used_by_reg`;
  * add `used_by_step`;
  * declare step edge helpers.
* `src/computation/machine/graph_register.cc`
  * update `Step` clear/move/check;
  * add step-owned edge recording and clearing;
  * update `force_regs_check_same_inputs()`;
  * update `force_reg_no_call()` and `force_reg_with_call()`;
  * update force-count checks and back-edge checks.
* `src/computation/machine/args.H` and `args.cc`
  * add dependent use/force APIs.
* `src/computation/machine/evaluate.cc`
  * implement dependent API behavior in all `RegOperationArgs*` classes;
  * update count handling when installing, retaining, and replacing steps.
* `src/computation/machine/reroot.cc`
  * update invalidation scans;
  * update decrement/replay of step demand counts.
* `src/computation/machine/gc.cc`
  * mark dynamic step USE/FORCE targets;
  * document fixed USE reachability.
* `src/computation/machine/tokens.cc`
  * ensure step destruction clears dependent edge backrefs.
* `src/computation/machine/show_graph.cc`
  * display dependent edges or use updated helper views.
* `src/builtins/IntMap.cc`
  * convert `forceAll` first.

Secondary audit targets:

* all direct references to `used_by`;
* all loops over `used_forced_regs`;
* all places that assume `regs[r].used_forced_regs.empty()` means no demand
  edges exist for a mapped step;
* all comments mentioning call-only decrementing or call-only step demand.

## Tests

Use `build/gcc-16-debug-O`.

Minimum verification:

    ninja -C ../build/gcc-16-debug-O
    meson test -C ../build/gcc-16-debug-O 'bali-phy:bali-phy 5d +A 50'

Also run tests that have previously exposed force-count or `has_step2()` bugs:

    meson test -C ../build/gcc-16-debug-O \
      'bali-phy:bali-phy testsuite prob_prog/infer_tree/coalescent/1' \
      'bali-phy:bali-phy testsuite prob_prog/coal_mining'

Add focused tests for:

* a dynamic FORCE edge list whose selected children change after reroot;
* a retained step where dependent FORCE counts are decremented and then
  re-added;
* a later dependent USE case, once a dependent-USE builtin is converted.

## Open Questions

* Do we need a new FORCE mode for values that may affect dependency shape but
  should not replace a sampled result?
* Should fixed USE edges eventually be marked by GC, or should we keep the
  closure-reachability invariant?
* Should graph display distinguish fixed and dependent edges visually?
* After the first dependent-USE conversion, can any old compatibility wrappers
  or extra allocation paths be removed?
