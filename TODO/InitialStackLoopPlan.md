# Initial `incremental_evaluate1` stack loop plan

This plan describes an experimental first step toward moving evaluation off
the C++ stack.  The scope is intentionally narrow: convert direct recursive
calls inside `incremental_evaluate1` and its helper paths into an explicit
frame loop.  Recursive calls that happen inside legacy `OperationArgs`
callbacks remain allowed for now.

The intended boundary after this plan is:

```text
direct recursion inside incremental_evaluate1: removed
legacy OperationArgs callback recursion: still present, marked compatibility
```

This is not full stack safety.  It is the first engine-shaped evaluator path
that later prepared and dynamic operations can plug into.

## Design analysis

This plan chooses one conservative point in the design space.  These are
implementation choices for the first experiment, not permanent constraints on a
later unified evaluator.

| Question | Choice in this plan | Main reason | Main alternative |
| --- | --- | --- | --- |
| Frame granularity | Activation frames | Fewer frames and one owner for active-register cleanup | Prefix/continuation frames with explicit `Leave` and `Return` frames |
| Dispatch | Enum plus switch | Simple to audit, debug, and trace | Function-pointer or threaded dispatch |
| Frame layout | One fixed `Eval1Frame` struct | Small first frame set and straightforward GC tracing | `std::variant`, frame descriptors, or variable-size payloads |
| Child-result delivery | Parent-frame mailbox | Result is stored next to the continuation that requested it | Engine-level pending result or separate result stack |
| Re-entry | Shared stack plus `base_depth` | Nested legacy calls can run without consuming outer frames | Local frame stack per public call |
| Cleanup | Central `finish_frame` helper | One place owns temp-root cleanup and active-register pop | Explicit `Leave` frames |
| Public return | `base_depth` plus final result | Avoids a separate return frame | Explicit `Return` frame |

Use `reg_heap` as the evaluator state.  Do not introduce an `Eval1Engine`
object in the first slice, since it would mostly duplicate access to registers,
steps, tokens, and heap mutation already owned by `reg_heap`.

Put the `eval1_frames` stack on `reg_heap`, not in a local object.  Nested
legacy calls from `OperationArgs` can still be handled by recording a
`base_depth` delimiter at each public entry to `incremental_evaluate1`: each
call runs only frames pushed above its starting depth.  Keeping frames on
`reg_heap` also lets GC trace and remap register ids stored in frames.

Use a parent-frame mailbox for child results:

```c++
std::optional<Eval1Result> child_result;
```

When a child frame returns, its result is written to the parent frame's mailbox.
On the next loop iteration, the parent resumes according to its frame kind.  This
avoids a separate return frame, a global `incoming_result`, and a generic action
interpreter.

Use activation frames, not prefix frames.  An `enter` frame is the initial state
of the frame that owns evaluation of one register.  If that evaluation needs a
child result, the same frame changes kind to the appropriate `after_*`
continuation before pushing the child frame.  The frame remains on the stack,
keeps the parent register active, and owns cleanup until `finish_frame` removes
it.  In other words, the frame kind records where evaluation of the register is
paused; it is not a separate stack frame for each source-level helper call.

This choice deliberately favors a smaller stack over a direct textual
translation of call/return.  A prefix-frame design would make the continuation
sequence more explicit:

```text
Return
Leave(r)
AfterChangeableCall(r, s)
Leave(child)
Enter(child)
```

That design remains plausible, especially if the engine later uses
function-pointer dispatch.  This first plan instead relies on the invariant that
an activation frame may mutate from `enter` to an `after_*` state while keeping
ownership of the active register.

Use switch dispatch, not function-pointer dispatch, in the first loop.  A
function-pointer or direct-threaded loop could make prefix frames look like
calls to small member functions, but frame tracing and exception unwinding would
still need a tag or descriptor.  Starting with a switch keeps dispatch and frame
metadata together while the invariants are still being tested.

Use a single fixed frame struct for the first loop.  Some fields are meaningful
only in some states.  This is less type-safe than separate frame objects, but it
keeps frame storage contiguous and makes the first GC update explicit and
small.  If the frame set grows, the next representation to consider is either a
`std::variant` of frame structs or a fixed frame header with variable-size
payloads and tracing descriptors.

Add `OperationArgs::steal_temp_heads()` because legacy callbacks currently keep
allocated result regs rooted through `OperationArgs` RAII.  When a legacy
operation result must be evaluated by the engine after `Args` goes out of scope,
the frame must take over that cleanup count.

## Commit 1: extract tail helpers

Do a behavior-preserving refactor in `src/computation/machine/evaluate.cc`.

Add private declarations in `src/computation/machine/graph_register.H`:

```c++
std::pair<int,int> incremental_evaluate1_changeable_call_tail_(int r, int s, std::pair<int,int> child);
std::pair<int,int> incremental_evaluate1_ref_with_force_tail_(int r, std::pair<int,int> child);
std::pair<int,int> incremental_evaluate1_unevaluated_ref_tail_(int r, int r2, bool was_index_var, std::pair<int,int> child);
std::pair<int,int> incremental_evaluate1_reduction_result_tail_(int r, int s, std::pair<int,int> child);
```

Replace current post-recursive blocks with calls to these helpers.  For example:

```c++
auto child = incremental_evaluate1(steps[s].call);
return incremental_evaluate1_changeable_call_tail_(r, s, child);
```

The helpers should contain only the code that currently runs after the recursive
call returns.  This commit should not change control flow.

Build after this commit.

## Commit 2: add temp-head transfer

Edit `src/computation/machine/args.H`:

```c++
int steal_temp_heads();
```

Edit `src/computation/machine/args.cc`:

```c++
// Compatibility: engine frames can steal legacy OperationArgs temp-head cleanup
// so callback-allocated results stay rooted across resumed child evaluation.
int OperationArgs::steal_temp_heads()
{
    int n = n_allocated;
    n_allocated = 0;
    return n;
}
```

This function should be used by the loop conversion commit.  It should not
remain unused long-term.

## Commit 3: refactor exception context formatting

Split the existing throwing helper into a prepend helper plus the current
throwing wrapper.

Add a helper like:

```c++
void prepend_reg_exception_context(reg_heap& M, int t, int R, myexception& e, bool changeable);
```

Then make the existing `throw_reg_exception(...)` call this helper and throw as
before.

This is behavior-preserving.  It is needed because the engine catches exceptions
at the loop boundary and must prepend contexts from active reduction frames.

## Commit 4: add eval1 frames and route `incremental_evaluate1` through the loop

Add private frame state to `reg_heap` in `src/computation/machine/graph_register.H`:

```c++
struct Eval1Result
{
    int dep_reg = 0;
    int value_reg = 0;
};

enum class Eval1FrameKind : unsigned char
{
    enter,
    forward_child_result,
    after_changeable_call,
    after_ref_with_force,
    after_unevaluated_ref,
    after_reduction_result
};

struct Eval1Frame
{
    Eval1FrameKind kind = Eval1FrameKind::enter;
    int r = 0;
    int s = 0;
    int child_reg = 0;
    bool was_index_var = false;
    bool active = false;
    int temp_heads_to_pop = 0;
    bool reduction_context = false;
    bool reduction_context_changeable = true;
    std::optional<Eval1Result> child_result;
};

std::vector<Eval1Frame> eval1_frames;
```

`Eval1FrameKind::enter` means "start or continue entering this frame's
register"; it does not mean that the frame will be popped as soon as the first
child evaluation is needed.  Before pushing a child, the current frame mutates
from `enter` to a continuation kind such as `after_changeable_call`.  That same
frame later receives the child result in `child_result`, performs the tail work,
and then finishes.

For example, the existing recursive shape:

```text
incremental_evaluate1(r)
  prelude(r)
  child = incremental_evaluate1(steps[s].call)
  tail(r, s, child)
  postlude(r)
```

becomes:

```text
enter(r)
  prelude(r)
  kind = after_changeable_call
  s = step_index_for_reg(r)
  push enter(steps[s].call)

enter(steps[s].call)
  ...
  finish_frame(child_result)

after_changeable_call(r, s)
  consume child_result
  run tail(r, s, child_result)
  finish_frame(parent_result)
```

The active-register lifetime spans the whole activation, including the time
while child frames are running.

Change public `incremental_evaluate1(int root)` into the loop:

```text
base_depth = eval1_frames.size()
push enter(root)

try:
  while eval1_frames.size() > base_depth:
    frame = eval1_frames.back()

    if frame.child_result:
      run resume switch
      continue

    run activation-state switch

catch:
  prepend reduction contexts for frames above base_depth
  unwind frames above base_depth
  rethrow
```

Do not hold an `Eval1Frame&` across `(*O)(Args)` or
`eval1_frames.push_back(...)`; nested legacy calls or vector growth may
invalidate references.  Store a frame index and reacquire the frame after such
calls.

### Finish-frame cleanup

Inside the loop, use a small local `finish_frame(result)` routine or lambda:

```text
done = eval1_frames.back()

pop done.temp_heads_to_pop temp heads

if done.active:
  stack_pop(done.r)
  reset reg_is_on_stack_bit on done.r

pop eval1_frames.back()

if eval1_frames.size() == base_depth:
  final_result = result
else:
  parent = eval1_frames.back()
  assert !parent.child_result
  parent.child_result = result
```

This is the replacement for the old public-wrapper pop.  The frame that
activated `r` owns popping `r`, even if its kind has changed from `enter` to an
`after_*` continuation while waiting for a child result.

### Resume switch

At the top of the loop, if the top frame has `child_result`, consume it:

```text
child = *frame.child_result
frame.child_result.reset()

switch frame.kind:
  forward_child_result:
    finish_frame(child)

  after_changeable_call:
    result = incremental_evaluate1_changeable_call_tail_(frame.r, frame.s, child_pair)
    finish_frame(result)

  after_ref_with_force:
    result = incremental_evaluate1_ref_with_force_tail_(frame.r, child_pair)
    finish_frame(result)

  after_unevaluated_ref:
    result = incremental_evaluate1_unevaluated_ref_tail_(frame.r, frame.child_reg, frame.was_index_var, child_pair)
    finish_frame(result)

  after_reduction_result:
    result = incremental_evaluate1_reduction_result_tail_(frame.r, frame.s, child_pair)
    finish_frame(result)

  enter:
    abort
```

No separate leave frame.  No separate return frame.  No generic action
interpreter.  The `finish_frame` helper performs the work that a leave frame
would otherwise do, and the `base_depth`/`final_result` case performs the work
that a return frame would otherwise do.

### Activation-state switch

For `enter`, first activate the frame if needed:

```text
debug check !reg_is_on_stack(r)
set reg_is_on_stack_bit
stack.push_back(r)
frame.active = true
```

Then inline the current `incremental_evaluate1_` dispatch:

```text
reg_is_constant(r):
  finish_frame({r, r})

reg_is_changeable(r):
  run changeable logic below

unevaluated_reg_is_ref_no_force(r):
  frame.kind = forward_child_result
  push enter(closure_at(r).reg_for_ref())

reg_is_ref_with_force(r):
  frame.kind = after_ref_with_force
  push enter(closure_at(r).reg_for_ref())

otherwise:
  run unevaluated logic below
```

For changeable regs:

```text
total_changeable_eval++

if result_for_reg(r) > 0:
  total_changeable_eval_with_result++
  finish_frame({r, result})

else if step_index_for_reg(r) > 0:
  frame.kind = after_changeable_call
  frame.s = s
  push enter(steps[s].call)

else:
  run legacy reduction
```

For unevaluated refs:

```text
was_index_var = is_index_var_code(closure_at(r))
r2 = closure_at(r).reg_for_ref()

frame.kind = after_unevaluated_ref
frame.child_reg = r2
frame.was_index_var = was_index_var
push enter(r2)
```

For WHNF with no forced regs:

```text
mark_reg_constant(r)
finish_frame({r, r})
```

For WHNF with forced regs, preserve current behavior:

```text
allocate r2
copy closure r -> r2
replace r with IndexVar(0) pointing to r2
continue with same frame as enter
```

For legacy reductions:

```text
frame.reduction_context = true
frame.reduction_context_changeable = true

create RegOperationArgs1* Args
run operation callback
```

If an unevaluated reduction does not use changeable data:

```text
set_C(r, value)
steps.reclaim_used(s)
frame.reduction_context = false
continue with same frame as enter
```

If a reduction produces a changeable result:

```text
mark_reg_changeable(r) when appropriate
choose or allocate r2
frame.temp_heads_to_pop += Args.steal_temp_heads()
frame.kind = after_reduction_result
frame.s = s
push enter(r2)
```

Steal temp heads only after `(*O)(Args)` succeeds and after any
`Args.allocate(...)` needed for `r2`.  If the callback throws before that point,
normal `OperationArgs` destruction cleans up.

## GC changes

Since `eval1_frames` lives on `reg_heap`, update GC immediately.

`get_roots` should add, for each frame:

```text
frame.r if > 0
frame.child_reg if > 0
frame.child_result->dep_reg if present and > 0
frame.child_result->value_reg if present and > 0
```

`reg_heap::trace` must remap the same register fields.

Do not trace `s`; it is a step id, not a register.  Actual stolen temp roots
remain in `temp`.

Frame invariants:

- frames may store register ids and step ids;
- frames must not store `closure`, `Runtime::Exp`, or references into heap
  storage across child evaluation;
- callback-allocated result regs must remain rooted by stolen temp heads;
- child results are stored only in the parent mailbox and traced while present;
- any future frame that stores heap-like data must get explicit tracing before
  it is used.

## Exception unwinding

On exception from the loop:

1. For frames above `base_depth`, iterate from top to bottom and prepend context
   for frames with `reduction_context`.
2. For `error_exception`, only prepend when `log_verbose`, matching current
   behavior.
3. For `myexception`, always prepend.
4. For `std::exception`, convert to `myexception`, prepend, unwind, then throw
   the new exception.
5. For unknown exceptions, just unwind and rethrow.

Then unwind frames above `base_depth` in reverse order using the same cleanup as
`finish_frame`, but without delivering results.

Iterating top-to-bottom while using `prepend` preserves the existing recursive
order: outer context ends up before inner context.

## Commit 5: delete old recursive evaluate1 helpers

After the loop handles all direct sites, delete the old helper declarations and
definitions:

```text
incremental_evaluate1_
incremental_evaluate1_changeable_
incremental_evaluate1_ref_with_force_
incremental_evaluate1_unevaluated_
```

Let the compiler report any remaining accidental users.

Keep `RegOperationArgs1Changeable` and `RegOperationArgs1Unevaluated`; their
calls to `M.incremental_evaluate1(...)` are the explicit compatibility boundary.

Add a brief note near the legacy operation callback path:

```c++
// Compatibility: legacy OperationArgs callbacks may recursively enter
// incremental_evaluate1. The eval1 frame stack uses base-depth delimiters so
// nested calls do not consume outer frames. Prepared operations should remove
// this path later.
```

## Verification

After each commit:

```bash
ninja -C /home/bredelings/Devel/bali-phy/build/clang-19-debug-O
```

After the full conversion:

```bash
meson test -C /home/bredelings/Devel/bali-phy/build/clang-19-debug-O "bali-phy:bali-phy 5d +A 50"
```
