# Moving evaluation off the C++ stack

The evaluators in `src/computation/machine/evaluate.cc` currently use the C++
call stack to represent object-language evaluation.  That is a poor fit for
Haskell-style code, because ordinary recursion in the object language can need
stack proportional to heap.  The goal is to make evaluator continuations
explicit and managed by the runtime.

The current code uses the C++ stack in two ways:

1. Evaluator-internal recursion, such as evaluating a referenced register,
   evaluating the call of a changeable register, or evaluating the result of a
   reduction.
2. Operation callback recursion, where a builtin receives `OperationArgs&` and
   calls back into the evaluator with methods such as `evaluate_slot_to_value`,
   `evaluate_slot_force`, or `evaluate_reg_to_closure`.

Solving only one of these is not enough.  A stack-safe design needs both:

- an operation protocol that makes argument demands visible to the evaluator;
- an explicit evaluator loop that uses heap-allocated frames instead of
  recursive calls to `incremental_evaluate*`.

This note does not depend on storing evaluator kinds on registers, adding
`runST`, or JIT compilation.  Those ideas may reuse this machinery later, but
they should remain separate design problems.

## Summary of the proposed design

The preferred design is:

1. Use fixed-demand prepared operations for the common case.
2. Use dynamic operation frames only for operations whose demand pattern really
   depends on earlier demand results.
3. Represent evaluator continuations with an explicit control stack.
4. Keep operation argument results in a generic prepared-operation frame, rather
   than copying closures or values into ad-hoc storage.
5. Implement `incremental_evaluate1`, `incremental_evaluate2`, and
   `incremental_evaluate_unchangeable` as wrappers around one stack engine with
   mode-specific policy.

The fixed-demand operation protocol is the main part of the builtin conversion.
Most existing operations already have this shape: for one invocation, they know
which arguments they will use or force.  Later evaluation may depend on the
closure produced by the operation, but that is a new operation invocation.

## Semantic constraints

The initial stack refactor should preserve behavior.

In particular, it should preserve:

- `incremental_evaluate1` semantics;
- `incremental_evaluate2` semantics;
- `incremental_evaluate_unchangeable` semantics;
- use and force dependency recording;
- effect registration behavior;
- step/result/token-delta behavior;
- force-count behavior;
- active-register cycle checks;
- exception context;
- garbage-collection roots.

The existing `reg_heap::stack` should not be repurposed as the new continuation
stack.  It records registers that are actively being evaluated.  That active-reg
stack is useful for cycle checks, exception context, and GC treatment of
partially evaluated registers.  A continuation stack records what the evaluator
should do next.  These are different concepts and should remain separate at
first.  Later, the old field could be renamed to something like `active_regs`.

## Why arbitrary `OperationArgs` callbacks are hard

An ordinary builtin currently hides its evaluation plan inside C++ code:

```c++
extern "C" closure builtin_function_foo(OperationArgs& Args)
{
    auto x = Args.evaluate_slot_to_value(0);
    int r = Args.evaluate_slot_force(1);
    auto y = Args.evaluate_slot_to_value(2);

    return compute_foo(x, r, y);
}
```

The evaluator cannot generate explicit stack frames for this directly, because
it does not know ahead of time:

- how many arguments the operation will evaluate;
- whether each argument is used or forced;
- whether an argument is evaluated as a value, closure, or register;
- whether the operation will call back into evaluation again later.

The key step is to make this evaluation plan data.

## Fixed-demand prepared operations

A prepared operation splits an operation into two parts:

1. A declarative demand plan.
2. A non-recursive finalizer.

The evaluator executes the demand plan.  When all arguments are prepared, it
calls the finalizer.  The finalizer may inspect the heap, allocate registers,
set effects, and return a closure, but it must not call back into evaluation.

Sketch:

```c++
enum class prepared_arg_mode : uint8_t
{
    raw_reg,
    use_reg,
    force_reg,
    use_value,
    force_value,
    unchangeable_reg
};

using prepared_finish_fn = closure (*)(PreparedArgs&);

struct PreparedOperation: public Object
{
    std::string name;
    std::span<const prepared_arg_mode> arg_plan;
    prepared_finish_fn finish = nullptr;
};
```

The exact representation can differ.  It may be better to split the mode into
orthogonal fields:

```text
target: operation slot / expression / env entry / operation-specific target
demand: raw / use / force / unchangeable
result shape: reg / closure / value
count policy: default / count on return / do not count
```

That would avoid modes such as `use_value` and `force_value` growing into many
combinations.  The simple enum is still a useful first sketch.

## Prepared operation frame

`PreparedArgs` should be cheap.  It should not copy closures or runtime values
into separate vectors.  It should be a view over the current prepared-operation
frame.

The frame stores a small amount of integer state per argument:

```c++
struct PreparedArgState
{
    int source_reg = 0;  // original register for the operation slot
    int dep_reg    = 0;  // dependency-facing register returned by evaluation
    int value_reg  = 0;  // register containing the WHNF/value result
};

struct PreparedOpFrame
{
    int current_reg = 0;
    int step = 0;
    const PreparedOperation* op = nullptr;
    uint8_t next_arg = 0;
    bool first_eval = false;
    bool used_changeable = false;
    small_vector<PreparedArgState, 6> args;
};
```

`source_reg`, `dep_reg`, and `value_reg` have different jobs:

- `source_reg` is the register named by the operation argument before
  evaluation.
- `dep_reg` is the register that dependency bookkeeping should see after
  following relevant references.
- `value_reg` is the register whose closure contains the WHNF/value result.

`PreparedArgs` is then just an accessor object:

```c++
class PreparedArgs
{
    reg_heap& M;
    PreparedOpFrame& F;

public:
    reg_heap& memory() { return M; }

    int current_reg() const { return F.current_reg; }
    int current_step() const { return F.step; }

    int source_reg(int i) const { return F.args[i].source_reg; }
    int dep_reg(int i) const { return F.args[i].dep_reg; }
    int value_reg(int i) const { return F.args[i].value_reg; }

    const closure& closure(int i) const
    {
        assert(value_reg(i) > 0);
        return M.closure_at(value_reg(i));
    }

    const Runtime::Exp& value(int i) const
    {
        return closure(i).get_code();
    }

    int allocate(closure&&);
    int allocate_reg();
    void set_effect(int r_effect);
};
```

The finalizer reads values by reference from the heap.  This avoids the current
`e_op` pattern of evaluating arguments and copying or moving `Runtime::Exp`
values onto a separate value stack.

## Executing a prepared operation

When the evaluator enters a prepared operation:

1. Create a `PreparedOpFrame`.
2. Fill `args[i].source_reg` from the operation slots.
3. Set `next_arg = 0`.
4. Interpret the demand plan under the current evaluator policy.

For each argument:

### `raw_reg`

No evaluation is performed.

```text
arg.source_reg = slot register
arg.dep_reg = source_reg
arg.value_reg = 0
next_arg++
```

The finalizer is expected to use `source_reg(i)`, not `value(i)`.

This is for operations that preserve laziness or register identity, such as
constructing closures that refer to unevaluated arguments.

### `use_reg`

The evaluator requests evaluation of the source register with use semantics.

```text
set prepared frame state to waiting for argument i
push EnterReg(source_reg, demand = Use)
```

When the child evaluation completes:

```text
arg.dep_reg = returned.dep_reg
arg.value_reg = returned.value_reg
policy records USE-related bookkeeping
next_arg++
```

The active evaluator policy determines what "use" means.  For example,
`incremental_evaluate1` and `incremental_evaluate2` record/change bookkeeping
differently.  The prepared operation only declares that this argument is used.

### `force_reg`

This is the same shape as `use_reg`, but the child evaluation request uses force
semantics.

```text
set prepared frame state to waiting for argument i
push EnterReg(source_reg, demand = Force)
```

### `use_value`

This is `use_reg` plus a check that the finalizer can safely access a value-like
result through `PreparedArgs::value(i)`.

The stored state is still just registers:

```text
arg.dep_reg = returned.dep_reg
arg.value_reg = returned.value_reg
```

The value itself is not copied into the frame.

### `force_value`

This is `force_reg` plus the same value-like result check.

### `unchangeable_reg`

The evaluator requests evaluation of the source register with unchangeable
semantics and stores the resulting register.

This is useful for operations that currently call
`Args.evaluate_slot_unchangeable`.

## Calling the finalizer

When `next_arg == op->arg_plan.size()`, all argument state is present in the
frame:

```c++
PreparedArgs args{M, frame};
closure result = frame.op->finish(args);
```

The plan should not be scanned again to build `PreparedArgs`.  The plan was
used to fill the frame.  `PreparedArgs` is only a view over that frame.

The finalizer may perform non-recursive heap operations:

- allocate registers;
- set closures;
- create effect objects;
- call `set_effect`;
- inspect source/result registers;
- call non-evaluating helpers such as `follow_reg_ref_no_force`.

The finalizer must not call back into evaluation:

- no `evaluate_slot_to_value`;
- no `evaluate_slot_to_closure`;
- no `evaluate_reg_to_closure`;
- no `evaluate_slot_force`;
- no `evaluate_slot_use`;
- no `evaluate_reg_force`;
- no `evaluate_reg_use`;
- no `incremental_evaluate*`;
- no `lazy_evaluate*`;
- no `context_ref::evaluate*`.

This is the main rule that makes prepared operations stack-safe.

## Dynamic-demand operations

Most operations have fixed demands, but not all.  Operations over structures
such as `EVector` or `IntMap` may need to inspect one value to decide which
value or values to inspect next.  An implementation that analyzes only one
element per operation can fit the fixed-demand protocol, but may be much slower
than analyzing several dependent elements inside one operation.

These operations should use dynamic activation frames:

```text
DynamicOpFrame {
    current_reg
    step
    operation identity
    operation-specific state
    temporary-root mark
}
```

The frame repeatedly performs one dynamic step:

```text
dynamic state -> need demand D and updated state
dynamic state + demand result -> need another demand
dynamic state + demand result -> done with result closure
```

Dynamic operation frames should reuse the same child-evaluation and demand
recording machinery as fixed operations.  They should not introduce a second
way to record use, force, counts, or token deltas.

There is a further semantic question: if one dynamic operation performs
multiple dependent uses or forces, those dependencies may belong more naturally
to the step than to a fixed set of registers.  Supporting that precisely may
require step-local use/force edges.  That is a separate semantic extension,
because it affects unsharing, force counts, token deltas, and invalidation.

The first stack-safety implementation should not require step-local use/force
edges.  It should allow dynamic frames structurally, but the first converted
dynamic operation can either use existing dependency mechanisms or wait until
the step-edge design is ready.

## Stack representation

The primary runtime structure should be an explicit evaluator engine:

```text
EvalEngine {
    vector<EvalFrame> control_stack
    optional<EvalResult> incoming_result
}
```

`EvalResult` is the result of entering a register:

```text
EvalResult {
    dep_reg
    value_reg
}
```

This matches the current pair returned by `incremental_evaluate1` and
`incremental_evaluate2`, but names the two roles.

The main frame kinds are:

```text
EnterReg
PreparedOpFrame
DynamicOpFrame
FinishReductionResult
Cleanup
```

`EnterReg` evaluates one register under an `EvalRequest`.  A child frame returns
by setting `incoming_result` and popping itself.  The parent frame consumes the
incoming result and either advances or produces another result.

This differs from a bytecode-style control stack plus result stack.  That
alternative is also viable:

```text
control stack: Finish(+), Eval(y), Eval(x)
result stack:  evaluated demand results
```

The result-stack design is attractive for fixed-demand operations because an
operation can expand into a little stack program.  However, the frame-local
`PreparedOpFrame` design has practical advantages for this codebase:

- source, dependency, and value registers live together in named fields;
- `PreparedArgs` can be a simple view over the frame;
- finalizers do not need to know about result-stack layout;
- cleanup state and temporary-root marks live with the operation;
- stack-effect bugs are less likely than with an untyped result stack.

For these reasons, the preferred design is a control stack with frame-local
operation state.  A result stack can still be added later if profiling or
implementation pressure shows that command expansion is better for fixed
operations.

## Evaluator requests and policy

Each `EnterReg` frame should carry an evaluation request:

```text
EvalRequest {
    reg
    mode
    demand_kind
    count_policy
}
```

The mode distinguishes the three existing evaluators:

```text
Eval1
Eval2
Unchangeable
```

The demand kind says why this child is being evaluated:

```text
Plain
Use
Force
Unchangeable
```

The count policy is especially important for `Eval2`.  The current
`incremental_evaluate2(r, do_count)` attaches count behavior to individual
child evaluations.  It should not become a single global flag on the engine.
Different child evaluations from the same root may need different count
behavior.

The engine owns the control flow.  A policy layer owns mode-specific semantics.
The policy answers questions such as:

- What happens when entering a changeable register?
- Is evaluating this register allowed to continue, or should it stop with
  `no_context`?
- How is a use demand recorded?
- How is a force demand recorded?
- Should force counts be incremented on return?
- Which token deltas should be written?
- How should an old result or old step be bumped?

For `Eval1`, the policy mostly records first-execution dependencies and creates
steps/results when a reduction becomes changeable.

For `Eval2`, the policy must handle force counts, `do_count`-like behavior,
token deltas, unshared results, bumped steps, and recomputation of invalid
changeable registers.

For `Unchangeable`, the policy should stop at changeable or forcing boundaries
and report `no_context` rather than attempting incremental bookkeeping.

The policy should be narrow.  It should not own the whole evaluator.

## Engine loop

The public entry points should remain:

```text
incremental_evaluate1(r)
incremental_evaluate2(r, do_count)
incremental_evaluate_unchangeable(r)
```

Internally, they submit an initial `EnterReg` request to the engine:

```text
push EnterReg(root request)

while control_stack is not empty:
    frame = control_stack.back()

    if incoming_result exists:
        let frame consume incoming_result
        clear incoming_result
        continue

    step frame
```

A frame step can:

- push another frame;
- replace itself with another frame;
- mutate the heap and continue;
- pop itself and set `incoming_result`;
- throw an exception.

The important point is that recursive evaluator calls become frame pushes.

### Entering registers

`EnterReg` replaces the recursive body of the current evaluator functions.  Its
transitions should match the current cases.

For an already evaluated constant, it returns:

```text
EvalResult { dep_reg = r, value_reg = r }
```

For an unchangeable reference, it pushes work to enter the referenced register
and records enough state to perform any reference-chain update needed for the
original register.

For `ref_with_force`, the policy determines whether force behavior is needed in
the current mode.  `Eval1` can mostly look through the reference.  `Eval2` must
preserve the existing force-count semantics.

For a changeable register, the policy decides whether an existing result is
usable, whether an existing call should be evaluated, or whether the original
closure should be reduced again.

For an unevaluated operation, `EnterReg` creates or finds the step needed for
reduction and then dispatches according to operation kind:

```text
prepared fixed operation -> push PreparedOpFrame
dynamic operation        -> push DynamicOpFrame
legacy operation         -> temporary compatibility path
```

The legacy operation path is not stack-safe if it recursively evaluates through
`OperationArgs`.  It is a migration aid only and should be marked as such in
code.

### Handling a prepared operation result

A prepared finalizer returns a closure.  The current evaluator then has two
broad cases.

If the reduction did not depend on changeable data, the parent register can be
updated with the returned closure and evaluation continues on the same register.
In the explicit loop this can be represented by setting the closure and pushing
or replacing with `EnterReg(parent_reg)`.

If the reduction did depend on changeable data, the parent register becomes
changeable.  If the returned closure is not already a register reference, it is
allocated into a child register.  The engine then evaluates that child register
so that the parent step can record its call/result.

This is where a `FinishReductionResult` frame is useful:

```text
FinishReductionResult(parent_reg, step)
EnterReg(result_reg)
```

When `EnterReg(result_reg)` finishes, `FinishReductionResult` consumes the
child result and performs mode-specific call/result/token bookkeeping.

This removes another recursive call from the current evaluator: reduction no
longer calls `incremental_evaluate*` on the closure it just produced.  It
schedules that work on the explicit stack.

## Active-register stack, GC, and cleanup

The explicit evaluator stack must be visible to GC.  It may contain:

- source registers;
- result registers;
- parent registers;
- step ids;
- closures or runtime expressions held by dynamic frames;
- temporary roots created while reducing an operation.

The current GC already treats the active-register stack, temporary heads, and
ordinary heads as roots.  The new evaluator frames need equivalent tracing.  A
frame should not hide a register id inside an opaque object without providing a
trace method or equivalent root enumeration.

Cleanup must also become explicit.  The current C++ stack gives automatic
destruction for `OperationArgs`, which pops temporary heads on destruction.  With
an explicit stack, each operation frame needs enough state to restore
temporary-root depth when the operation finishes or unwinds through an
exception.

The first implementation can use eager mutation with explicit cleanup/unwind
frames.  A full transaction log is probably too large for the initial change.

Active-register push/pop also needs explicit cleanup.  Entering a register sets
`reg_is_on_stack_bit` and pushes the register onto the active-reg stack.
Leaving that register must reset the bit and pop the active-reg stack even when
evaluation exits through an exception.

## Examples

### Strict value operation

Current form:

```c++
extern "C" closure builtin_function_gamma_density(OperationArgs& Args)
{
    double a = Args.evaluate_slot_to_value(0).as_double();
    double b = Args.evaluate_slot_to_value(1).as_double();
    double x = Args.evaluate_slot_to_value(2).as_double();

    return gamma_pdf(a, b, x);
}
```

Prepared form:

```c++
static closure gamma_density_finish(PreparedArgs& Args)
{
    double a = Args.value(0).as_double();
    double b = Args.value(1).as_double();
    double x = Args.value(2).as_double();

    return gamma_pdf(a, b, x);
}

static constexpr prepared_arg_mode gamma_density_plan[] = {
    prepared_arg_mode::use_value,
    prepared_arg_mode::use_value,
    prepared_arg_mode::use_value
};

extern "C" PreparedOperation prepared_builtin_gamma_density {
    "gamma_density",
    gamma_density_plan,
    gamma_density_finish
};
```

The finalizer contains no recursive evaluation.  The evaluator prepares the
three values before calling it.

### Effectful operation

Current form:

```c++
extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);
    auto prob = Args.evaluate_slot_to_value(1).as_log_double();

    int r_prob = Args.reg_for_slot(1);
    r_prob = Args.memory().follow_reg_ref_no_force(r_prob);

    object_ptr<effect> e(new register_prior(r_from_dist, r_prob, prob));
    int r_effect = Args.allocate(closure(e));

    Args.set_effect(r_effect);

    return {R::IndexVar(0), {r_effect}};
}
```

Prepared form:

```c++
static closure register_prior_finish(PreparedArgs& Args)
{
    int r_from_dist = Args.dep_reg(0);
    auto prob = Args.value(1).as_log_double();

    int r_prob = Args.source_reg(1);
    r_prob = Args.memory().follow_reg_ref_no_force(r_prob);

    object_ptr<effect> e(new register_prior(r_from_dist, r_prob, prob));
    int r_effect = Args.allocate(closure(e));

    Args.set_effect(r_effect);

    return closure(R::IndexVar(0), {r_effect});
}

static constexpr prepared_arg_mode register_prior_plan[] = {
    prepared_arg_mode::use_reg,
    prepared_arg_mode::use_value
};

extern "C" PreparedOperation prepared_builtin_register_prior {
    "register_prior",
    register_prior_plan,
    register_prior_finish
};
```

This still allocates and sets an effect.  That is fine: allocation and effect
registration are not the source of C++ stack growth.  Recursive evaluation is.

### Lazy/register-preserving operation

Some operations intentionally avoid evaluating arguments and instead return a
closure that refers to their registers.

Prepared form:

```c++
static closure changeable_apply_finish(PreparedArgs& Args)
{
    int f_reg = Args.source_reg(0);
    int x_reg = Args.source_reg(1);

    return closure(
        R::OperationApp(new modifiable,
                        {R::IndexVar(1), R::IndexVar(0)}),
        {f_reg, x_reg}
    );
}

static constexpr prepared_arg_mode changeable_apply_plan[] = {
    prepared_arg_mode::raw_reg,
    prepared_arg_mode::raw_reg
};

extern "C" PreparedOperation prepared_builtin_changeable_apply {
    "changeable_apply",
    changeable_apply_plan,
    changeable_apply_finish
};
```

`raw_reg` does not push an evaluation frame.  It only records the source
register for the finalizer.

### Core lazy operations

The core lazy operations can also be prepared operations:

- `apply f x` demands the function head as a used closure, then constructs the
  applied closure.
- `case e of alts` demands the scrutinee as a used closure or value, then
  chooses the branch and constructs the branch closure.
- `seq e body` forces `e`, then returns the body closure.
- `let` has no evaluation demands; it allocates bindings and returns the body
  closure.

Converting at least one of these early is important.  Strict arithmetic
operations are good protocol tests, but stack safety for object-language
recursion depends on the path through the lazy control operations.

## Relationship to existing `e_op`

Existing `e_op` functions are already close to prepared operations:

- evaluate all arguments;
- push argument values onto `e_value_stack`;
- call a simple non-recursive function;
- return a `Runtime::Exp` value.

Prepared operations generalize this:

- arguments can be raw, used, forced, value, closure, or unchangeable;
- results can be full `closure`s, not only simple values;
- values can be read from result registers instead of copied to a value stack;
- finalizers can allocate and set effects, as long as they do not recursively
  evaluate.

The existing `e_value_stack` is also a useful comparison point for stack
representation.  It shows that a value stack can work for simple operations.
The prepared-frame design keeps the same no-recursive-evaluation boundary while
making source/dependency/value registers explicit.

## Migration strategy

Do not convert every builtin at once.

The migration has two different goals:

1. Validate the prepared-operation protocol on easy operations.
2. Establish a real stack-safe object-language recursion path.

Easy protocol validation can start with strict kernels that already resemble
`e_op`:

- `Num.cc`
- `Real.cc`
- `Text.cc`
- `Char.cc`
- `Pair.cc`

Many fixed-argument value builtins should also be mostly mechanical:

- `Distribution.cc`
- `Bits.cc`
- `Matrix.cc`
- `Likelihood.cc`
- `LikelihoodSEV.cc`
- parts of `SModel.cc`
- `Alphabet.cc`
- `Vector.cc`
- `Range.cc`
- `IntSet.cc`
- parts of `Alignment.cc`
- parts of `PopGen.cc`
- probability-kernel parts of `SMC.cc`

The first meaningful stack-safety milestone should also convert at least one
core lazy operation such as `apply` or `case`, and route the corresponding
`incremental_evaluate1` path through the explicit engine.

Harder candidates include:

- `Modifiables.cc`, because it manipulates effects and changeable structure;
- `Prelude.cc`, because of IORef/ST-style behavior;
- `IntMap.cc`, where some functions apply user functions or force map contents;
- `MCMC.cc`, because many operations manipulate contexts and proposals;
- proposal-heavy parts of `SMC.cc`.

These harder operations may still be convertible, but they should not drive the
initial design.

Suggested implementation order:

1. Add the explicit evaluator stack types and make GC trace them.
2. Add the shared engine with enough `EnterReg` behavior to handle constants and
   simple references.
3. Add prepared operations and convert a small strict value operation.
4. Convert one core lazy operation such as `apply` or `case`.
5. Route `incremental_evaluate1` through the engine for the converted path.
6. Add deep-recursion tests.  Tests for unimplemented stack safety should be
   marked expected-fail until the engine path handles them.
7. Extend the policy layer and frames until `incremental_evaluate2` can use the
   same engine.
8. Add dynamic operation frames when converting the first operation that needs
   dynamic demands.
9. Remove or shrink legacy recursive `OperationArgs` paths as converted
   operations make them unnecessary.

The old operation path can remain temporarily for arbitrary C++ callbacks.  It
is not stack-safe if the callback recursively evaluates, but it allows gradual
migration.  Code using that path should carry a brief compatibility note saying
why it remains and what needs to happen to remove it.

## Performance notes

The prepared-operation frame should be cheap:

- one frame per active prepared operation;
- one small integer record per argument;
- no copied closures;
- no copied `Runtime::Exp` values unless a finalizer explicitly needs to
  allocate such a value;
- `PreparedArgs` is only a view over the frame.

This can be competitive with the current `e_op` value stack.  The current path
copies or moves values onto `e_value_stack`; the prepared path stores result
registers and lets finalizers read values from the heap by reference.

For tiny arithmetic operations, extra heap indirection may matter.  If profiling
shows this is a problem, specialized fast paths can be added later for common
arities or unboxed values.  The first implementation should prioritize correct
stack behavior and simple frame layout.

## Open questions

- Should `use_value` reject lambdas only in debug mode or always?
- Should `raw_reg` set `value_reg = 0` to catch mistakes, or
  `value_reg = source_reg` for convenience?
- Should the demand mode be one enum, or should demand kind and result shape be
  orthogonal fields?
- Should prepared finalizers be allowed to call advanced heap mutation helpers
  such as `set_call`, `mark_reg_changeable`, and `add_shared_step` directly, or
  should these be wrapped in a narrower API?
- How should old `Operation` and new `PreparedOperation` share loader and
  binding metadata?
- Which operations are hot enough to deserve arity-specialized prepared frames?
- How much of the current `RegOperationArgs1`, `RegOperationArgs2*`, and
  `RegOperationArgsUnchangeable` behavior belongs in policy methods versus in
  prepared-frame code?
- How should exception context be reconstructed from explicit frames?
- Should step-local use/force edges be added before dynamic collection
  operations are converted, or should the first dynamic operation use only the
  existing dependency model?
