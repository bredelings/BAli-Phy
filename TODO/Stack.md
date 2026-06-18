# Moving evaluation off the C++ stack

The current evaluators in `src/computation/machine/evaluate.cc` use the C++
call stack in two ways:

1. Evaluator-internal recursion, such as evaluating a referenced register,
   evaluating the call of a changeable register, or evaluating an argument of
   an operation.
2. Operation callback recursion, where a builtin receives `OperationArgs&` and
   calls back into the evaluator with methods such as
   `evaluate_slot_to_value`, `evaluate_slot_force`, or
   `evaluate_reg_to_closure`.

Haskell-style programs can require stack proportional to heap because ordinary
program recursion is expressed through the object language.  The long-term goal
is to make that stack explicit and managed by the runtime, avoiding C++ stack
overflow without adding unnecessary overhead to common operations.

This note only describes the stack work.  It intentionally does not depend on
storing evaluator kinds on registers, adding `runST`, or JIT compilation.  Those
ideas may reuse this machinery later, but they should remain separate design
problems.

## First constraint: preserve semantics

The initial stack refactor should not change evaluator behavior.

In particular, it should preserve:

- `incremental_evaluate1` semantics.
- `incremental_evaluate2` semantics.
- `incremental_evaluate_unchangeable` semantics.
- Effect registration behavior.
- Step/result/token-delta behavior.
- Force-count behavior.

The first milestone is mechanical: replace evaluator-to-evaluator recursion
with explicit frames where possible.

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

The evaluator cannot generate stack frames for this directly, because it does
not know ahead of time:

- how many arguments the operation will evaluate;
- whether each argument is used or forced;
- whether an argument is evaluated as a value, closure, or register;
- whether the operation will call back into evaluation again later.

The key step is to make this evaluation plan data.

## Prepared operations

A `PreparedOperation` splits an operation into two parts:

1. A declarative argument evaluation plan.
2. A non-recursive finalizer.

The evaluator executes the plan by pushing/resuming explicit frames.  When all
arguments are prepared, it calls the finalizer.  The finalizer may inspect the
heap, allocate registers, set effects, and return a closure, but it must not
call back into evaluation.

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

The exact representation can differ.  For static exported operations, an array
of `prepared_arg_mode` may be easier than a `std::span` initialized from a
temporary.

## Frame storage

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
    small_vector<PreparedArgState, 6> args;
};
```

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
`e_op` pattern of evaluating arguments and copying/moving `Runtime::Exp` values
onto a separate value stack.

## Executing the argument plan

When the evaluator enters a prepared operation:

1. Create a `PreparedOpFrame`.
2. Fill `args[i].source_reg` from the operation slots.
3. Set `next_arg = 0`.
4. Interpret the plan incrementally.

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
push continuation: resume prepared op after argument i
push evaluation frame: evaluate source_reg with USE
```

When the child evaluation completes:

```text
arg.dep_reg = returned.dep_reg
arg.value_reg = returned.value_reg
next_arg++
```

The active evaluator policy determines what "use" means.  For example,
`incremental_evaluate1` and `incremental_evaluate2` record/change bookkeeping
differently.  The prepared operation only declares that this argument is used.

### `force_reg`

This is the same shape as `use_reg`, but the child evaluation request uses force
semantics.

```text
push continuation: resume prepared op after argument i
push evaluation frame: evaluate source_reg with FORCE
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

## Example: strict value operation

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

## Example: effectful operation

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

## Example: lazy/register-preserving operation

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

## Migration strategy

Do not convert every builtin at once.

1. Add `PreparedOperation` alongside existing `Operation`.
2. Make the evaluator recognize and execute `PreparedOperation` frames.
3. Convert existing `simple_function_*` / `e_op` operations first.
4. Convert strict value builtins that only use fixed `evaluate_slot_to_value`
   calls.
5. Convert effectful but non-recursive builtins such as `register_prior` and
   `register_likelihood`.
6. Convert closure/register-sensitive helpers where useful.
7. Leave context/MCMC/proposal-heavy operations on the old `OperationArgs`
   callback path until there is a clear need.

The old operation path remains valid for arbitrary C++ callbacks.  It is not
stack-safe if the callback recursively evaluates, but it allows gradual
migration.

## Likely easy candidates

The easiest functions are strict kernels that already resemble `e_op`:

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

Harder candidates include:

- `Modifiables.cc`, because it manipulates effects and changeable structure;
- `Prelude.cc`, because of IORef/ST-style behavior;
- `IntMap.cc`, where some functions apply user functions or force map
  contents;
- `MCMC.cc`, because many operations manipulate contexts and proposals;
- proposal-heavy parts of `SMC.cc`.

These harder operations may still be convertible, but they should not drive the
initial design.

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
- Should prepared finalizers be allowed to call advanced heap mutation helpers
  such as `set_call`, `mark_reg_changeable`, and `add_shared_step` directly, or
  should these be wrapped in a narrower API?
- How should old `Operation` and new `PreparedOperation` share loader and
  binding metadata?
- Which operations are hot enough to deserve arity-specialized prepared frames?
