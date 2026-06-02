#include "modifiable.H"
#include "index_var.H"
#include "computation/machine/graph_register.H"

closure modifiable_op(OperationArgs& Args)
{
    auto& C = Args.current_closure();

    // Use the first argument as an initial value.
    if (C.exp.size() == 1)
    {
        auto& M = Args.memory();

        // 1. Get the reg x to call.
        int x = Args.reg_for_slot(0);

        // 2. Allocate a new reg m with closure (modifiable).
        int m = Args.allocate( {modifiable(),{}} );

        // 3. Mark m changeable.
        M.mark_reg_changeable(m);

        // 4. Mark m unforgettable.
        M.mark_reg_unforgettable(m);

        // 5. Give m a step that calls x.
        int s = M.add_shared_step(m);
        M.set_call(s, x, true);

        // 7. Unchangeably evaluate to m.
        return closure(Runtime::IndexVar(0), {m});
    }
    else if (C.exp.size() == 2)
    {
        Args.make_changeable();

        // 1. Get the function and object
        int f_reg = Args.reg_for_slot(0);
        int x_reg = Args.reg_for_slot(1);

        // 2. Allocate the expression to evaluate
        return closure(Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}),
                       {f_reg, x_reg});
    }
    // Complain if there is no value at all.
    else
        throw myexception()<<"Evaluating modifiable with no result.";
}

modifiable::modifiable():
    Operation(modifiable_op, "modifiable")
{}

bool is_modifiable(const expression_ref& E)
{
    bool result = E.head().type() == type_constant::modifiable_type;
    assert(result == E.head().is_a<modifiable>());
    return result;
}
