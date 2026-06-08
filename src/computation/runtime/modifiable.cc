#include "computation/runtime/modifiable.H"
#include "computation/machine/graph_register.H"
#include "computation/runtime/ast.H"

closure modifiable_op(OperationArgs& Args)
{
    auto& C = Args.current_closure();
    int n_args = C.n_slots();

    // Use the first argument as an initial value.
    if (n_args == 1)
    {
        auto& M = Args.memory();

        // 1. Get the reg x to call.
        int x = Args.reg_for_slot(0);

        // 2. Allocate a new reg m with closure (modifiable).
        int m = Args.allocate( {R::OperationApp(new modifiable, {})} );

        // 3. Mark m changeable.
        M.mark_reg_changeable(m);

        // 4. Mark m unforgettable.
        M.mark_reg_unforgettable(m);

        // 5. Give m a step that calls x.
        int s = M.add_shared_step(m);
        M.set_call(s, x, true);

        // 7. Unchangeably evaluate to m.
        return closure(R::IndexVar(0), {m});
    }
    else if (n_args == 2)
    {
        Args.make_changeable();

        // 1. Get the function and object
        int f_reg = Args.reg_for_slot(0);
        int x_reg = Args.reg_for_slot(1);

        // 2. Allocate the expression to evaluate
        return closure(R::apply(R::IndexVar(1), {R::IndexVar(0)}),
                       {f_reg, x_reg});
    }
    // Complain if there is no value at all.
    else
        throw myexception()<<"Evaluating modifiable with no result.";
}

modifiable::modifiable():
    Operation(modifiable_op, "modifiable")
{}

bool is_modifiable(const Runtime::Exp& E)
{
    const auto* app = E.to<Runtime::OperationApp>();
    if (not app)
        return false;

    return dynamic_cast<const modifiable*>(app->head.get());
}
