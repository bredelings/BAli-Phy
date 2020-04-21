#include "modifiable.H"
#include "index_var.H"

closure modifiable_op(OperationArgs& Args)
{
    Args.make_changeable();
    
    auto& C = Args.current_closure();

    // Use the first argument as an initial value.
    if (C.exp.size())
    {
        // We must call the reg instead of just calling its value. This records
        // the dependence on the reg, which maybe changeable, so that if the changeable
        // step is destroyed, we don't end up calling a non-existant reg.
        int r_call = Args.reg_for_slot(0);
        return {index_var(0),{r_call}};
    }
    // Complain if there is no value at all.
    else
        throw myexception()<<"Evaluating modifiable with no result.";
}

modifiable::modifiable():
    Operation(1, modifiable_op, "modifiable")
{}

bool is_modifiable(const expression_ref& E)
{
    bool result = E.head().type() == modifiable_type;
    assert(result == E.head().is_a<modifiable>());
    return result;
}

