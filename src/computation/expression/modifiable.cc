#include "random_variable.H"
#include "expression.H"
#include "modifiable.H"

closure modifiable_op(OperationArgs& Args)
{
    Args.make_changeable();
    
    auto& C = Args.current_closure();

    // Use the first argument as an initial value.
    if (C.exp.size())
    {
	// Should we record a force (but no dependence) on this, in order to force the distribution parameters?
	int r_value = Args.evaluate_slot_no_record(0);
	return {index_var(0),{r_value}};
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

