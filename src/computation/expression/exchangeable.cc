#include "exchangeable.H"
#include "index_var.H"

closure exchangeable_op(OperationArgs& Args)
{
    Args.make_changeable();
    
    auto& C = Args.current_closure();

    assert(C.exp.size() == 2);
    
    int f_reg = Args.reg_for_slot(0);
    int x_reg = Args.reg_for_slot(1);

    expression_ref f = index_var(1);
    expression_ref x = index_var(0);
    expression_ref E = {f, x};

    closure C2(E,{f_reg,x_reg});

    int r_call = Args.allocate( std::move(C2) );
    return {index_var(0),{r_call}};
}

exchangeable::exchangeable():
    Operation(2, exchangeable_op, "exchangeable")
{}

bool is_exchangeable(const expression_ref& E)
{
    bool result = E.head().type() == exchangeable_type;
    assert(result == E.head().is_a<exchangeable>());
    return result;
}

