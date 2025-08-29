#include "reg_var.H"

bool is_reg_var(const expression_ref& E)
{
    bool result = E.head().type() == type_constant::reg_var_type;
    assert(result == E.head().is_a<reg_var>());
    return result;
}

