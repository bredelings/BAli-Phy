#include "reg_var.H"

bool is_reg_var(const expression_ref& E)
{
    return E.is_a<reg_var>();
}

