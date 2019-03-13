#include "computation/maybe_modifiable.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/modifiable.H"

maybe_modifiable get_maybe_modifiable(const expression_ref& E)
{
    if (is_modifiable(E))
    {
        auto reg = E.sub()[0];
        if (is_reg_var(reg))
            return reg.as_<reg_var>().target;
        throw myexception()<<"get_maybe_modifiable: modifiable expression "<<E<<" should have a reg_var as its object.";
    }
    else if (is_reg_var(E) or E.size())
    {
        throw myexception()<<"get_maybe_modifiable: expression "<<E<<" is neither an atomic constant nor a modifiable";
    }
    else
    {
        return E;
    }
}


