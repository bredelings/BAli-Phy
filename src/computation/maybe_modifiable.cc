#include "computation/maybe_modifiable.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/modifiable.H"

expression_ref maybe_modifiable::get_value(const context* C) const
{
    if (modifiable_reg)
        return C->get_modifiable_value(*modifiable_reg);
    else
        return *value;
}

void maybe_modifiable::set_value(context* C, const expression_ref& v)
{
    if (modifiable_reg)
        C->set_modifiable_value(*modifiable_reg,v);
    else if (v != *value)
        throw myexception()<<"maybe_modifiable::set_value: trying to set constant '"<<*value<<"' to '"<<v<<"'";
}

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


