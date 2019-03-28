#include "computation/maybe_modifiable.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/modifiable.H"

std::optional<int> maybe_modifiable::is_modifiable(const context& C) const
{
    if (head)
        return C.compute_expression_is_modifiable_reg(*head);
    else
        return {};
}

std::optional<int> maybe_modifiable::is_random_variable(const context& C) const
{
    if (head)
        return C.compute_expression_is_random_variable(*head);
    else
        return {};
}

std::optional<bounds<double>> maybe_modifiable::has_bounds(const context& C) const
{
    if (head and C.compute_expression_has_bounds(*head))
        return C.get_bounds_for_compute_expression(*head);
    else
        return {};
}


expression_ref maybe_modifiable::get_value(const context& C) const
{
    if (head)
        return C.evaluate(*head);
    else
        return *value;
}

void maybe_modifiable::set_value(context& C, const expression_ref& v)
{
    if (value)
    {
        if (v != *value)
            throw myexception()<<"maybe_modifiable::set_value: trying to set constant '"<<*value<<"' to '"<<v<<"'";
    }
    else if (auto r = is_modifiable(C))
        C.set_modifiable_value(*r,v);
    else
        throw myexception()<<"maybe_modifiable::set_value: can't set non-modifiable head to '"<<v<<"'";
}

maybe_modifiable get_maybe_modifiable(context& C, const expression_ref& E)
{
    if (is_modifiable(E))
    {
        auto reg = E.sub()[0];
        if (not is_reg_var(reg))
            throw myexception()<<"get_maybe_modifiable: modifiable expression "<<E<<" should have a reg_var as its object.";

        return C.add_compute_expression(reg);
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


