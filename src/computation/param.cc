#include "computation/param.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/modifiable.H"
#include "computation/context.H"

using std::optional;

expression_ref param::ref(const context& C) const
{
    if (head)
        return C.get_expression(*head);
    else
        return *value;
}

optional<int> param::is_modifiable(const context& C) const
{
    if (head)
        return C.compute_expression_is_modifiable_reg(*head);
    else
        return {};
}

optional<int> param::is_random_variable(const context& C) const
{
    if (head)
        return C.compute_expression_is_random_variable(*head);
    else
        return {};
}

expression_ref param::get_value(const context& C) const
{
    if (head)
    {
        if (strategy == eval_strategy::changeable)
            return C.evaluate(*head);
        else if (strategy == eval_strategy::precomputed)
        {
            auto result = C.precomputed_value_for_head(*head);
            assert(result);
            return result->exp;
        }
        else if (strategy == eval_strategy::unchangeable)
            return C.evaluate_unchangeable(*head);
        else
            std::abort();
    }
    else
        return *value;
}

void param::set_value(context& C, const expression_ref& v) const
{
    if (value)
    {
        if (v != *value)
            throw myexception()<<"param::set_value: trying to set constant '"<<*value<<"' to '"<<v<<"'";
    }
    else if (auto r = is_modifiable(C))
        C.set_modifiable_value(*r,v);
    else
        throw myexception()<<"param::set_value: can't set non-modifiable head to '"<<v<<"'";
}

// for use with maybe_modifiable_structure

param get_param(context& C, const expression_ref& E, eval_strategy s)
{
    if (E.head() == constructor("Modifiable",1))
    {
        auto reg = E.sub()[0];
        if (not is_reg_var(reg))
            throw myexception()<<"get_param: modifiable expression "<<E<<" should have a reg_var as its object.";

        return param(C.add_compute_expression(reg), s);
    }
    else if (is_reg_var(E) or E.size())
    {
        throw myexception()<<"get_param: expression "<<E<<" is neither an atomic constant nor a modifiable";
    }
    else
    {
        return E;
    }
}


expression_ref ConstParam::ref() const
{
    return x.ref(*C);
}

expression_ref ConstParam::get_value() const
{
    return x.get_value(*C);
}

optional<expression_ref> ConstParam::constant_value() const
{
    return x.constant_value();
}

optional<int> ConstParam::is_modifiable() const
{
    return x.is_modifiable(*C);
}

optional<int> ConstParam::is_random_variable() const
{
    return x.is_random_variable(*C);
}

ConstParam::operator bool()
{
    return (bool)x;
}

ConstParam::ConstParam()
{}

ConstParam::ConstParam(param x_, const context* C_)
    :x(x_),C(C_)
{}

expression_ref Param::ref() const
{
    return x.ref(*C);
}

expression_ref Param::get_value() const
{
    return x.get_value(*C);
}

void Param::set_value(const expression_ref& v) const
{
    x.set_value(*C, v);
}

optional<expression_ref> Param::constant_value() const
{
    return x.constant_value();
}

optional<int> Param::is_modifiable() const
{
    return x.is_modifiable(*C);
}

optional<int> Param::is_random_variable() const
{
    return x.is_random_variable(*C);
}

Param::operator bool()
{
    return (bool)x;
}

Param::Param()
{}

Param::Param(param x_, context* C_)
    :x(x_),C(C_)
{}
