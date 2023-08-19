#include "computation/param.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/var.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"
#include "computation/context.H"
#include "computation/machine/graph_register.H"

#include "util/log-level.H"
#include "computation/machine/gcobject.H"

using std::optional;
using std::vector;

expression_ref param::ref(const context_ref& C) const
{
    if (reg)
        return reg_var(*reg);
    else if (head)
        return C.get_expression(*head);
    else
        return *value;
}

optional<int> param::is_modifiable(const context_ref& C) const
{
    if (reg)
    {
        if (C.find_modifiable_reg(*reg))
            return *reg;
        else
            return {};
    }
    // FIXME - this doesn't search for the modifiable.
    else if (head)
        return C.compute_expression_is_modifiable_reg(*head);
    else
        return {};
}

expression_ref param::get_value(const context_ref& C) const
{
    if (reg)
    {
        if (strategy == eval_strategy::changeable)
            return C.evaluate_reg(*reg);
        else if (strategy == eval_strategy::precomputed)
        {
            auto result = C.precomputed_value_for_reg(*reg);
            assert(result);
            return result->exp;
        }
        else if (strategy == eval_strategy::unchangeable)
            std::abort();
        else
            std::abort();
    }
    else if (head)
    {
        if (strategy == eval_strategy::changeable)
            return C.evaluate_head(*head);
        else if (strategy == eval_strategy::precomputed)
        {
            auto result = C.precomputed_value_for_head(*head);
            assert(result);
            return result->exp;
        }
        else if (strategy == eval_strategy::unchangeable)
            return C.evaluate_head_unchangeable(*head);
        else
            std::abort();
    }
    else
        return *value;
}

void param::set_value(context_ref& C, const expression_ref& v) const
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

context_ptr context_ptr::operator[](int i) const
{
    int r = C.memory()->value_for_reg(reg);
    auto& c = C.memory()->closure_at(r);
    if (auto& e = c.exp; e.size() == 0 and is_gcable_type(e.type()))
    {
        if (auto im = e.to<IntMap>())
            r = (*im)[i];
        else
            std::abort();
    }
    else
        r = c.reg_for_slot(i);
    return {C, r};
}

context_ptr context_ptr::list_element(int index) const
{
    context_ptr L = result();
    int i=0;
    for(;L.size() > 0 and i < index;i++)
    {
        assert(L.size() == 2);
        L = L[1];
    }
    if (i < index)
        throw myexception()<<"Trying to get list element "<<index<<" for a list of size "<<i<<"!";

    assert(L.size() == 2);

    // If we return the result here, then we can't find a modifiable!
    return L[0];
}

expression_ref context_ptr::value() const
{
    return result().head();
}

EVector context_ptr::list_to_vector() const
{
    object_ptr<EVector> vec(new EVector);

    context_ptr L = result();
    while(L.size() > 0)
    {
        assert(L.size() == 2);

        vec->push_back(L[0].value());

        L = L[1];
    }

    return std::move(*vec);
}

vector<context_ptr> context_ptr::list_elements() const
{
    vector<context_ptr> elements;

    context_ptr L = result();
    while(L.size() > 0)
    {
        assert(L.size() == 2);

        elements.push_back(L[0]);

        L = L[1];
    }

    return elements;
}

context_ptr context_ptr::result() const
{
    auto cp2 = *this;
    cp2.move_to_result();
    return cp2;
}

optional<context_ptr> context_ptr::modifiable() const
{
    auto cp2 = *this;
    if (cp2.move_to_modifiable())
        return cp2;
    else
        return {};
}

void context_ptr::set_value(const expression_ref& v)
{
    auto m = modifiable();
    if (not m)
        throw myexception()<<"Trying to set the value of non-modifiable reg "<<get_reg();
    int r = m->get_reg();
    const_cast<context_ref&>(C).set_reg_value(r,v);
}

void context_ptr::move_to_result()
{
    reg = C.memory()->value_for_reg(reg);
    assert(reg != -1);
}

bool context_ptr::move_to_modifiable()
{
    auto mod_reg = C.find_modifiable_reg(reg);
    if (mod_reg)
    {
        reg = *mod_reg;
        return true;
    }
    else
        return false;
}

int context_ptr::size() const
{
    int r = C.memory()->value_for_reg(reg);
    return C.memory()->expression_at(r).size();
}

expression_ref context_ptr::head() const
{
    int r = C.memory()->value_for_reg(reg);
    return C.memory()->expression_at(r).head();
}

context_ptr::context_ptr(const context_ref& c, int i)
    :C(c)
{
    auto [r1,_] = C.incremental_evaluate(i);
    reg = r1;
}

