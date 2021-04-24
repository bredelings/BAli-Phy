#include "computation/param.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/var.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"
#include "computation/context.H"
#include "computation/machine/graph_register.H"

#include "util/log-level.H"

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

// for use with maybe_modifiable_structure

param get_param(context_ref& C, const expression_ref& E, eval_strategy s)
{
    if (E.head() == constructor("Modifiable",1))
    {
        auto reg = E.sub()[0];
        if (not is_reg_var(reg))
            throw myexception()<<"get_param: modifiable expression "<<E<<" should have a reg_var as its object.";

        return param(reg.as_<reg_var>(), s);
    }
    else if (is_reg_var(E))
    {
        return param(E.as_<reg_var>(), s);
    }
    else if (E.size())
    {
        throw myexception()<<"get_param: expression "<<E<<" is neither an atomic constant nor a modifiable";
    }
    else
    {
        return E;
    }
}

vector<param> get_params_from_list_(context_ref& C, const expression_ref& structure, optional<int> check_size)
{
    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    vector<param> params;

    auto vec = *list_to_evector(structure);
    for(auto& e: vec)
        params.push_back( get_param(C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected a list of length "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

vector<param> get_params_from_list(context_ref& C, const expression_ref& list, optional<int> check_size)
{
    expression_ref structure = C.evaluate_expression({var("Parameters.maybe_modifiable_structure"), list});

    return get_params_from_list_(C, structure, check_size);
}

vector<param> get_params_from_array_(context_ref& C, const expression_ref& structure, optional<int> check_size)
{
    vector<param> params;

    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    for(auto& e: structure.sub())
        params.push_back( get_param(C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected an array "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

vector<param> get_params_from_array(context_ref& C, const expression_ref& array, optional<int> check_size)
{
    expression_ref structure = C.evaluate_expression({var("Parameters.maybe_modifiable_structure"), array});

    return get_params_from_array_(C, structure, check_size);
}

context_ptr context_ptr::operator[](int i) const
{
    int r = C.memory()->closure_at(reg).reg_for_slot(i);
    return {C, r};
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
    return C.memory()->expression_at(reg).size();
}

expression_ref context_ptr::head() const
{
    return C.memory()->expression_at(reg).head();
}

context_ptr::context_ptr(const context_ref& c, int i)
    :C(c)
{
    auto [r1,_] = C.incremental_evaluate(i);
    reg = r1;
}

