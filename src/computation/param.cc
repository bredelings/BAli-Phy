#include "computation/param.H"
#include "computation/expression/constructor.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/var.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"
#include "computation/context.H"

#include "util/log-level.H"

using std::optional;
using std::vector;

expression_ref param::ref(const context_ref& C) const
{
    if (head)
        return C.get_expression(*head);
    else
        return *value;
}

optional<int> param::is_modifiable(const context_ref& C) const
{
    if (head)
        return C.compute_expression_is_modifiable_reg(*head);
    else
        return {};
}

expression_ref param::get_value(const context_ref& C) const
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

vector<param> get_params_from_list(context_ref& C, const expression_ref& list, std::optional<int> check_size)
{
    vector<param> params;
    expression_ref structure = C.evaluate_expression({var("Parameters.maybe_modifiable_structure"), list});

    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    auto vec = *list_to_evector(structure);
    for(auto& e: vec)
        params.push_back( get_param(C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected a list of length "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

vector<param> get_params_from_array(context_ref& C, const expression_ref& array, std::optional<int> check_size)
{
    vector<param> params;
    expression_ref structure = C.evaluate_expression({var("Parameters.maybe_modifiable_structure"), array});

    if (log_verbose >= 3)
        std::cerr<<"structure = "<<structure<<"\n\n";

    for(auto& e: structure.sub())
        params.push_back( get_param(C, e) );

    if (check_size and (params.size() != *check_size))
        throw myexception()<<"Expected an array "<<*check_size<<", but got one of length "<<params.size()<<"!";

    return params;
}

