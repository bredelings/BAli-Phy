#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/bool.H"
#include "util/myexception.H"
#include <algorithm>
#include "vector_from_list.H"
#include "util/json.hh"
#include <string_view>

using std::string;
using std::string_view;

expression_ref deep_eval_translate_list(OperationArgs& Args, int r)
{
    // 1. If this is a list then return an EVector
    if (auto list_regs = maybe_get_regs_for_list(Args,r))
    {
	EVector v;
	for(int r2: *list_regs)
	    v.push_back(deep_eval_translate_list(Args,r2));
	return v;
    }

    // 2. If this is NOT a list, then deep_eval the structure components
    closure C1 = Args.evaluate_reg_to_closure(r);
    expression_ref E1 = deindexify(trim_unnormalize(C1));

    if (E1.is_atomic())
	return E1;

    std::unique_ptr<expression> E (new expression(E1.as_expression()));

    // Having finished with C, it is now safe to do evaluation.
    for(auto& e: E->sub)
    {
	if (e.is_index_var())
	{
	    int r2 = e.as_index_var();
	    e = deep_eval_translate_list(Args, r2);
	}
	else if (e.is_a<reg_var>())
	{
	    int r2 = e.as_<reg_var>().target;
	    e = deep_eval_translate_list(Args, r2);
	}
    }

    // Steal the already allocated expression instead of making a copy.
    return expression_ref(std::move(E));
}


string to_c_string(const EVector& es)
{
    string s;
    s.resize(es.size());
    for(int i=0;i<s.size();i++)
	s[i] = es[i].as_char();
    return s;
}


json to_c_json(const expression_ref& E)
{
    string_view name = E.head().as_<constructor>().f_name;
    assert(name.substr(0,10) == "Data.JSON.");
    name = name.substr(10);

    if (name == "Null")
    {
	return {};
    }
    else if (name == "Number")
    {
	auto& n = E.sub()[0];
	if (n.is_double())
	    return n.as_double();
	else if (n.is_int())
	    return n.as_int();
	else if (n.is_log_double())
	    return (double)n.as_log_double();
    }
    else if (name == "Bool")
    {
	auto& b = E.sub()[0];
	if (is_bool_true(b))
	    return true;
	else if (is_bool_false(b))
	    return false;
    }
    else if (name == "String")
        return E.sub()[0].as_<String>();
    else if (name == "Array")
    {
	auto& a = E.sub()[0];

	json array;
	for(auto& e: a.as_<EVector>())
	    array.push_back(to_c_json(e));
	return array;
    }
    else if (name == "Object")
    {
	auto& o = E.sub()[0];

	json object;
	for(auto& e: o.as_<EVector>())
	{
	    assert(has_constructor(e,"(,)"));
	    string key  = to_c_string(e.sub()[0].as_<EVector>());
	    json value  = to_c_json(e.sub()[1]);
	    object[key] = value;
	}
	return object;
    }
    throw myexception()<<"Can't translate "<<E<<" into JSON!";
}


extern "C" closure builtin_function_c_json(OperationArgs& Args)
{
    int r = Args.reg_for_slot(0);

    Box<json> J = to_c_json(deep_eval_translate_list(Args, r));
    return (const Object&)J;
}
