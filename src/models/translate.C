#include "translate.H"
#include "parse.H"
#include <vector>
#include <set>
#include "rules.H"
#include "myexception.H"
#include "util.H"

using std::vector;
using std::set;
using std::string;
using boost::property_tree::ptree;

// Translate pass M+FM -> RCTMC[M,FM]
void pass1(ptree& p)
{
    // 1. Handle children.
    for(auto& child: p)
	pass1(child.second);
    
    // 2. Convert e.g. TN+F -> RCTMC[TN,F]
    if (can_unify(get_result_type(p),parse_type("FM[_]")) and p.count("submodel"))
    {
	ptree q = p.get_child("submodel");
	auto& r = p;
	r.erase("submodel");
	
	ptree result = {};
	result.put_value("RCTMC");
	result.push_back({"Q",q});
	result.push_back({"R",r});
	p = result;
    }
}

/// True if t1 is derivedd from
equations_t type_derived_from(const type_t& t1, const type_t& t2)
{
    return unify(t1, t2);
}

/// True if some conversion function can be applied to the expression of type t1, so that it is of type t2
equations_t convertible_to(ptree& model, const type_t& t1, type_t t2)
{
    auto equations = type_derived_from(t1, t2);
    if (equations.get_value<string>() != "fail")
	return equations;

    if (t2.get_value<string>() == "MMM")
    {
	t2.put_value("MM");
	equations = convertible_to(model,t1,t2);
	if (equations.get_value<string>() != "fail")
	{
	    ptree result;
	    result.put_value("MMM");
	    result.push_back({"submodel",model});
	    model = result;
	}
    }
    else if (t2.get_value<string>() == "MM")
    {
	t2.put_value("RA");
	equations = convertible_to(model,t1,t2);
	if (equations.get_value<string>() != "fail")
	{
	    ptree result;
	    result.put_value("UnitMixture");
	    result.push_back({"submodel",model});
	    model = result;
	}
    }
    else if (t2.get_value<string>() == "RA")
    {
	t2.put_value("EM");
	equations = convertible_to(model,t1,t2);
	if (equations.get_value<string>() != "fail")
	{
	    ptree result;
	    result.put_value("RCTMC");
	    result.push_back({"Q",model});
	    result.push_back({"R",ptree("F")});
	    model = result;
	}
    }

    return equations;
}

void pass2(const ptree& required_type, ptree& model, equations_t& equations)
{
    auto name = model.get_value<string>();

    if (can_be_converted_to<int>(name) and (required_type.get_value<string>() == "Int" or required_type.get_value<string>() == "Double"))
    {
	assert(model.empty());
	return;
    }

    if (can_be_converted_to<double>(name) and required_type.get_value<string>() == "Double")
    {
	assert(model.empty());
	return;
    }

    if (required_type.get_value<string>() == "String")
    {
	assert(model.empty());
	return;
    }

    auto rule = require_rule_for_func(name);

    // 1a. Find variables in type and equations.
    set<string> variables_to_avoid = find_variables(required_type);
    add(variables_to_avoid, find_variables(equations));
    for(const auto& eq: equations)
	variables_to_avoid.insert(eq.first);

    // 1b. Find variables in rule type
    set<string> rule_type_variables = find_variables(rule.get_child("result_type"));
    for(const auto& x: rule.get_child("args"))
	add(rule_type_variables, find_variables( x.second.get_child("arg_type") ) );

    // 1c. Make substitutions in rule type
    //    std::cout<<"substituting-from: "<<show(rule)<<std::endl;
    auto renaming = alpha_rename(rule_type_variables, variables_to_avoid);
    substitute(renaming, rule.get_child("result_type") );
    for(auto& x: rule.get_child("args"))
    {
	ptree& arg_type = x.second.get_child("arg_type");
	substitute( renaming, arg_type );
    }
    //    std::cout<<"substituting-to  : "<<show(rule)<<std::endl;

    //	std::cout<<"name = "<<name<<" required_type = "<<unparse_type(required_type)<<"  result_type = "<<unparse_type(result_type)<<std::endl;

    // 2. Skip rules where the type does not match

    type_t result_type = rule.get_child("result_type");
    equations_t equations2 = type_derived_from(result_type, required_type);
    merge_equations(equations2,equations);

    if (equations2.get_value<string>() == "fail")
    {
	equations2 = convertible_to(model, result_type, required_type);
	merge_equations(equations2,equations);
	if (equations2.get_value<string>() != "fail")
	{
	    equations = equations2;
	    pass2(required_type, model, equations);
	    return;
	}
    }
    equations = equations2;
//	if (equations.get_value<string>() == "fail")
//	    std::cout<<"fail!"<<std::endl;
//	else
//	    std::cout<<"OK."<<std::endl;

    if (equations.get_value<string>() == "fail")
	throw myexception()<<"Term '"<<model.get_value<string>()<<"' of type '"<<unparse_type(result_type)<<"' cannot be converted to type '"<<unparse_type(required_type)<<"'";

    // 2.5. Check type of arguments if pass_arguments
    if (rule.get("pass_arguments",false))
    {
	type_t arg_required_type = get_type_for_arg(rule, "*");
	for(auto& child: model)
	    pass2(arg_required_type, child.second, equations);
	return;
    }

    // 3. Handle supplied arguments first.
    for(auto& child: model)
    {
	type_t arg_required_type = get_type_for_arg(rule, child.first);
	substitute(equations, arg_required_type);
	pass2(arg_required_type, child.second, equations);
    }

    // 4. Handle default arguments 
    for(const auto& arg: rule.get_child("args"))
    {
	const auto& argument = arg.second;

	string arg_name = argument.get<string>("arg_name");
	bool arg_is_required = (arg_name != "*");

	if (model.count(arg_name)) continue;

	// If there's no default arg 
	if (not argument.count("default_value"))
	{
	    if (arg_is_required)
		throw myexception()<<"Command '"<<name<<"' missing required argument '"<<arg_name<<"'";
	}
	else
	{
	    auto arg_required_type = argument.get_child("arg_type");
	    substitute(equations, arg_required_type);
	    auto default_arg = argument.get_child("default_value");
	    pass2(arg_required_type, default_arg, equations);
	    model.push_back({arg_name, default_arg});
	}
    }
}

std::pair<ptree,equations_t> translate_model(const string& required_type, const string& model)
{
    auto p = parse(model);
    auto t = parse_type(required_type);
    pass1(p);
    equations_t equations;
    pass2(t, p, equations);
    return {p,equations};
}

