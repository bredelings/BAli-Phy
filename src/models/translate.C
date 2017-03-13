#include "translate.H"
#include "parse.H"
#include <vector>
#include <set>
#include "rules.H"
#include "myexception.H"
#include "util.H"

using std::vector;
using std::set;
using std::map;
using std::string;
using boost::property_tree::ptree;
using boost::optional;

// Translate pass M+FM -> RCTMC[M,FM]
void pass1(ptree& p)
{
    // 1. Handle children.
    for(auto& child: p)
	pass1(child.second);
    
    // 2. Convert e.g. TN+F -> RCTMC[TN,F]
    if (unify(get_result_type(p),parse_type("FM[_]")) and p.count("submodel"))
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

/// True if some conversion function can be applied to the expression of type t1, so that it is of type t2
equations convertible_to(ptree& model, const type_t& t1, type_t t2)
{
    auto E = unify(t1, t2);
    if (E)
	return E;

    if (t2.get_value<string>() == "MMM")
    {
	t2.put_value("MM");
	E = convertible_to(model,t1,t2);
	if (E)
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
	E = convertible_to(model,t1,t2);
	if (E)
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
	E = convertible_to(model,t1,t2);
	if (E)
	{
	    ptree result;
	    result.put_value("RCTMC");
	    result.push_back({"Q",model});
	    result.push_back({"R",ptree("F")});
	    model = result;
	}
    }

    return E;
}

set<string> find_rule_type_vars(const ptree& rule)
{
    set<string> vars = find_variables_in_type(rule.get_child("result_type"));
    for(const auto& x: rule.get_child("args"))
	add(vars, find_variables_in_type( x.second.get_child("arg_type") ) );
    return vars;
}

Rule substitute_in_rule_types(const map<string,term_t>& renaming, Rule rule)
{
    substitute(renaming, rule.get_child("result_type") );
    for(auto& x: rule.get_child("args"))
    {
	ptree& arg_type = x.second.get_child("arg_type");
	substitute( renaming, arg_type );
    }
    return rule;
}

Rule substitute_in_rule_types(const equations& renaming, Rule rule)
{
    substitute(renaming, rule.get_child("result_type") );
    for(auto& x: rule.get_child("args"))
    {
	ptree& arg_type = x.second.get_child("arg_type");
	substitute( renaming, arg_type );
    }
    return rule;
}

Rule freshen_type_vars(Rule rule, const set<string>& bound_vars)
{
    // 1. Find variables in rule type
    set<string> rule_type_variables = find_rule_type_vars(rule);

    // 2. Make substitutions in rule type
    auto renaming = alpha_rename(rule_type_variables, bound_vars);
    return substitute_in_rule_types(renaming, rule);
}

equations pass2(const ptree& required_type, ptree& model, set<string> bound_vars)
{
    auto name = model.get_value<string>();

    if (can_be_converted_to<int>(name))
    {
	if (required_type.get_value<string>() != "Double" and required_type.get_value<string>() != "Int")
	    throw myexception()<<"Can't convert '"<<name<<"' to type '"<<unparse_type(required_type)<<"'";
	assert(model.empty());
	return {};
    }

    if (can_be_converted_to<double>(name))
    {
	if (required_type.get_value<string>() != "Double")
	    throw myexception()<<"Can't convert '"<<name<<"' to type '"<<unparse_type(required_type)<<"'";
	assert(model.empty());
	return {};
    }

    if (name.size()>=2 and name[0] == '"' and name.back() == '"' and required_type.get_value<string>() == "String") 
    {
	assert(model.empty());
	return {};
    }

    // 0. Any variables in required_type must be listed as bound
    assert(includes(bound_vars, find_variables_in_type(required_type)));

    // 1. Get rule with fresh type vars
    auto rule = require_rule_for_func(name);
    rule = freshen_type_vars(rule, bound_vars);

    //	std::cout<<"name = "<<name<<" required_type = "<<unparse_type(required_type)<<"  result_type = "<<unparse_type(result_type)<<std::endl;

    // 2. Unify required type with rule result type
    type_t result_type = rule.get_child("result_type");
    auto E = unify(result_type, required_type);

    // 2.1 Handle unification failure
    if (not E)
    {
	if (convertible_to(model, result_type, required_type))
	    return pass2(required_type, model, bound_vars);
	else
	    throw myexception()<<"Term '"<<model.get_value<string>()<<"' of type '"<<unparse_type(result_type)<<"' cannot be converted to type '"<<unparse_type(required_type)<<"'";
    }

    // 2.3 Update required type and rules with discovered constraints
    substitute_in_rule_types(E, rule);

    // 2.4 Record any new variables that we are using as bound variables
    add(bound_vars, find_rule_type_vars(rule));
    
	// 2.5. Check type of arguments if pass_arguments
    if (rule.get("pass_arguments",false) or rule.get("list_arguments",false))
    {
	for(auto& child: model)
	{
	    type_t arg_required_type = get_type_for_arg(rule, "*");
	    E = E && pass2(arg_required_type, child.second, bound_vars);
	    substitute_in_rule_types(E, rule);
	    add(bound_vars, E.referenced_vars());
	}
	E.eliminate_except(find_variables_in_type(required_type));
	return E;
    }

    // 3. Handle supplied arguments first.
    for(auto& child: model)
    {
	if (get_arg(rule, child.first).get("no_apply",false))
	    throw myexception()<<"Rule for function '"<<rule.get<string>("name")<<"' doesn't allow specifying a value for '"<<child.first<<"'.";
	type_t arg_required_type = get_type_for_arg(rule, child.first);
	E = E && pass2(arg_required_type, child.second, bound_vars);
	substitute_in_rule_types(E, rule);
	add(bound_vars, E.referenced_vars());
    }

    // 4. Handle default arguments 
    for(const auto& arg: rule.get_child("args"))
    {
	const auto& argument = arg.second;

	string arg_name = argument.get<string>("arg_name");
	bool arg_is_required = (arg_name != "*") and not argument.get("no_apply",false);

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
	    auto default_arg = argument.get_child("default_value");
	    E = E && pass2(arg_required_type, default_arg, bound_vars);
	    substitute_in_rule_types(E, rule);
	    add(bound_vars, E.referenced_vars());

	    model.push_back({arg_name, default_arg});
	}
    }

    E.eliminate_except(find_variables_in_type(required_type));
    return E;
}

std::pair<ptree,equations> translate_model(const string& required_type, const string& model)
{
    auto p = parse(model);
    auto t = parse_type(required_type);
    pass1(p);
    auto E = pass2(t, p, find_variables_in_type(t));
    return {p,E};
}

