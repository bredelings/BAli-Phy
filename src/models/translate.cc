#include "translate.H"
#include "parse.H"
#include <vector>
#include <set>
#include "rules.H"
#include "util/myexception.H"
#include <iostream>
#include "setup.H"
#include "util/set.H"

using std::vector;
using std::set;
using std::pair;
using std::map;
using std::string;
using std::optional;

/// True if some conversion function can be applied to the expression of type t1, so that it is of type t2
equations convertible_to(ptree& model, const type_t& t1, type_t t2)
{
    auto E = unify(t1, t2);
    if (E)
	return E;

    if (t2.get_value<string>() == "Double")
    {
	t2.put_value("Int");
	E = convertible_to(model, t1, t2);
	if (E)
	{
	    ptree result;
	    result.put_value("intToDouble");
	    result.push_back({"x",model});
	    model = result;
	}
    }
    else if (t2.get_value<string>() == "MultiMixtureModel")
    {
	t2.put_value("MixtureModel");
	E = convertible_to(model,t1,t2);
	if (E)
	{
	    ptree result;
	    result.put_value("multiMixtureModel");
	    result.push_back({"submodel",model});
	    model = result;
	}
    }
    else if (t2.get_value<string>() == "MixtureModel")
    {
	t2.put_value("RevCTMC");
	E = convertible_to(model,t1,t2);
	if (E)
	{
	    ptree result;
	    result.put_value("unit_mixture");
	    result.push_back({"submodel",model});
	    model = result;
	}
    }
    else if (t2.get_value<string>() == "RevCTMC")
    {
	t2.put_value("ExchangeModel");
	E = convertible_to(model,t1,t2);
	if (E)
	{
	    ptree result;
	    result.put_value("f");
	    result.push_back({"submodel",model});
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
    substitute(renaming, rule.get_child("constraints") );
    for(auto& x: rule.get_child("args"))
    {
	ptree& arg_type = x.second.get_child("arg_type");
	substitute( renaming, arg_type );
    }
    return rule;
}

template <typename Substitution>
void substitute_in_types(const Substitution& renaming, term_t& T)
{
    // maybe skip this if there is not child called "value"?
    substitute(renaming, T.get_child("type") );
    for(auto& x: T.get_child("value"))
	if (not x.second.is_null()) //  // For function[x=null,body=E]
	    substitute_in_types( renaming, x.second );
}

term_t extract_value(const term_t& T)
{
    term_t value = T.get_child("value");
    for(auto& x: value)
	if (not x.second.is_null()) //  // For function[x=null,body=E]
	    x.second = extract_value(x.second);
    return value;
}

term_t valueize(const term_t& T)
{
    term_t t;
    t.value = T.value;
    for(auto& [key,value]: T)
        t.push_back({key,valueize(value)});
    term_t T2;
    T2.push_back({"value",t});
    T2.push_back({"is_default_value",ptree(false)});
    return T2;
}

Rule substitute_in_rule_types(const equations& renaming, Rule rule)
{
    substitute(renaming, rule.get_child("result_type") );
    substitute(renaming, rule.get_child("constraints") );
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

struct tr_name_scope_t
{
    map<string,ptree> identifiers;
    optional<map<string,ptree>> args;
    map<string,ptree> state;
};

set<string> find_type_variables_from_scope(const tr_name_scope_t& scope)
{
    set<string> vars;
    for(auto& [_, type]: scope.identifiers)
	add(vars, find_variables_in_type(type));
    if (scope.args)
        for(auto& [_, type]: *scope.args)
            add(vars, find_variables_in_type(type));
    return vars;
}

optional<ptree> type_for_var_in_scope(const string& name, const tr_name_scope_t& scope)
{
    if (scope.identifiers.count(name))
        return scope.identifiers.at(name);
    else
        return {};
}

optional<ptree> type_for_arg_in_scope(const string& name, const tr_name_scope_t& scope)
{
    if (not scope.args)
        return {};
    else if (scope.args->count(name))
        return scope.args->at(name);
    else
        return {};
}

tr_name_scope_t extend_scope(const tr_name_scope_t& scope, const string& var, const type_t type)
{
    auto scope2 = scope;
    if (scope2.identifiers.count(var))
        scope2.identifiers.erase(var);
    scope2.identifiers.insert({var,type});
    return scope2;
}

// OK, so 'model' is going to have arg=value pairs set, but not necessarily in the right order.
equations pass2(const Rules& R, const ptree& required_type, ptree& model, set<string> bound_vars, const tr_name_scope_t& scope)
{
    // 0a. Any variables in required_type must be listed as bound
    assert(includes(bound_vars, find_variables_in_type(required_type)));
    // 0b. Any type variables in scope must also be listed as bound
    assert(includes(bound_vars, find_type_variables_from_scope(scope)));

    // 1. Get result type and the rule, if there is one.
    type_t result_type;
    optional<Rule> rule;
    if (model.is_a<int>())
	result_type=type_t("Int");
    else if (model.is_a<double>())
	result_type=type_t("Double");
    else if (model.is_a<bool>())
	result_type=type_t("Bool");
    else
    {
	auto name = model.get_value<string>();

	if (name.size()>=2 and name[0] == '"' and name.back() == '"')
	    result_type=type_t("String");
	else if (auto type = type_for_var_in_scope(name, scope))
	    result_type = *type;
	else if (not name.empty() and name[0] == '@')
        {
            auto type = type_for_arg_in_scope(name.substr(1), scope);
            if (not type)
                throw myexception()<<"can't find argument '"<<name<<"'";
	    result_type = *type;
        }
	else if (name == "let")  //let[m=E,F]
	{
	    string var_name = model[0].first;
	    ptree var_exp = model[0].second;
	    ptree body_exp = model[1].second;

	    auto a = get_fresh_type_var(bound_vars);
	    bound_vars.insert(a);

	    // 1. Analyze the body, forcing it to have the required type
	    equations E = pass2(R, required_type, body_exp, bound_vars, extend_scope(scope, var_name, a));
	    if (not E)
		throw myexception()<<"Expression '"<<unparse_annotated(body_exp)<<"' is not of required type "<<unparse_type(required_type)<<"!";
	    add(bound_vars, E.referenced_vars());

	    // 2. Analyze the bound expression with type a
	    substitute(E, a);
	    E = E && pass2(R, a, var_exp, bound_vars, scope);
	    if (not E)
		throw myexception()<<"Expression '"<<unparse_annotated(var_exp)<<"' is not of required type "<<unparse_type(a)<<"!";

	    // Create the new model tree with args in correct order
	    model = ptree("let",{{var_name, var_exp},{"",body_exp}});

	    auto keep = find_variables_in_type(required_type);
	    add(keep, find_type_variables_from_scope(scope));
	    auto S = E.eliminate_except(keep);

	    model = ptree({{"value",model},{"type",required_type}});

	    substitute_in_types(S,model);

	    return E;
	}
	else if (name == "function")  //function[x,F]
	{
	    string var_name = model[0].first;
	    ptree body_exp = model[1].second;

	    auto a = get_fresh_type_var(bound_vars);
	    bound_vars.insert(a);
	    auto b = get_fresh_type_var(bound_vars);
	    bound_vars.insert(b);

	    // 1. Unify required type with (a -> b)
	    auto ftype = ptree("Function",{ {"",a},{"",b} });
	    equations E = unify(ftype, required_type);
	    if (not E)
		throw myexception()<<"Supplying a function, but expected '"<<unparse_type(required_type)<<"!";

            // 2. Analyze the body, forcing it to have type (b)
	    E = E && pass2(R, b, body_exp, bound_vars, extend_scope(scope, var_name, a));
	    if (not E)
		throw myexception()<<"Expression '"<<unparse(body_exp, R)<<"' is not of required type "<<unparse_type(required_type)<<"!";
	    add(bound_vars, E.referenced_vars());

	    // 3. Create the new model tree with args in correct order
	    model = ptree("function",{{var_name, {}},{"",body_exp}});

	    auto keep = find_variables_in_type(required_type);
	    add(keep, find_type_variables_from_scope(scope));
	    auto S = E.eliminate_except(keep);

	    model = ptree({{"value",model},{"type",required_type}});

	    substitute_in_types(S,model);

	    return E;
	}
        else if (name == "get_state")
        {
            string state_name = model[0].second;
            if (not scope.state.count(state_name))
                throw myexception()<<"translate: no state '"<<state_name<<"'!";
            result_type = scope.state.at(state_name);
            auto E = unify(result_type, required_type);
            if (not E)
                throw myexception()<<"get_state: state '"<<state_name<<"' is of type '"<<unparse_type(result_type)<<"', not required type '"<<unparse_type(required_type)<<"'";

            auto arg = ptree({{"value",ptree(state_name)},{"type","String"}});
            // ARGH: arrays with ptree are really annoying.
            model = ptree("get_state",{ {"",arg}});

            auto keep = find_variables_in_type(required_type);
            add(keep, find_type_variables_from_scope(scope));
            auto S = E.eliminate_except(keep);

            model = ptree({{"value",model},{"type",required_type}});
            substitute_in_types(S, model);

            return E;
        }
	else
	{
	    rule = R.require_rule_for_func(name);
	    rule = freshen_type_vars(*rule, bound_vars);
            // Record any new variables that we are using as bound variables
            add(bound_vars, find_rule_type_vars(*rule));

	    //	std::cout<<"name = "<<name<<" required_type = "<<unparse_type(required_type)<<"  result_type = "<<unparse_type(result_type)<<std::endl;

	    result_type = rule->get_child("result_type");
	}
    }

    // 2. Unify required type with rule result type
    auto E = unify(result_type, required_type);
    if (rule)
	for(const auto& constraint: rule->get_child("constraints"))
	    E.add_constraint(constraint.second);
    
    // 3. Attempt a conversion if the result_type and the required_type don't match.
    if (not E)
    {
	if (convertible_to(model, result_type, required_type))
	    return pass2(R, required_type, model, bound_vars, scope);
	else
	    throw myexception()<<"Term '"<<unparse(model, R)<<"' of type '"<<unparse_type(result_type)
			       <<"' cannot be converted to type '"<<unparse_type(required_type)<<"'";
    }

    // 4. If this is a constant or variable, then we are done here.
    if (not rule)
    {
	if (not model.empty())
	    throw myexception()<<"Term '"<<model.value<<"' of type '"<<unparse_type(result_type)
			       <<"' should not have arguments!";

	model = ptree({{"value",model},{"type",result_type}});

	return E;
    }

    // Use the canonical name for the function
    model.value = rule->get<string>("name");

    // 5.1 Update required type and rules with discovered constraints
    rule = substitute_in_rule_types(E, *rule);

    // Create the new model tree with args in correct order
    auto name = model.get_value<string>();
    ptree model2(name);

    // 6. Check that we didn't supply unreferenced arguments.
    map<string,int> arg_count;
    for(const auto& supplied_arg: model)
    {
	string arg_name = supplied_arg.first;
	if (not maybe_get_arg(*rule, arg_name))
	    throw myexception()<<"Function '"<<name<<"' has no argument '"<<arg_name<<"' in term:\n"<<model.show();
	arg_count[arg_name]++;
	if (arg_count[arg_name] > 1)
	    throw myexception()<<"Supplied argument '"<<arg_name<<"' more than once in term:\n"<<model.show();
    }

    map<string,ptree> arg_env;
    for(const auto& [_, argument]: rule->get_child("args"))
    {
	string arg_name = argument.get<string>("arg_name");

	auto arg_required_type = argument.get_child("arg_type");
        arg_env.insert({arg_name, arg_required_type});
    }

    // 7. Handle arguments in rule order
    int skip=0;
    for(const auto& arg: rule->get_child("args"))
    {
	skip++;
	const auto& argument = arg.second;

	string arg_name = argument.get<string>("arg_name");

	auto arg_required_type = argument.get_child("arg_type");
	substitute(E, arg_required_type);
	ptree arg_value;
	bool is_default = false;
	if (model.count(arg_name))
	    arg_value = model.get_child(arg_name);
	else if (argument.count("default_value"))
	{
	    is_default = true;
	    arg_value = argument.get_child("default_value");
	}
	else
	    throw myexception()<<"Command '"<<name<<"' missing required argument '"<<arg_name<<"'";

        auto scope2 = scope;
        if (is_default)
            scope2.args = arg_env;

        auto orig_arg_value = arg_value;
	E = E && pass2(R, arg_required_type, arg_value, bound_vars, scope2);
	if (not E)
	    throw myexception()<<"Expression '"<<unparse(orig_arg_value, R)<<"' is not of required type "<<unparse_type(arg_required_type)<<"!";
	for(auto& x: argument)
	    arg_value.push_back(x);
	arg_value.push_back({"is_default_value",ptree(is_default)});
	model2.push_back({arg_name, arg_value});
	add(bound_vars, E.referenced_vars());
    }

    model = model2;

    auto keep = find_variables_in_type(required_type);
    add(keep, find_type_variables_from_scope(scope));
    auto S = E.eliminate_except(keep);

    model = ptree({{"value",model},{"type",result_type}});
    if (rule->get("no_log",false))
	model.push_back({"no_log",ptree(true)});
    if (auto extract = rule->get_child_optional("extract"))
	model.push_back({"extract",*extract});

    substitute_in_types(S, model);

    return E;
}

std::pair<ptree,equations> translate_model(const Rules& R, const ptree& required_type, ptree model,
                                           const map<string,term_t>& scope,
                                           const map<string,term_t>& state)
{
    tr_name_scope_t scope2;
    scope2.identifiers = scope;
    scope2.state = state;
    auto E = pass2(R, required_type, model, find_variables_in_type(required_type), scope2);
    return {model,E};
}

