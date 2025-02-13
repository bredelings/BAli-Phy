#include "typecheck.H"
#include "parse.H"
#include <vector>
#include <set>
#include "rules.H"
#include "util/myexception.H"
#include <iostream>
#include "compile.H"
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

    auto [head2,args2] = get_type_apps(t2);

    if (head2 == "Double")
    {
	t2 = ptree("Int");
	E = convertible_to(model, t1, t2);
	if (E)
	{
	    ptree result;
	    result.put_value("intToDouble");
	    result.push_back({"x",model});
	    model = result;
	}
    }
    // * -> DiscreteDistribution<*>
    else if (head2 == "DiscreteDist" and args2.size() == 1)
    {
	auto [head3,args3] = get_type_apps(args2[0]);
	if (head3 == "CTMC")
	{
            // CTMC<a> -> DiscreteDist<CTMC<a>>
	    auto a = args3[0];
	    t2 = make_type_app("CTMC",a);

	    E = convertible_to(model,t1,t2);
	    if (E)
	    {
		ptree result;
		result.put_value("unit_mixture");
		result.push_back({"submodel",model});
		model = result;
	    }
	}
	else
	{
	    // List<(a,Double)> -> DiscreteDist<a>
	    auto a = args2[0];
	    t2 = make_type_app("List", make_type_apps("Tuple",{a,"Double"}));

	    E = convertible_to(model, t1, t2);
	    if (E)
	    {
		ptree result;
		result.put_value("discrete");
		result.push_back({"pairs",model});
		model = result;
	    }
	}
    }
    // * -> Distribution<*>
    else if (head2 == "Distribution" and args2.size() == 1)
    {
	auto [head1,args1] = get_type_apps(t1);
        // DiscreteDist<a> -> Distribution<a>
	if (head1 == "DiscreteDist" and args1.size() == 1)
        {
	    E = convertible_to(model,args1[0],args2[0]);
	    if (E)
	    {
		ptree result;
		result.put_value("convertDiscrete");
		result.push_back({"x",model});
		model = result;
	    }
        }
    }
    else if (head2 == "MultiMixtureModel" and args2.size() == 1)
    {
	auto a = args2[0];
	t2 = make_type_app("DiscreteDist",make_type_app("CTMC",a));

	E = convertible_to(model,t1,t2);
	if (E)
	{
	    ptree result;
	    result.put_value("multiMixtureModel");
	    result.push_back({"submodel",model});
	    model = result;
	}
    }
    else if (head2 == "CTMC" and args2.size() == 1)
    {
	auto a = args2[0];
	t2 = make_type_app("ExchangeModel", a);
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
	add(vars, find_variables_in_type( x.second.get_child("type") ) );
    return vars;
}

Rule substitute_in_rule_types(const map<string,term_t>& renaming, Rule rule)
{
    substitute(renaming, rule.get_child("result_type") );
    substitute(renaming, rule.get_child("constraints") );
    for(auto& x: rule.get_child("args"))
    {
	ptree& arg_type = x.second.get_child("type");
	substitute( renaming, arg_type );
    }
    return rule;
}

template <typename Substitution>
void substitute_in_types(const Substitution& renaming, term_t& T)
{
    // maybe skip this if there is not child called "value"?
    substitute(renaming, T.get_child("type") );
    auto value = T.get_child("value");
    for(auto& x: value)
        substitute_in_types( renaming, x.second );
}

term_t extract_value(const term_t& T)
{
    auto value = T.get_child("value");
    for(auto& x: value)
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
	ptree& arg_type = x.second.get_child("type");
	substitute( renaming, arg_type );
    }
    return rule;
}

Rule freshen_type_vars(Rule rule, const FVSource& fv_state)
{
    // 1. Find variables in rule type
    set<string> rule_type_variables = find_rule_type_vars(rule);

    // 2. Make substitutions in rule type
    auto renaming = alpha_rename(rule_type_variables, fv_state);
    return substitute_in_rule_types(renaming, rule);
}

ptree convert_to(const equations& eqs, ptree model, type_t type, type_t required_type)
{
    substitute(eqs, type);
    substitute(eqs, required_type);
    if (not (eqs && convertible_to(model, type, required_type)))
    {
	throw myexception()<<"Term '"<<unparse(model)<<"' of type '"<<unparse_type(type)
			   <<"' cannot be converted to type '"<<unparse_type(required_type)<<"'";
    }

    return model;
}

optional<ptree> TypecheckingState::unify_or_convert(const ptree& model, const type_t& type, const type_t& required_type) const
{
    auto tmp_eqs = eqs && unify(type, required_type);
    if (tmp_eqs)
    {
	eqs = tmp_eqs;
	return {};
    }
    else
	return convert_to(eqs, model, type, required_type);
}

TypecheckingState TypecheckingState::copy_no_equations() const
{
    auto scope2 = *this;
    scope2.eqs.clear();
    return scope2;
}

set<string> TypecheckingState::find_type_variables() const
{
    set<string> vars;
    for(auto& [_, type]: identifiers)
	add(vars, find_variables_in_type(type));

    if (args)
        for(auto& [_, type]: *args)
            add(vars, find_variables_in_type(type));
    return vars;
}

optional<ptree> TypecheckingState::type_for_var(const string& name) const
{
    if (identifiers.count(name))
        return identifiers.at(name);
    else
        return {};
}

optional<ptree> TypecheckingState::type_for_arg(const string& name) const
{
    if (not args)
        return {};
    else if (args->count(name))
        return args->at(name);
    else
        return {};
}

void TypecheckingState::extend_scope(const string& var, const type_t type)
{
    if (identifiers.count(var))
        identifiers.erase(var);
    identifiers.insert({var,type});
}

TypecheckingState TypecheckingState::extended_scope(const string& var, const type_t type) const
{
    auto scope = *this;
    scope.extend_scope(var,type);
    return scope;
}

set<string> get_used_args(const ptree& model)
{
    set<string> used_args;
    for(auto& [_,used_arg]: model.get_child("used_args"))
        used_args.insert(used_arg.get_value<string>());
    return used_args;
}

void set_used_args(ptree& model, const set<string>& used_args)
{
    ptree p_used_args;
    for(auto& used_arg: used_args)
        p_used_args.push_back({"",ptree(used_arg)});
    if (auto p = model.get_child_optional("used_args"))
        *p = p_used_args;
    else
        model.push_back({"used_args",p_used_args});
}

ptree TypecheckingState::typecheck_and_annotate_decls(const ptree& decls)
{
    set<string> used_args;
    ptree decls2("!Decls");
    
    for(auto& [name, exp]: decls)
    {
	auto a = get_fresh_type_var("a");
	extend_scope(name,a);
	auto exp2 = typecheck_and_annotate(a, exp);
	add(used_args, get_used_args(exp2));
	if (not eqs)
	{
	    substitute(eqs,a);
	    throw myexception()<<"Expression '"<<unparse_annotated(exp2)<<"' is not of required type "<<unparse_type(a)<<"!";
	}
	decls2.push_back({name,exp2});
    }
    return decls2;
}


optional<ptree>
TypecheckingState::typecheck_and_annotate_let(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    if (name != "!let") return {}; //let[m=E,F]

    ptree decls = model[0].second;
    ptree body  = model[1].second;

    set<string> used_args;

    // 1. Analyze the decls
    auto scope2 = *this;
    auto decls2 = scope2.typecheck_and_annotate_decls(decls);
    for(auto& [name,exp]: decls2)
	add(used_args, get_used_args(exp));

    // 2. Analyze the body, forcing it to have the required type
    auto body2 =  scope2.typecheck_and_annotate(required_type, body);
    add(used_args, get_used_args(body2));
    eqs = scope2.eqs;

    // Create the new model tree with args in correct order
    auto model2 = ptree("!let",{{"decls", decls2},{"body",body2}});
    model2 = ptree({{"value",model2},{"type",required_type}});
    set_used_args(model2, used_args);

    return model2;
}

pair<ptree, map<string,ptree>> TypecheckingState::parse_pattern(const ptree& pattern) const
{
    if (is_nontype_variable(pattern))
    {
        auto type = get_fresh_type_var("p");
        return {type,{{string(pattern),type}}};
    }
    else if (is_tuple(pattern))
    {
        ptree type("Tuple");
        map<string,ptree> var_to_type;
        for(auto& [_,value]: pattern)
        {
            auto [slot_type, slot_vars] = parse_pattern(value);
            type.push_back(pair(string(""),slot_type));
            for(auto& [var_name,var_type]: slot_vars)
            {
                if (var_to_type.count(var_name))
                    throw myexception()<<"Variable '"<<var_name<<"' occurs twice in "<<unparse(pattern)<<"!";
                var_to_type.insert({var_name,var_type});
            }
        }
        return {type,var_to_type};
    }
    else
        std::abort();
}


optional<ptree>
TypecheckingState::typecheck_and_annotate_lambda(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    if (name != "function") return {}; //function[x,F]

    // OK, to parse a pattern, we need to
    // - find all the variables
    // - give each variable a type
    // - construct a type for the whole thing
    // This could return a pair<type,map<name,type_var>>
    // - we need to enforce that variable names are not duplicated within the pattern.
    ptree pattern  = model[0].second;
    ptree body_exp = model[1].second;

    // 0. Compute the type (a -> b) of the function.

    // This generates fresh type variables and adds them to fv_state.
    auto [a, type_for_binder] = parse_pattern(model[0].second);

    auto scope2 = *this;
    for(auto& [var,type]: type_for_binder)
        scope2.extend_scope(var, type);

    auto b = get_fresh_type_var("b");

    // 1. Unify required type with (a -> b)
    auto ftype = make_type_apps("Function",{a,b});
    eqs = eqs && unify(ftype, required_type);
    if (not eqs)
        throw myexception()<<"Supplying a function, but expected '"<<unparse_type(required_type)<<"!";

    // 2. Analyze the body, forcing it to have type (b)
    if (auto btype = eqs.value_of_var(b))
        b = *btype;

    auto body_exp2 =  scope2.typecheck_and_annotate(b, body_exp);
    eqs = eqs && scope2.eqs;
    auto used_args = get_used_args(body_exp2);
    if (not eqs)
        throw myexception()<<"Expression '"<<unparse(model)<<"' is not of required type "<<unparse_type(required_type)<<"!";

    // 3. Create the new model tree with args in correct order
    auto pattern2 = scope2.typecheck_and_annotate(a, pattern);
    auto model2 = ptree("function",{{"",pattern2},{"",body_exp2}});

    model2 = ptree({{"value",model2},{"type",required_type}});
    set_used_args(model2, used_args);

    return model2;
}

optional<ptree>
TypecheckingState::typecheck_and_annotate_tuple(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    if (name != "Tuple") return {}; //Tuple(x,y,z,...)

    // 1. Unify required type with Tuple(a,b,c,...)

    vector<ptree> element_types;
    for(int i=0;i<model.size();i++)
    {
        auto a = get_fresh_type_var("a");
        element_types.push_back(a);
    }
    auto tuple_type = make_type_apps("Tuple",element_types);

    eqs = eqs && unify(tuple_type, required_type);
    if (not eqs)
        throw myexception()<<"Supplying a function, but expected '"<<unparse_type(required_type)<<"!";

    // 2. Analyze the body, forcing it to have type (b)
    set<string> used_args;
    auto model2 = ptree("Tuple",{});
    for(int i=0;i<model.size();i++)
    {
        auto element = array_index(model,i);
        auto element_required_type = element_types[i];
        substitute(eqs, element_required_type);
        auto element2 =  typecheck_and_annotate(element_required_type, element);
        add(used_args, get_used_args(element2));
        if (not eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(element2)<<"' is not of required type "<<unparse_type(element_required_type)<<"!";
        element2.push_back({"is_default_value",ptree(false)}); // Do we need to add this annotation?
        model2.push_back({"",element2});
    }

    // 3. Create the new model tree with args in correct order
    model2 = ptree({{"value",model2},{"type",required_type}});
    set_used_args(model2, used_args);

    return model2;
}

optional<ptree>
TypecheckingState::typecheck_and_annotate_list(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    if (name != "List") return {}; //List[x,y,z,...]

    // 1. Unify required type or add conversion function.
    auto a = get_fresh_type_var("a");
    auto list_type = make_type_app("List",a);
    if (auto model2 = unify_or_convert(model, list_type, required_type))
	return typecheck_and_annotate(required_type, *model2);

    // 2. Analyze the body, forcing it to have type (b)
    set<string> used_args;
    auto model2 = ptree("List",{});
    for(auto& [_,element]: model)
    {
        auto element_required_type = a;
        substitute(eqs, element_required_type);
        auto element2 =  typecheck_and_annotate(element_required_type, element);
        add(used_args, get_used_args(element2));
        if (not eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(element2)<<"' is not of required type "<<unparse_type(element_required_type)<<"!";
        element2.push_back({"is_default_value",ptree(false)}); // Do we need to add this annotation?
        model2.push_back({"",element2});
    }

    // 3. Create the new model tree with args in correct order
    model2 = ptree({{"value",model2},{"type",required_type}});
    set_used_args(model2, used_args);

    return model2;
}

optional<ptree>
TypecheckingState::typecheck_and_annotate_get_state(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    if (name != "get_state") return {}; // get_state[state_name]

    string state_name = model[0].second;
    if (not state.count(state_name))
        throw myexception()<<"translate: no state '"<<state_name<<"'!";
    auto result_type = state.at(state_name);
    eqs = eqs && unify(result_type, required_type);
    if (not eqs)
        throw myexception()<<"get_state: state '"<<state_name<<"' is of type '"<<unparse_type(result_type)<<"', not required type '"<<unparse_type(required_type)<<"'";

    auto arg = ptree({{"value",ptree(state_name)},{"type","String"}});
    // ARGH: arrays with ptree are really annoying.
    auto model2 = ptree("get_state",{ {"",arg}});

    model2 = ptree({{"value",model2},{"type",required_type}});
    set_used_args(model2,{});

    return model2;
}

optional<ptree>
TypecheckingState::typecheck_and_annotate_var(const ptree& required_type, const ptree& model) const
{
    if (not model.has_value<string>()) return {};

    auto name = model.get_value<string>();

    // 1. Get var type
    type_t result_type;
    set<string> used_args;
    if (not name.empty() and name[0] == '@')
    {
        auto arg_name = name.substr(1);
        auto type = type_for_arg(arg_name);
        used_args = {arg_name};
        if (not type)
            throw myexception()<<"can't find argument '"<<name<<"'";
        result_type = *type;
        assert(not result_type.is_null());
    }
    else if (auto type = type_for_var(name))
    {
        result_type = *type;
        assert(not result_type.is_null());
    }
    else
        return {};

    // 2. Check argument applications.
    ptree model2;
    model2.value = name;
    int arity = 0;
    for(auto& [arg_name, arg]: model)
    {
	// 2a. Complain about named arguments.
	if (not arg_name.empty()) throw myexception()<<"Named arguments node allowed in functions that are variables";

	// 2b. Check that function takes this many arguments.
	auto a = get_fresh_type_var("a");
	auto b = get_fresh_type_var("b");
	auto ftype = make_type_apps("Function",{a,b});
	eqs = eqs && unify(result_type, ftype);
	if (not eqs)
	{
	    if (arity == 0)
		throw myexception()<<"Treating non-function variable '"<<name<<"' as function!";
	    else
		throw myexception()<<"Supplying "<<model.size()<<" arguments to function '"<<name<<"', but it only takes "<<arity<<"!";
	}

	// 2c. Typecheck the argument
	auto arg2 = typecheck_and_annotate(a, arg);

	model2.push_back({"",arg2});

	// 2d. The expression taking the next argument has type 'b'
	result_type = b;
	arity++;
    }

    // 3. Unify required type or add conversion function.
    if (auto model2 = unify_or_convert(model, result_type, required_type))
	return typecheck_and_annotate(required_type, *model2);

    model2 = ptree({{"value",model2},{"type",result_type}});
    set_used_args(model2, used_args);

    return model2;
}

optional<ptree>
TypecheckingState::typecheck_and_annotate_constant(const ptree& required_type, const ptree& model) const
{
    // 1. Determine result type from literal.
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
        else
            return {};
    }

    // 2. Unify required type with rule result type
    if (auto model2 = unify_or_convert(model, result_type, required_type))
	return typecheck_and_annotate(required_type, *model2);

    // 3. If this is a constant or variable, then we are done here.
    if (not model.empty())
        throw myexception()<<"Term '"<<model.value<<"' of type '"<<unparse_type(result_type)
                           <<"' should not have arguments!";

    auto model2 = ptree({{"value",model},{"type",result_type}});
    set_used_args(model2,{});

    return model2;
}

ptree TypecheckingState::typecheck_and_annotate_function(const ptree& required_type, const ptree& model) const
{
    assert(model.has_value<string>());
    auto name = model.get_value<string>();
    auto rule = R.require_rule_for_func(name);
    rule = freshen_type_vars(rule, *fv_source);
    // Record any new variables that we are using as bound variables

    //	std::cout<<"name = "<<name<<" required_type = "<<unparse_type(required_type)<<"  result_type = "<<unparse_type(result_type)<<std::endl;

    auto result_type = rule.get_child("result_type");

    // 2. Unify required type with rule result type
    if (auto model2 = unify_or_convert(model, result_type, required_type))
	return typecheck_and_annotate(required_type, *model2);

    for(const auto& constraint: rule.get_child("constraints"))
        eqs.add_constraint(constraint.second);

    // 5.1 Update required type and rules with discovered constraints
    rule = substitute_in_rule_types(eqs, rule);

    // Create the new model tree with args in correct order
    ptree model2;
    set<string> used_args;
    model2.value =rule.get<string>("name"); 

    // 6. Check that we didn't supply unreferenced arguments.
    map<string,int> arg_count;
    for(const auto& [arg_name,_]: model)
    {
	if (not maybe_get_arg(rule, arg_name))
	    throw myexception()<<"Function '"<<name<<"' has no argument '"<<arg_name<<"' in term:\n"<<model.show();
	arg_count[arg_name]++;
	if (arg_count[arg_name] > 1)
	    throw myexception()<<"Supplied argument '"<<arg_name<<"' more than once in term:\n"<<model.show();
    }

    map<string,ptree> arg_env;
    for(const auto& [_, argument]: rule.get_child("args"))
    {
	string arg_name = argument.get<string>("name");

	auto arg_required_type = argument.get_child("type");
        arg_env.insert({arg_name, arg_required_type});
    }

    // 7. Handle arguments in rule order
    for(const auto& [_,argument]: rule.get_child("args"))
    {
	string arg_name = argument.get<string>("name");

	auto arg_required_type = argument.get_child("type");
	substitute(eqs, arg_required_type);
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

        auto scope2 = *this;
        if (is_default)
            scope2.args = arg_env;

        optional<ptree> alphabet_value;
        if (auto alphabet_expression = argument.get_child_optional("alphabet"))
        {
            auto scope3 = *this;
            scope3.args = arg_env;
	    auto alphabet_required_type = get_fresh_type_var("a");
            auto alphabet_value2= scope3.typecheck_and_annotate(alphabet_required_type, *alphabet_expression);
            eqs = eqs && scope3.eqs;
            if (not eqs)
                throw myexception()<<"Expression '"<<unparse_annotated(alphabet_value2)<<"' makes unification fail!";
            auto alphabet_type = alphabet_value2.get_child("type");
            scope2.state["alphabet"] = alphabet_type;
            alphabet_value = alphabet_value2;
        }

        auto arg_value2 = scope2.typecheck_and_annotate(arg_required_type, arg_value);
        eqs = eqs && scope2.eqs;
        if (not eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(arg_value2)<<"' is not of required type "<<unparse_type(arg_required_type)<<"!";

        if (alphabet_value)
            arg_value2.push_back({"alphabet",*alphabet_value});
        arg_value2.push_back({"is_default_value",ptree(is_default)});
        if (not is_default)
            add(used_args, get_used_args(arg_value2));

	model2.push_back({arg_name, arg_value2});
    }

    model2 = ptree({{"value",model2},{"type",result_type}});
    if (rule.get("no_log",false))
	model2.push_back({"no_log",ptree(true)});
    if (auto extract = rule.get_child_optional("extract"))
	model2.push_back({"extract",*extract});

    set_used_args(model2, used_args);

    return model2;
}

ptree TypecheckingState::typecheck_and_annotate(const ptree& required_type, const ptree& model) const
{
    ptree result;
    // 1. Get result type and the rule, if there is one.
    if (auto constant = typecheck_and_annotate_constant(required_type, model))
        result = *constant;

    else if (auto variable = typecheck_and_annotate_var(required_type, model))
        result = *variable;

    else if (auto let = typecheck_and_annotate_let(required_type, model))
        result = *let;

    else if (auto lambda = typecheck_and_annotate_lambda(required_type, model))
        result = *lambda;

    else if (auto list = typecheck_and_annotate_list(required_type, model))
        result = *list;

    else if (auto tuple = typecheck_and_annotate_tuple(required_type, model))
        result = *tuple;

    else if (auto get_state = typecheck_and_annotate_get_state(required_type, model))
        result = *get_state;

    else
	result = typecheck_and_annotate_function(required_type, model);

    return result;
}

void TypecheckingState::add_states(const std::map<string,std::pair<std::string,ptree>>& state_info)
{
    for(auto& [state_name,p]: state_info)
    {
	auto& [_, state_type] = p;
	state.insert({state_name, state_type});
    }
}
