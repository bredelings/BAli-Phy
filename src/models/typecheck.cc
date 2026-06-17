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

set<string> find_rule_type_vars(const Rule& rule)
{
    set<string> vars = find_variables_in_type(rule.result_type);
    for(const auto& arg: rule.args)
	add(vars, find_variables_in_type(arg.type));
    return vars;
}

Rule substitute_in_rule_types(const map<string,type_t>& renaming, Rule rule)
{
    substitute(renaming, rule.result_type);
    for(auto& constraint: rule.constraints)
        substitute(renaming, constraint);
    for(auto& arg: rule.args)
	substitute(renaming, arg.type);
    return rule;
}

Rule substitute_in_rule_types(const equations& renaming, Rule rule)
{
    substitute(renaming, rule.result_type);
    for(auto& constraint: rule.constraints)
        substitute(renaming, constraint);
    for(auto& arg: rule.args)
	substitute(renaming, arg.type);
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

// Builds the AST call node used by implicit type conversions, preserving the
// same conversion function and argument names as the old model representation.
CM::UntypedExpr conversion_call(const string& function, const string& arg_name, CM::UntypedExpr model)
{
    return {
        CM::NoAnn{},
        CM::Call<CM::NoAnn>{
            function,
            {
                CM::Arg<CM::NoAnn>{
                    arg_name,
                    std::move(model),
                    false,
                    false,
                    std::nullopt
                }
            }
        }
    };
}

// True if an implicit conversion can rewrite an AST expression of type t1 into
// one compatible with t2, mutating model to the inserted conversion call.
equations convertible_to(CM::UntypedExpr& model, const type_t& t1, type_t t2)
{
    auto E = unify(t1, t2);
    if (E)
        return E;

    assert(not is_type_variable(t1) and not is_type_variable(t2));

    auto [head2,args2] = get_type_apps(t2);

    if (head2 == "Double")
    {
        t2 = "Int";
        E = convertible_to(model, t1, t2);
        if (E)
            model = conversion_call("intToDouble", "x", std::move(model));
    }
    else if (head2 == "DiscreteDist" and args2.size() == 1)
    {
        auto [head3,args3] = get_type_apps(args2[0]);
        if (head3 == "CTMC")
        {
            auto a = args3[0];
            t2 = CM::type_app("CTMC",a);

            E = convertible_to(model,t1,t2);
            if (E)
                model = conversion_call("unit_mixture", "submodel", std::move(model));
        }
        else
        {
            auto a = args2[0];
            t2 = CM::type_app("List", CM::type_apps("Tuple",{a,"Double"}));

            E = convertible_to(model, t1, t2);
            if (E)
                model = conversion_call("discrete", "pairs", std::move(model));
        }
    }
    else if (head2 == "Distribution" and args2.size() == 1)
    {
        auto [head1,args1] = get_type_apps(t1);
        if (head1 == "DiscreteDist" and args1.size() == 1)
        {
            E = convertible_to(model,args1[0],args2[0]);
            if (E)
                model = conversion_call("convertDiscrete", "x", std::move(model));
        }
    }
    else if (head2 == "MultiMixtureModel" and args2.size() == 1)
    {
        auto a = args2[0];
        t2 = CM::type_app("DiscreteDist",CM::type_app("CTMC",a));

        E = convertible_to(model,t1,t2);
        if (E)
            model = conversion_call("multiMixtureModel", "submodel", std::move(model));
    }
    else if (head2 == "CTMC" and args2.size() == 1)
    {
        auto a = args2[0];
        t2 = CM::type_app("ExchangeModel", a);
        E = convertible_to(model,t1,t2);
        if (E)
            model = conversion_call("f", "submodel", std::move(model));
    }

    return E;
}

// Unifies an AST expression type with the required type, or returns an AST
// conversion call that should be typechecked recursively.
optional<CM::UntypedExpr> TypecheckingState::unify_or_convert_model_expr(const CM::UntypedExpr& model, const type_t& type, const type_t& required_type) const
{
    auto tmp_eqs = eqs && unify(type, required_type);
    if (tmp_eqs)
    {
        eqs = tmp_eqs;
        return {};
    }

    auto model2 = model;
    auto type2 = type;
    auto required_type2 = required_type;
    substitute(eqs, type2);
    substitute(eqs, required_type2);
    if (not (eqs && convertible_to(model2, type2, required_type2)))
    {
        auto type_str = unparse_type(type2);
        auto required_str = unparse_type(required_type2);
        myexception e;
        e << "Term '" << unparse(model) << "' of type '" << type_str
          << "' cannot be converted to type '" << required_str << "'";
        throw e;
    }

    return model2;
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

optional<type_t> TypecheckingState::type_for_var(const string& name) const
{
    if (identifiers.count(name))
        return identifiers.at(name);
    else
        return {};
}

optional<type_t> TypecheckingState::type_for_arg(const string& name) const
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

// Applies solved type equations to every expression in a typed AST declaration
// list.
void substitute_annotated(const equations& eqs, CM::TypedDecls& decls)
{
    for(auto& [name, expr]: decls)
        substitute_annotated(eqs, expr);
}

// Applies solved type equations throughout an annotated model pattern.
void substitute_annotated(const equations& eqs, CM::TypedPattern& pattern)
{
    substitute(eqs, pattern.ann.type);

    pattern.visit(CM::overloaded{
        // Recurse through tuple-pattern element annotations.
        [&](CM::TuplePattern<CM::Ann>& tuple)
        {
            for(auto& element: tuple.elements)
                substitute_annotated(eqs, element);
        },
        [](auto&) {}
    });
}

// Applies solved type equations throughout an annotated model AST.
// This keeps expression annotations consistent after unification.
void substitute_annotated(const equations& eqs, CM::TypedExpr& expr)
{
    substitute(eqs, expr.ann.type);

    expr.visit(CM::overloaded{
        // Recurse through call argument values and optional argument
        // alphabets, since both carry typed subexpressions.
        [&](CM::Call<CM::Ann>& call)
        {
            for(auto& arg: call.args)
            {
                if (arg.value)
                    substitute_annotated(eqs, *arg.value);
                if (arg.alphabet)
                    substitute_annotated(eqs, *arg.alphabet);
            }
        },
        // Recurse through list element annotations.
        [&](CM::List<CM::Ann>& list)
        {
            for(auto& element: list.elements)
                substitute_annotated(eqs, element);
        },
        // Recurse through tuple element annotations.
        [&](CM::Tuple<CM::Ann>& tuple)
        {
            for(auto& element: tuple.elements)
                substitute_annotated(eqs, element);
        },
        // Recurse through let declarations and body annotations.
        [&](CM::Let<CM::Ann>& let)
        {
            substitute_annotated(eqs, let.decls);
            substitute_annotated(eqs, let.body);
        },
        // Recurse through lambda pattern and body annotations.
        [&](CM::Lambda<CM::Ann>& lambda)
        {
            substitute_annotated(eqs, lambda.pattern);
            substitute_annotated(eqs, lambda.body);
        },
        // Recurse through the sampled distribution annotation.
        [&](CM::Sample<CM::Ann>& sample)
        {
            substitute_annotated(eqs, sample.dist);
        },
        [](auto&) {}
    });
}

namespace
{

// Builds a typed model AST annotation with the common default metadata values.
CM::Ann model_ann(type_t type, set<string> used_args = {})
{
    return {std::move(type), std::move(used_args), false, {}};
}

// Assigns solved types to a lambda pattern after parse_pattern() has produced
// the pattern type structure and unification has refined it.
CM::TypedPattern typecheck_pattern(const TypecheckingState& TC, type_t required_type, const CM::UntypedPattern& pattern)
{
    substitute(TC.eqs, required_type);

    return pattern.visit(CM::overloaded{
        [&](const CM::VarPattern& var) -> CM::TypedPattern
        {
            return {model_ann(required_type), var};
        },
        // Assigns each tuple-pattern slot the corresponding tuple type.
        [&](const CM::TuplePattern<CM::NoAnn>& tuple) -> CM::TypedPattern
        {
            auto [head, args] = get_type_apps(required_type);
            if (head != "Tuple" or args.size() != tuple.elements.size())
                throw myexception()<<"Tuple pattern '"<<unparse(pattern)<<"' has incompatible type "<<unparse_type(required_type)<<"!";

            CM::TuplePattern<CM::Ann> typed_tuple;
            for(int i=0; i<tuple.elements.size(); i++)
                typed_tuple.elements.push_back(typecheck_pattern(TC, args[i], tuple.elements[i]));
            return {model_ann(required_type), std::move(typed_tuple)};
        }
    });
}

optional<CM::TypedExpr> typecheck_model_call(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr);

// Rejects parser placeholders before they can reach old ptree compatibility
// behavior.
optional<CM::TypedExpr> typecheck_model_invalid_placeholder(const CM::UntypedExpr& expr)
{
    if (expr.is<CM::Placeholder>())
        throw myexception()<<"Placeholder '_' may only appear while parsing argument application.";
    return {};
}

// Typechecks scalar literal AST nodes directly, falling back through the
// AST entry point if unification inserts a conversion call.
optional<CM::TypedExpr> typecheck_model_constant(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    type_t result_type;
    CM::TypedExpr::Node node;

    if (auto literal = expr.to<CM::IntLiteral>())
    {
        result_type = "Int";
        node = *literal;
    }
    else if (auto literal = expr.to<CM::DoubleLiteral>())
    {
        result_type = "Double";
        node = *literal;
    }
    else if (auto literal = expr.to<CM::BoolLiteral>())
    {
        result_type = "Bool";
        node = *literal;
    }
    else if (auto literal = expr.to<CM::StringLiteral>())
    {
        result_type = "String";
        node = *literal;
    }
    else
        return {};

    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    return CM::TypedExpr{model_ann(result_type), std::move(node)};
}

// Typechecks variable and argument-reference nodes against the current scope,
// preserving used-argument tracking for @arg references.
optional<CM::TypedExpr> typecheck_model_var(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    type_t result_type;
    set<string> used_args;
    CM::TypedExpr::Node node;

    if (auto var = expr.to<CM::Var>())
    {
        auto type = TC.type_for_var(var->name);
        if (not type)
            return {};
        result_type = *type;
        node = *var;
    }
    else if (auto arg = expr.to<CM::ArgRef>())
    {
        auto type = TC.type_for_arg(arg->name);
        if (not type)
            throw myexception()<<"can't find argument '@"<<arg->name<<"'";
        result_type = *type;
        used_args = {arg->name};
        node = *arg;
    }
    else
        return {};

    assert(not result_type.is_null());

    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    return CM::TypedExpr{model_ann(result_type, std::move(used_args)), std::move(node)};
}

// Typechecks a bare rule name as a zero-argument command when it is not a
// scoped variable.
optional<CM::TypedExpr> typecheck_model_nullary_call(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto var = expr.to<CM::Var>();
    if (not var)
        return {};
    if (TC.type_for_var(var->name))
        return {};
    if (not TC.R.get_rule_for_func(var->name))
        return {};

    CM::UntypedExpr call{
        CM::NoAnn{},
        CM::Call<CM::NoAnn>{var->name, {}}
    };
    return typecheck_model_call(TC, required_type, call);
}

// Typechecks calls where the callee is a scoped function variable instead of a
// rule-backed command.
optional<CM::TypedExpr> typecheck_model_var_call(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto call = expr.to<CM::Call<CM::NoAnn>>();
    if (not call)
        return {};

    type_t result_type;
    set<string> used_args;
    if (not call->function.empty() and call->function[0] == '@')
    {
        auto arg_name = call->function.substr(1);
        auto type = TC.type_for_arg(arg_name);
        if (not type)
            return {};
        result_type = *type;
        used_args = {arg_name};
    }
    else if (auto type = TC.type_for_var(call->function))
        result_type = *type;
    else
        return {};

    CM::Call<CM::Ann> typed_call{call->function, {}};
    int arity = 0;
    for(auto& arg: call->args)
    {
        if (not arg.name.empty())
            throw myexception()<<"Named arguments not allowed in functions that are variables";
        if (not arg.value)
            throw myexception()<<"Missing argument value in function variable application";

        auto a = TC.get_fresh_type_var("a");
        auto b = TC.get_fresh_type_var("b");
        auto ftype = CM::type_apps("Function", {a, b});
        TC.eqs = TC.eqs && unify(result_type, ftype);
        if (not TC.eqs)
        {
            if (arity == 0)
                throw myexception()<<"Treating non-function variable '"<<call->function<<"' as function!";
            else
                throw myexception()<<"Supplying "<<call->args.size()<<" arguments to function '"<<call->function<<"', but it only takes "<<arity<<"!";
        }

        auto arg2 = typecheck_model_expr(TC, a, *arg.value);
        add(used_args, arg2.ann.used_args);
        typed_call.args.push_back({
            "",
            std::move(arg2),
            false,
            false,
            std::nullopt
        });

        result_type = b;
        arity++;
    }

    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    return CM::TypedExpr{model_ann(result_type, std::move(used_args)), std::move(typed_call)};
}

// Typechecks list elements against a fresh element type and returns a typed
// list expression with the caller's required top-level annotation.
optional<CM::TypedExpr> typecheck_model_list(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto list = expr.to<CM::List<CM::NoAnn>>();
    if (not list)
        return {};

    auto element_required_type = TC.get_fresh_type_var("a");
    auto result_type = CM::type_app("List", element_required_type);
    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    substitute(TC.eqs, element_required_type);

    set<string> used_args;
    CM::List<CM::Ann> typed_list;
    for(auto& element: list->elements)
    {
        auto element2 = typecheck_model_expr(TC, element_required_type, element);
        add(used_args, element2.ann.used_args);
        if (not TC.eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(element2)<<"' is not of required type "<<unparse_type(element_required_type)<<"!";
        typed_list.elements.push_back(std::move(element2));
    }

    return CM::TypedExpr{model_ann(required_type, std::move(used_args)), std::move(typed_list)};
}

// Typechecks tuple elements against fresh slot types and returns a typed tuple
// expression with the caller's required top-level annotation.
optional<CM::TypedExpr> typecheck_model_tuple(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto tuple = expr.to<CM::Tuple<CM::NoAnn>>();
    if (not tuple)
        return {};

    vector<type_t> element_required_types;
    for(int i=0; i<tuple->elements.size(); i++)
        element_required_types.push_back(TC.get_fresh_type_var("a"));
    auto result_type = CM::type_apps("Tuple", element_required_types);

    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    set<string> used_args;
    CM::Tuple<CM::Ann> typed_tuple;
    for(int i=0; i<tuple->elements.size(); i++)
    {
        auto element_required_type = element_required_types[i];
        substitute(TC.eqs, element_required_type);
        auto element2 = typecheck_model_expr(TC, element_required_type, tuple->elements[i]);
        add(used_args, element2.ann.used_args);
        if (not TC.eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(element2)<<"' is not of required type "<<unparse_type(element_required_type)<<"!";
        typed_tuple.elements.push_back(std::move(element2));
    }

    return CM::TypedExpr{model_ann(required_type, std::move(used_args)), std::move(typed_tuple)};
}

// Typechecks a get_state node by looking up the named state in the current
// typechecking state and unifying it with the required type.
optional<CM::TypedExpr> typecheck_model_get_state(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto get_state = expr.to<CM::GetState>();
    if (not get_state)
        return {};

    if (not TC.state.count(get_state->state_name))
        throw myexception()<<"translate: no state '"<<get_state->state_name<<"'!";
    auto result_type = TC.state.at(get_state->state_name);
    TC.eqs = TC.eqs && unify(result_type, required_type);
    if (not TC.eqs)
        throw myexception()<<"get_state: state '"<<get_state->state_name<<"' is of type '"<<unparse_type(result_type)<<"', not required type '"<<unparse_type(required_type)<<"'";

    return CM::TypedExpr{model_ann(required_type), *get_state};
}

// Typechecks let declarations in an extended scope, then typechecks the body
// against the caller's required type.
optional<CM::TypedExpr> typecheck_model_let(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto let = expr.to<CM::Let<CM::NoAnn>>();
    if (not let)
        return {};

    set<string> used_args;
    auto scope2 = TC;
    auto decls2 = typecheck_model_decls(scope2, let->decls);
    for(auto& [name, decl_expr]: decls2)
        add(used_args, decl_expr.ann.used_args);

    auto body2 = typecheck_model_expr(scope2, required_type, let->body);
    add(used_args, body2.ann.used_args);
    TC.eqs = scope2.eqs;

    return CM::TypedExpr{
        model_ann(required_type, std::move(used_args)),
        CM::Let<CM::Ann>{std::move(decls2), std::move(body2)}
    };
}

// Typechecks a lambda by parsing the AST pattern natively, then checking the
// body in the scope introduced by the pattern.
optional<CM::TypedExpr> typecheck_model_lambda(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto lambda = expr.to<CM::Lambda<CM::NoAnn>>();
    if (not lambda)
        return {};

    auto [a, type_for_binder] = TC.parse_pattern(lambda->pattern);

    auto scope2 = TC;
    for(auto& [var,type]: type_for_binder)
        scope2.extend_scope(var, type);

    auto b = TC.get_fresh_type_var("b");
    auto ftype = CM::type_apps("Function", {a, b});
    TC.eqs = TC.eqs && unify(ftype, required_type);
    if (not TC.eqs)
        throw myexception()<<"Supplying a function, but expected '"<<unparse_type(required_type)<<"!";

    substitute(TC.eqs, b);

    auto body2 = typecheck_model_expr(scope2, b, lambda->body);
    TC.eqs = TC.eqs && scope2.eqs;
    auto used_args = body2.ann.used_args;

    substitute(TC.eqs, a);

    auto pattern2 = typecheck_pattern(scope2, a, lambda->pattern);
    substitute(TC.eqs, ftype);

    return CM::TypedExpr{
        model_ann(ftype, std::move(used_args)),
        CM::Lambda<CM::Ann>{
            std::move(pattern2),
            std::move(body2)
        }
    };
}

// Typechecks rule-backed function calls, including defaults, alphabets,
// used-argument propagation, and rule-level logging/extraction metadata.
optional<CM::TypedExpr> typecheck_model_call(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto call = expr.to<CM::Call<CM::NoAnn>>();
    if (not call)
        return {};

    auto maybe_rule = TC.R.get_rule_for_func(call->function);
    if (not maybe_rule)
        return {};

    auto rule = freshen_type_vars(*maybe_rule, *TC.fv_source);
    auto result_type = rule.result_type;

    if (auto converted = TC.unify_or_convert_model_expr(expr, result_type, required_type))
        return typecheck_model_expr(TC, required_type, *converted);

    for(const auto& constraint: rule.constraints)
        TC.eqs.add_constraint(constraint);

    rule = substitute_in_rule_types(TC.eqs, rule);

    map<string,int> arg_count;
    map<string,const CM::Arg<CM::NoAnn>*> supplied_args;
    for(const auto& arg: call->args)
    {
        if (not maybe_get_arg(rule, arg.name))
            throw myexception()<<"Function '"<<call->function<<"' has no argument '"<<arg.name<<"' in term:\n"<<show_model(expr);
        arg_count[arg.name]++;
        if (arg_count[arg.name] > 1)
            throw myexception()<<"Supplied argument '"<<arg.name<<"' more than once in term:\n"<<show_model(expr);
        if (arg.value)
            supplied_args[arg.name] = &arg;
    }

    map<string,type_t> arg_env;
    for(const auto& argument: rule.args)
        arg_env.insert({argument.name, argument.type});

    set<string> used_args;
    CM::Call<CM::Ann> typed_call{rule.name, {}};
    for(const auto& argument: rule.args)
    {
        auto arg_name = argument.name;
        auto arg_required_type = argument.type;
        substitute(TC.eqs, arg_required_type);

        CM::UntypedExpr arg_value;
        bool is_default = false;
        if (auto supplied = supplied_args.find(arg_name); supplied != supplied_args.end())
            arg_value = *supplied->second->value;
        else if (argument.default_value)
        {
            is_default = true;
            arg_value = *argument.default_value;
        }
        else
            throw myexception()<<"Command '"<<call->function<<"' missing required argument '"<<arg_name<<"'";

        auto scope2 = TC;
        if (is_default)
            scope2.args = arg_env;

        optional<CM::TypedExpr> alphabet_value;
        if (auto alphabet_expression = argument.alphabet)
        {
            auto scope3 = TC;
            scope3.args = arg_env;
            auto alphabet_required_type = TC.get_fresh_type_var("a");
            CM::TypedExpr alphabet_value2;
            try
            {
                alphabet_value2 = typecheck_model_expr(scope3, alphabet_required_type, *alphabet_expression);
            }
            catch(myexception& e)
            {
                e.prepend("In alphabet for argument '" + arg_name + "' of command '" + call->function + "': ");
                throw;
            }
            TC.eqs = TC.eqs && scope3.eqs;
            if (not TC.eqs)
                throw myexception()<<"Expression '"<<unparse_annotated(alphabet_value2)<<"' makes unification fail!";
            scope2.state["alphabet"] = alphabet_value2.ann.type;
            alphabet_value = std::move(alphabet_value2);
        }

        CM::TypedExpr arg_value2;
        try
        {
            arg_value2 = typecheck_model_expr(scope2, arg_required_type, arg_value);
        }
        catch(myexception& e)
        {
            e.prepend("In argument '" + arg_name + "' of command '" + call->function + "': ");
            throw;
        }
        TC.eqs = TC.eqs && scope2.eqs;
        if (not TC.eqs)
            throw myexception()<<"Expression '"<<unparse_annotated(arg_value2)<<"' is not of required type "<<unparse_type(arg_required_type)<<"!";

        if (not is_default)
            add(used_args, arg_value2.ann.used_args);

        typed_call.args.push_back({
            arg_name,
            std::move(arg_value2),
            is_default,
            false,
            std::move(alphabet_value)
        });
    }

    CM::Ann ann = model_ann(result_type, std::move(used_args));
    ann.no_log = rule.no_log;
    ann.extract = rule.extract;
    for(const auto& arg: typed_call.args)
        assert(arg.value);
    return CM::TypedExpr{std::move(ann), std::move(typed_call)};
}

// Typechecks sample sugar using the sample binding rule, then returns the
// dedicated AST Sample node expected by extraction and pretty-printing.
optional<CM::TypedExpr> typecheck_model_sample(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    auto sample = expr.to<CM::Sample<CM::NoAnn>>();
    if (not sample)
        return {};

    CM::UntypedExpr sample_call{
        CM::NoAnn{},
        CM::Call<CM::NoAnn>{
            "sample",
            {
                CM::Arg<CM::NoAnn>{
                    "dist",
                    sample->dist,
                    false,
                    false,
                    std::nullopt
                }
            }
        }
    };

    auto typed_call_expr = typecheck_model_call(TC, required_type, sample_call);
    assert(typed_call_expr);
    auto& typed_call = typed_call_expr->as<CM::Call<CM::Ann>>();
    assert(typed_call.args.size() == 1);
    auto dist = std::move(require_arg_value(typed_call.args[0]));

    return CM::TypedExpr{
        std::move(typed_call_expr->ann),
        CM::Sample<CM::Ann>{std::move(dist)}
    };
}

}

// Dispatches AST expression typechecking to direct handlers and reports
// unsupported nodes explicitly.
CM::TypedExpr typecheck_model_expr(const TypecheckingState& TC, const type_t& required_type, const CM::UntypedExpr& expr)
{
    if (auto invalid = typecheck_model_invalid_placeholder(expr))
        return *invalid;
    else if (auto constant = typecheck_model_constant(TC, required_type, expr))
        return *constant;
    else if (auto var = typecheck_model_var(TC, required_type, expr))
        return *var;
    else if (auto nullary_call = typecheck_model_nullary_call(TC, required_type, expr))
        return *nullary_call;
    else if (auto var_call = typecheck_model_var_call(TC, required_type, expr))
        return *var_call;
    else if (auto list = typecheck_model_list(TC, required_type, expr))
        return *list;
    else if (auto tuple = typecheck_model_tuple(TC, required_type, expr))
        return *tuple;
    else if (auto get_state = typecheck_model_get_state(TC, required_type, expr))
        return *get_state;
    else if (auto let = typecheck_model_let(TC, required_type, expr))
        return *let;
    else if (auto lambda = typecheck_model_lambda(TC, required_type, expr))
        return *lambda;
    else if (auto sample = typecheck_model_sample(TC, required_type, expr))
        return *sample;
    else if (auto call = typecheck_model_call(TC, required_type, expr))
        return *call;
    else
        throw myexception()<<"No direct typechecker for expression '"<<unparse(expr)<<"'.";
}

// Typechecks declarations in order, extending the scope with each fresh
// declaration type before checking its expression.
CM::TypedDecls typecheck_model_decls(TypecheckingState& TC, const CM::Decls<CM::NoAnn>& decls)
{
    CM::TypedDecls decls2;

    for(auto& [name, expr]: decls)
    {
        auto a = TC.get_fresh_type_var("a");
        TC.extend_scope(name, a);
        auto expr2 = typecheck_model_expr(TC, a, expr);
        if (not TC.eqs)
        {
            substitute(TC.eqs, a);
            throw myexception()<<"Expression '"<<unparse_annotated(expr2)<<"' is not of required type "<<unparse_type(a)<<"!";
        }
        decls2.push_back({name, std::move(expr2)});
    }

    return decls2;
}

// Parses lambda patterns from the model AST, assigning fresh types to binders
// and rejecting expression forms that are not valid patterns.
pair<type_t, map<string,type_t>> TypecheckingState::parse_pattern(const CM::UntypedPattern& pattern) const
{
    if (auto var = pattern.to<CM::VarPattern>())
    {
        auto type = get_fresh_type_var("p");
        return {type, {{var->name, type}}};
    }
    else if (auto tuple = pattern.to<CM::TuplePattern<CM::NoAnn>>())
    {
        vector<type_t> element_types;
        map<string,type_t> var_to_type;
        for(auto& value: tuple->elements)
        {
            auto [slot_type, slot_vars] = parse_pattern(value);
            element_types.push_back(slot_type);
            for(auto& [var_name,var_type]: slot_vars)
            {
                if (var_to_type.count(var_name))
                    throw myexception()<<"Variable '"<<var_name<<"' occurs twice in "<<unparse(pattern)<<"!";
                var_to_type.insert({var_name,var_type});
            }
        }
        auto type = CM::type_apps("Tuple", element_types);
        return {type,var_to_type};
    }
    else
        throw myexception()<<"Invalid lambda pattern '"<<unparse(pattern)<<"'.";
}

void TypecheckingState::add_states(const std::map<string,std::pair<std::string,type_t>>& state_info)
{
    for(auto& [state_name,p]: state_info)
    {
	auto& [_, state_type] = p;
	state.insert({state_name, state_type});
    }
}
