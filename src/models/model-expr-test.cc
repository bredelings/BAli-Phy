#include "models/model-expr.H"
#include "models/compile.H"
#include "models/parse.H"
#include "models/rules.H"
#include "models/typecheck.H"

#include <cassert>
#include <chrono>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

namespace
{

using namespace CmdModel;

// Exercises native command-line type construction, parsing, decomposition,
// variable recognition, equality, ordering, and display.
void test_model_type_ast()
{
    auto int_type = type_t("Int");
    auto double_type = type_t("Double");
    auto a_type = type_t("a");
    auto fresh_type = type_t("a#0");

    assert(int_type == type_con("Int"));
    assert(a_type == type_var("a"));
    assert(is_type_variable(a_type));
    assert(is_type_variable(fresh_type));
    assert(not is_type_variable(int_type));

    auto list_int = list_type(int_type);
    auto [list_head, list_args] = get_type_apps(list_int);
    assert(list_head == type_t("List"));
    assert(list_args.size() == 1);
    assert(list_args[0] == int_type);

    auto pair_type = tuple_type({int_type, type_t("Bool")});
    assert(unparse_type(pair_type) == "(Int,Bool)");
    assert(function_type(int_type, double_type) == parse_type("Int -> Double"));
    assert(CM::type_app("Distribution", double_type) == parse_type("Distribution<Double>"));
    assert(parse_type("a") == a_type);
    assert(type_atom("a#0") == fresh_type);

    std::set<type_t> ordered{double_type, int_type, list_int, pair_type};
    assert(ordered.count(int_type));
    assert(ordered.count(list_int));

    auto vars = find_variables_in_type(function_type(a_type, list_type(type_t("b#1"))));
    assert(vars == std::set<std::string>({"a", "b#1"}));
}

// Builds a small untyped variable expression for AST unit tests.
UntypedExpr var_expr(std::string name)
{
    return {NoAnn{}, Var{std::move(name)}};
}

// Builds a small untyped argument-reference expression for AST unit tests.
UntypedExpr arg_ref_expr(std::string name)
{
    return {NoAnn{}, ArgRef{std::move(name)}};
}

// Builds a small untyped integer expression for AST unit tests.
UntypedExpr int_expr(int value)
{
    return {NoAnn{}, IntLiteral{value}};
}

// Builds a small untyped double expression for AST unit tests.
UntypedExpr double_expr(double value)
{
    return {NoAnn{}, DoubleLiteral{value}};
}

// Builds a small untyped boolean expression for AST unit tests.
UntypedExpr bool_expr(bool value)
{
    return {NoAnn{}, BoolLiteral{value}};
}

// Builds a small untyped string expression for AST unit tests.
UntypedExpr string_expr(std::string value)
{
    return {NoAnn{}, StringLiteral{std::move(value)}};
}

// Builds a small untyped get_state expression for AST unit tests.
UntypedExpr get_state_expr(std::string name)
{
    return {NoAnn{}, GetState{std::move(name)}};
}

// Wraps an untyped expression as a positional call argument.
Arg<NoAnn> positional_arg(UntypedExpr value)
{
    return {"", std::move(value), false, false, std::nullopt};
}

// Wraps an untyped expression as a named call argument.
Arg<NoAnn> named_arg(std::string name, UntypedExpr value)
{
    return {std::move(name), std::move(value), false, false, std::nullopt};
}

// Builds an untyped call expression from a function name and argument edges.
UntypedExpr call_expr(std::string function, std::vector<Arg<NoAnn>> args)
{
    return {NoAnn{}, Call<NoAnn>{std::move(function), std::move(args)}};
}

// Builds an untyped list expression from element expressions.
UntypedExpr list_expr(std::vector<UntypedExpr> elements)
{
    return {NoAnn{}, List<NoAnn>{std::move(elements)}};
}

// Builds an untyped tuple expression from element expressions.
UntypedExpr tuple_expr(std::vector<UntypedExpr> elements)
{
    return {NoAnn{}, Tuple<NoAnn>{std::move(elements)}};
}

// Builds an untyped let expression from declarations and a body expression.
UntypedExpr let_expr(Decls<NoAnn> decls, UntypedExpr body)
{
    return {NoAnn{}, Let<NoAnn>{std::move(decls), std::move(body)}};
}

// Builds an untyped variable pattern for lambda tests.
UntypedPattern var_pattern(std::string name)
{
    return {NoAnn{}, VarPattern{std::move(name)}};
}

// Builds an untyped tuple pattern for lambda tests.
UntypedPattern tuple_pattern(std::vector<UntypedPattern> elements)
{
    return {NoAnn{}, TuplePattern<NoAnn>{std::move(elements)}};
}

// Builds an untyped lambda expression from a pattern and body expression.
UntypedExpr lambda_expr(UntypedPattern pattern, UntypedExpr body)
{
    return {NoAnn{}, Lambda<NoAnn>{std::move(pattern), std::move(body)}};
}

// Builds an untyped sample expression from a distribution expression.
UntypedExpr sample_expr(UntypedExpr dist)
{
    return {NoAnn{}, Sample<NoAnn>{std::move(dist)}};
}

// Builds a typed model AST annotation with explicit metadata defaults.
Ann expr_ann(type_t type, std::set<std::string> used_args = {}, bool no_log = false, std::optional<std::string> extract = {})
{
    return {std::move(type), std::move(used_args), no_log, std::move(extract)};
}

// Builds a typed expression from an annotation type and a concrete AST node.
template<class Node>
TypedExpr typed_expr(type_t type, Node node, std::set<std::string> used_args = {})
{
    return {expr_ann(std::move(type), std::move(used_args)), std::move(node)};
}

// Wraps a typed expression as a positional typed call argument.
Arg<Ann> typed_positional_arg(TypedExpr value)
{
    return {"", std::move(value), false, false, std::nullopt};
}

// Wraps a typed expression as a named typed call argument with optional edge
// metadata.
Arg<Ann> typed_named_arg(std::string name, TypedExpr value, bool is_default = false, bool suppress_default = false, std::optional<TypedExpr> alphabet = {})
{
    std::optional<TypedExpr> alphabet_value;
    if (alphabet)
        alphabet_value = std::move(*alphabet);

    return {
        std::move(name),
        std::move(value),
        is_default,
        suppress_default,
        std::move(alphabet_value)
    };
}

// Builds a typed call expression from a function name, argument edges, and
// annotation metadata.
TypedExpr typed_call_expr(std::string function, std::vector<Arg<Ann>> args, type_t type, bool no_log = false, std::optional<std::string> extract = {})
{
    return {expr_ann(std::move(type), {}, no_log, std::move(extract)), Call<Ann>{std::move(function), std::move(args)}};
}

// Checks that copying recursive AST nodes performs deep copies of boxed
// children rather than sharing nested expression storage.
void test_copy_independence()
{
    UntypedExpr original{
        NoAnn{},
        Call<NoAnn>{
            "f",
            {positional_arg(var_expr("x"))}
        }
    };

    auto copied = original;

    auto& copied_call = copied.as<Call<NoAnn>>();
    auto& copied_var = require_arg_value(copied_call.args[0]).as<Var>();
    copied_var.name = "y";

    auto& original_call = original.as<Call<NoAnn>>();
    auto& original_var = require_arg_value(original_call.args[0]).as<Var>();

    assert(original_var.name == "x");
    assert(copied_var.name == "y");
}

// Checks basic node predicates and child traversal for a simple sample node.
void test_accessors_and_traversal()
{
    UntypedExpr expr{
        NoAnn{},
        Sample<NoAnn>{var_expr("dist")}
    };

    assert(is_sample(expr));
    assert(not is_list(expr));

    int n_children = 0;
    for_each_child(expr, [&](const UntypedExpr&) { n_children++; });
    assert(n_children == 1);
}

// Requires check_invariants() to reject one malformed untyped expression.
void expect_invariant_failure(const UntypedExpr& expr)
{
    try
    {
        check_invariants(expr);
    }
    catch (const std::logic_error&)
    {
        return;
    }
    assert(false);
}

// Requires check_invariants() to reject one malformed untyped pattern.
void expect_pattern_invariant_failure(const UntypedPattern& pattern)
{
    try
    {
        check_invariants(pattern);
    }
    catch (const std::logic_error&)
    {
        return;
    }
    assert(false);
}

// Exercises structural invariant checks that are independent of typechecking.
void test_invariants()
{
    check_invariants(tuple_expr({int_expr(1), int_expr(2)}));
    expect_invariant_failure(tuple_expr({int_expr(1)}));

    check_invariants(tuple_pattern({var_pattern("x"), var_pattern("y")}));
    expect_pattern_invariant_failure(tuple_pattern({var_pattern("x")}));
}

// Verifies that absent call-argument values are represented on argument edges
// without pretending to be expressions.
void test_absent_argument_values()
{
    UntypedExpr expr{
        NoAnn{},
        Call<NoAnn>{
            "f",
            {
                {"x", std::nullopt, false, false, std::nullopt},
                named_arg("y", int_expr(2))
            }
        }
    };
    auto& call = expr.as<Call<NoAnn>>();
    assert(not call.args[0].value);
    assert(call.args[1].value);
}

// Checks parser wrappers now return native model AST nodes.
void test_parser_wrappers()
{
    Rules rules({});

    auto expr = parse_model_expr(rules, "~normal(0, 1)", "test expression");
    assert(is_sample(expr));

    auto decls = parse_model_decls(rules, "x = 1; y = x");
    assert(decls.size() == 2);
    assert(decls[0].first == "x");
    assert(decls[1].first == "y");

    auto tuple_lambda = parse_model_expr(rules, "|(x,y):x|", "tuple-pattern lambda");
    auto& lambda = tuple_lambda.as<Lambda<NoAnn>>();
    auto& pattern = lambda.pattern.as<TuplePattern<NoAnn>>();
    assert(pattern.elements.size() == 2);
    assert(pattern.elements[0].as<VarPattern>().name == "x");
    assert(pattern.elements[1].as<VarPattern>().name == "y");
}

// Exercises untyped AST pretty-printing for representative syntax.
void test_untyped_pretty_printing()
{
    assert(unparse(int_expr(1)) == "1");
    assert(unparse(double_expr(1.5)) == "1.5");
    assert(unparse(bool_expr(true)) == "true");
    assert(unparse(string_expr("abc")) == "\"abc\"");
    assert(unparse(var_expr("x")) == "x");
    assert(unparse(arg_ref_expr("arg")) == "@arg");

    assert(unparse(call_expr("f", {positional_arg(int_expr(1)), positional_arg(var_expr("x"))})) == "f(1, x)");
    assert(unparse(call_expr("+", {positional_arg(int_expr(1)), positional_arg(int_expr(2))})) == "1+2");
    assert(unparse(sample_expr(call_expr("normal", {positional_arg(int_expr(0)), positional_arg(int_expr(1))}))) == "~normal(0, 1)");

    auto expr = let_expr({{"x", int_expr(1)}}, var_expr("x"));
    assert(unparse(expr) == "x where {x = 1}");

    Decls<NoAnn> decls{
        {"x", int_expr(1)},
        {"y", call_expr("+", {positional_arg(var_expr("x")), positional_arg(int_expr(1))})}
    };
    assert(unparse(decls) == "x = 1; y = x+1");

    assert(show_model(sample_expr(var_expr("normal"))) == "~ normal");
    assert(show_model(var_expr("x")) == "= x");
}

// Exercises typed AST pretty-printing directly, without a ptree conversion
// oracle.
void test_typed_pretty_printing()
{
    assert(unparse_annotated(typed_expr(type_t("Int"), IntLiteral{1})) == "1");
    assert(unparse_annotated(typed_expr(type_t("String"), StringLiteral{"abc"})) == "\"abc\"");

    auto add = typed_call_expr("+", {
        typed_positional_arg(typed_expr(type_t("Int"), IntLiteral{1})),
        typed_positional_arg(typed_expr(type_t("Int"), IntLiteral{2}))
    }, type_t("Int"));
    assert(unparse_annotated(add) == "1+2");

    auto pair_type = CM::type_apps("Tuple", {type_t("Text"), type_t("Int")});
    auto list_type = CM::type_app("List", pair_type);
    TypedExpr pairs{
        expr_ann(list_type),
        List<Ann>{{
            typed_expr(pair_type, Tuple<Ann>{{typed_expr(type_t("Text"), Var{"a"}), typed_expr(type_t("Int"), IntLiteral{1})}}),
            typed_expr(pair_type, Tuple<Ann>{{typed_expr(type_t("Text"), Var{"b"}), typed_expr(type_t("Int"), IntLiteral{2})}})
        }}
    };
    assert(unparse_annotated(pairs) == "{a: 1, b: 2}");

    auto default_state = typed_expr(type_t("Alphabet"), GetState{"alphabet"});
    auto hidden_default = typed_expr(type_t("Int"), IntLiteral{1});
    auto call = typed_call_expr("f", {
        typed_named_arg("alphabet", std::move(default_state), true),
        typed_named_arg("n", std::move(hidden_default), true, true)
    }, type_t("Model"));
    assert(unparse_annotated(call) == "f");

    auto sample = typed_expr(
        type_t("Double"),
        Sample<Ann>{typed_expr(CM::type_app("Distribution", type_t("Double")), Var{"normal"})}
    );
    assert(show_model_annotated(sample) == "~ normal");
}

// Checks solved type equations propagate through typed AST annotations.
void test_typed_substitution()
{
    auto a = type_t("a#0");
    auto b = type_t("b#1");
    auto c = type_t("c#2");
    auto alphabet_type = type_t("alphabet#3");

    TypedExpr list_expr{
        Ann{CM::type_app("List", c), {}, false, {}},
        List<Ann>{{TypedExpr{Ann{c, {}, false, {}}, IntLiteral{1}}}}
    };

    TypedExpr expr{
        Ann{a, {}, false, {}},
        Call<Ann>{
            "f",
            {
                Arg<Ann>{
                    "x",
                    TypedExpr{Ann{b, {}, false, {}}, Var{"x"}},
                    false,
                    false,
                    TypedExpr{Ann{alphabet_type, {}, false, {}}, Var{"dna"}}
                },
                Arg<Ann>{
                    "items",
                    std::move(list_expr),
                    false,
                    false,
                    std::nullopt
                }
            }
        }
    };

    auto eqs = unify(a, type_t("Double"))
             && unify(b, type_t("Int"))
             && unify(c, type_t("String"))
             && unify(alphabet_type, type_t("Alphabet"));
    assert(eqs);

    substitute_annotated(eqs, expr);

    assert(expr.ann.type == type_t("Double"));
    auto& call = expr.as<Call<Ann>>();
    assert(require_arg_value(call.args[0]).ann.type == type_t("Int"));
    assert(call.args[0].alphabet);
    assert(call.args[0].alphabet->ann.type == type_t("Alphabet"));
    assert(require_arg_value(call.args[1]).ann.type == CM::type_app("List", type_t("String")));

    auto& list = require_arg_value(call.args[1]).as<List<Ann>>();
    assert(list.elements[0].ann.type == type_t("String"));

    TypedDecls decls{
        {"x", TypedExpr{Ann{type_t("decl#4"), {}, false, {}}, Var{"x"}}}
    };
    auto decl_eqs = unify(type_t("decl#4"), type_t("Int"));
    assert(decl_eqs);
    substitute_annotated(decl_eqs, decls);
    assert(decls[0].second.ann.type == type_t("Int"));
}

// Builds a TypecheckingState fixture with optional identifiers and state vars.
TypecheckingState test_typechecker(const Rules& rules, const std::map<std::string,type_t>& identifiers = {}, const std::map<std::string,type_t>& state = {})
{
    return TypecheckingState(rules, std::make_shared<FVSource>(), identifiers, state);
}

// Requires AST expression typechecking to produce a typed expression with the
// requested top-level type after substituting solved equations.
void expect_typecheck_expr(const Rules& rules, const type_t& required_type, const UntypedExpr& model, const std::map<std::string,type_t>& identifiers = {}, const std::map<std::string,type_t>& state = {})
{
    auto TC = test_typechecker(rules, identifiers, state);
    auto typed = typecheck_model_expr(TC, required_type, model);
    substitute_annotated(TC.eqs, typed);
    assert(typed.ann.type == required_type);
}

// Requires AST expression typechecking with the standard empty rule set.
void expect_typecheck_expr(const type_t& required_type, const UntypedExpr& model, const std::map<std::string,type_t>& identifiers = {}, const std::map<std::string,type_t>& state = {})
{
    Rules rules({});
    expect_typecheck_expr(rules, required_type, model, identifiers, state);
}

// Exercises direct AST typechecking for constants, variables, collections,
// lambda patterns, state, lets, and function-variable calls.
void test_typecheck_exprs()
{
    expect_typecheck_expr(type_t("Int"), var_expr("x"), {{"x", type_t("Int")}});
    expect_typecheck_expr(type_t("Int"), int_expr(1));
    expect_typecheck_expr(
        CM::type_apps("List", {type_t("Int")}),
        list_expr({int_expr(1), int_expr(2)})
    );
    expect_typecheck_expr(
        CM::type_apps("Tuple", {type_t("Int"), type_t("Bool")}),
        tuple_expr({int_expr(1), bool_expr(true)})
    );
    expect_typecheck_expr(
        type_t("Alphabet"),
        get_state_expr("alphabet"),
        {},
        {{"alphabet", type_t("Alphabet")}}
    );
    expect_typecheck_expr(
        type_t("Int"),
        let_expr({{"x", int_expr(1)}}, var_expr("x"))
    );
    expect_typecheck_expr(
        CM::type_apps("Function", {type_t("Int"), type_t("Int")}),
        lambda_expr(var_pattern("x"), var_expr("x"))
    );
    expect_typecheck_expr(
        type_t("Double"),
        call_expr("f", {positional_arg(int_expr(1))}),
        {{"f", CM::type_apps("Function", {type_t("Int"), type_t("Double")})}}
    );
}

// Checks that tuple-pattern lambda typechecking solves the structured argument
// type and annotates each pattern slot with its concrete type.
void test_typecheck_tuple_pattern_lambda()
{
    Rules rules({});
    auto required_type = CM::type_apps(
        "Function",
        {CM::type_apps("Tuple", {type_t("Int"), type_t("Bool")}), type_t("Int")}
    );
    auto model = lambda_expr(tuple_pattern({var_pattern("x"), var_pattern("flag")}), var_expr("x"));

    auto TC = test_typechecker(rules);
    auto typed = typecheck_model_expr(TC, required_type, model);
    substitute_annotated(TC.eqs, typed);

    assert(typed.ann.type == required_type);
    auto& lambda = typed.as<Lambda<Ann>>();
    auto& pattern = lambda.pattern.as<TuplePattern<Ann>>();
    assert(pattern.elements.size() == 2);
    assert(pattern.elements[0].ann.type == type_t("Int"));
    assert(pattern.elements[1].ann.type == type_t("Bool"));
    assert(pattern.elements[0].as<VarPattern>().name == "x");
    assert(pattern.elements[1].as<VarPattern>().name == "flag");
    assert(lambda.body.ann.type == type_t("Int"));
}

// Checks that function-valued variables propagate used_args from positional
// arguments, including when the callee is itself an @arg reference.
void test_typecheck_variable_function_used_args()
{
    Rules rules({});

    // Typechecks one variable-function call and checks the exact used_args set
    // on the typed result.
    auto expect_used_args = [&](const UntypedExpr& model, const std::map<std::string,type_t>& identifiers, const std::map<std::string,type_t>& args, const std::set<std::string>& expected)
    {
        auto TC = test_typechecker(rules, identifiers);
        TC.args = args;
        auto typed = typecheck_model_expr(TC, type_t("Double"), model);
        assert(typed.ann.used_args == expected);
    };

    auto f_type = CM::type_apps("Function", {type_t("Int"), type_t("Double")});
    expect_used_args(
        call_expr("f", {positional_arg(arg_ref_expr("x"))}),
        {{"f", f_type}},
        {{"x", type_t("Int")}},
        {"x"}
    );

    expect_used_args(
        call_expr("@f", {positional_arg(arg_ref_expr("x"))}),
        {},
        {{"f", f_type}, {"x", type_t("Int")}},
        {"f", "x"}
    );
}

// Verifies expression typechecker failures that are intentionally clearer than
// old fallback behavior.
void test_typecheck_direct_errors()
{
    Rules rules({});
    // Checks that one direct AST typecheck attempt fails with the expected
    // diagnostic fragment.
    auto expect_error = [&](const type_t& required_type, const CM::UntypedExpr& expr, const std::string& message)
    {
        auto TC = test_typechecker(rules);
        try
        {
            (void)typecheck_model_expr(TC, required_type, expr);
        }
        catch(const std::exception& e)
        {
            assert(std::string(e.what()).find(message) != std::string::npos);
            return;
        }
        assert(false);
    };

    expect_error(type_t("Int"), CM::UntypedExpr{NoAnn{}, Placeholder{}}, "Placeholder '_'");
    expect_error(type_t("Int"), call_expr("unknown", {positional_arg(int_expr(1))}), "No direct typechecker");
}

// Test workaround: Rules currently load only from binding files, so these tests
// create a tiny temporary package.  Replace with an in-memory Rules builder if
// the production loader grows one.
std::filesystem::path make_rule_fixture()
{
    auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
    auto root = std::filesystem::temp_directory_path() / ("bali-phy-model-expr-test-" + std::to_string(stamp));
    auto functions = root / "bindings" / "functions";
    std::filesystem::create_directories(functions);

    {
        std::ofstream out(functions / "fixture_model.json");
        out << R"JSON({
    "name": "fixture_model",
    "result_type": "Int",
    "no_log": true,
    "extract": "all",
    "call": "fixtureModel(@x,@y,@z)",
    "args": [
        {"name": "x", "type": "Int"},
        {"name": "y", "type": "Int", "default_value": "2"},
        {"name": "z", "type": "Int", "alphabet": "dna"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "intToDouble.json");
        out << R"JSON({
    "name": "intToDouble",
    "result_type": "Double",
    "no_log": true,
    "call": "intToDouble(@x)",
    "args": [
        {"name": "x", "type": "Int"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "zero.json");
        out << R"JSON({
    "name": "zero",
    "result_type": "Int",
    "no_log": true,
    "call": "zero",
    "args": []
})JSON";
    }

    {
        std::ofstream out(functions / "sample.json");
        out << R"JSON({
    "name": "sample",
    "result_type": "a",
    "perform": true,
    "no_log": true,
    "call": "sample(@dist)",
    "args": [
        {"name": "dist", "type": "Distribution<a>"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "normal.json");
        out << R"JSON({
    "name": "normal",
    "result_type": "Distribution<Double>",
    "call": "normal(@mu,@sigma)",
    "args": [
        {"name": "mu", "type": "Double"},
        {"name": "sigma", "type": "Double"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "discrete.json");
        out << R"JSON({
    "name": "discrete",
    "result_type": "DiscreteDist<a>",
    "no_log": true,
    "call": "Discrete(@pairs)",
    "args": [
        {"name": "pairs", "type": "List<(a,Double)>"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "convertDiscrete.json");
        out << R"JSON({
    "name": "convertDiscrete",
    "result_type": "Distribution<a>",
    "no_log": true,
    "call": "@x",
    "args": [
        {"name": "x", "type": "DiscreteDist<a>"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "unit_mixture.json");
        out << R"JSON({
    "name": "unit_mixture",
    "result_type": "DiscreteDist<CTMC<a>>",
    "no_log": true,
    "call": "unitMixture(@submodel)",
    "args": [
        {"name": "submodel", "type": "CTMC<a>"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "multiMixtureModel.json");
        out << R"JSON({
    "name": "multiMixtureModel",
    "result_type": "MultiMixtureModel<a>",
    "no_log": true,
    "call": "SModel.mmm(@submodel)",
    "args": [
        {"name": "submodel", "type": "DiscreteDist<CTMC<a>>"}
    ]
})JSON";
    }

    {
        std::ofstream out(functions / "f.json");
        out << R"JSON({
    "name": "f",
    "result_type": "CTMC<a>",
    "call": "SModel.plus_f(@submodel)",
    "args": [
        {"name": "submodel", "type": "ExchangeModel<a>"}
    ]
})JSON";
    }

    return root;
}

void test_typecheck_decls(const Rules& rules);

// Verifies rule-backed calls, defaults, alphabets, and conversion calls using
// direct AST inputs and a temporary binding-file fixture.
void test_typecheck_rule_calls()
{
    auto root = make_rule_fixture();
    try
    {
        Rules rules({root});
        expect_typecheck_expr(
            rules,
            type_t("Int"),
            call_expr("fixture_model", {named_arg("x", int_expr(1)), named_arg("z", int_expr(3))}),
            {{"dna", type_t("Alphabet")}}
        );
        expect_typecheck_expr(rules, type_t("Double"), int_expr(1));
        expect_typecheck_expr(rules, type_t("Int"), var_expr("zero"));
        expect_typecheck_expr(rules, type_t("Double"), sample_expr(call_expr("normal", {
            named_arg("mu", int_expr(0)),
            named_arg("sigma", int_expr(1))
        })));
        auto shadowed_rule_call = parse_model_expr(rules, "f(1) where {f = |x:x|}", "shadowing test");
        expect_typecheck_expr(rules, type_t("Int"), shadowed_rule_call);
        auto shadowed_rule_model = compile_model(
            rules,
            test_typechecker(rules),
            CodeGenState(rules),
            type_t("Int"),
            "f(1) where {f = |x:x|}",
            "shadowing compile test"
        );
        assert(shadowed_rule_model.type == type_t("Int"));
        auto tuple_pattern_model = compile_model(
            rules,
            test_typechecker(rules),
            CodeGenState(rules),
            type_t("Int"),
            "f((1,2)) where {f = |(x,y):x|}",
            "tuple-pattern shadowing compile test"
        );
        assert(tuple_pattern_model.type == type_t("Int"));
        expect_typecheck_expr(
            rules,
            CM::type_app("DiscreteDist", type_t("Int")),
            list_expr({
                tuple_expr({int_expr(1), double_expr(0.25)}),
                tuple_expr({int_expr(2), double_expr(0.75)})
            })
        );
        expect_typecheck_expr(
            rules,
            CM::type_app("Distribution", type_t("Int")),
            var_expr("d"),
            {{"d", CM::type_app("DiscreteDist", type_t("Int"))}}
        );
        expect_typecheck_expr(
            rules,
            CM::type_app("DiscreteDist", CM::type_app("CTMC", type_t("AA"))),
            var_expr("m"),
            {{"m", CM::type_app("CTMC", type_t("AA"))}}
        );
        expect_typecheck_expr(
            rules,
            CM::type_app("MultiMixtureModel", type_t("AA")),
            var_expr("m"),
            {{"m", CM::type_app("CTMC", type_t("AA"))}}
        );
        expect_typecheck_expr(
            rules,
            CM::type_app("CTMC", type_t("AA")),
            var_expr("s"),
            {{"s", CM::type_app("ExchangeModel", type_t("AA"))}}
        );
        test_typecheck_decls(rules);
    }
    catch (...)
    {
        std::filesystem::remove_all(root);
        throw;
    }
    std::filesystem::remove_all(root);
}

// Exercises declaration typechecking directly through the AST declaration path.
void test_typecheck_decls(const Rules& rules)
{
    // Checks that a declaration block typechecks to the expected declaration
    // names and substituted top-level types.
    auto expect_typed_decls = [&](const Decls<NoAnn>& decls, std::vector<std::pair<std::string,type_t>> expected)
    {
        auto ast_TC = test_typechecker(rules);
        auto typed = typecheck_model_decls(ast_TC, decls);
        substitute_annotated(ast_TC.eqs, typed);

        assert(typed.size() == expected.size());
        for(std::size_t i = 0; i < expected.size(); i++)
        {
            assert(typed[i].first == expected[i].first);
            assert(typed[i].second.ann.type == expected[i].second);
        }
    };

    expect_typed_decls({{"x", int_expr(1)}}, {{"x", type_t("Int")}});
    expect_typed_decls({
        {"x", int_expr(1)},
        {"y", var_expr("x")}
    }, {{"x", type_t("Int")}, {"y", type_t("Int")}});
    expect_typed_decls({
        {"xs", list_expr({int_expr(1), int_expr(2)})},
        {"pair", tuple_expr({var_expr("xs"), bool_expr(true)})}
    }, {
        {"xs", CM::type_app("List", type_t("Int"))},
        {"pair", CM::type_apps("Tuple", {CM::type_app("List", type_t("Int")), type_t("Bool")})}
    });
    expect_typed_decls({
        {"x", let_expr({{"y", int_expr(1)}}, var_expr("y"))}
    }, {{"x", type_t("Int")}});
    if (rules.get_rule_for_func("intToDouble"))
    {
        expect_typed_decls({
            {"x", call_expr("intToDouble", {named_arg("x", int_expr(1))})}
        }, {{"x", type_t("Double")}});
    }
}

// Exercises declaration typechecking for declarations that do not need
// binding-file rules.
void test_typecheck_decls()
{
    Rules rules({});
    test_typecheck_decls(rules);
}

// Checks that typed pretty extraction consumes direct typed AST expressions.
void test_extraction()
{
    auto scalar = pretty_model_t(typed_expr(type_t("Int"), IntLiteral{1}));
    assert(scalar.show() == "1");

    auto dist_type = CM::type_app("Distribution", type_t("Double"));
    auto sampled_arg = typed_expr(
        type_t("Double"),
        Sample<Ann>{typed_expr(dist_type, Var{"normal"})}
    );
    auto model = typed_call_expr("f", {typed_named_arg("x", sampled_arg)}, type_t("Model"));
    auto pretty = pretty_model_t(model);
    assert(pretty.show_main() == "f");
    assert(pretty.show_extracted().find("f:x") != std::string::npos);

    auto no_log = typed_call_expr("f", {typed_named_arg("x", sampled_arg)}, type_t("Model"), true);
    auto pretty_no_log = pretty_model_t(no_log);
    assert(pretty_no_log.show_extracted().empty());

    auto extract_all = typed_call_expr("f", {typed_named_arg("x", typed_expr(type_t("Int"), IntLiteral{1}))}, type_t("Model"), false, "all");
    auto pretty_extract_all = pretty_model_t(extract_all);
    assert(pretty_extract_all.show_extracted().find("f:x") != std::string::npos);

    auto with_alphabet = typed_call_expr("f", {
        typed_named_arg(
            "x",
            typed_expr(type_t("Int"), Var{"x"}, {"x"}),
            false,
            false,
            typed_expr(type_t("Alphabet"), Var{"dna"})
        )
    }, type_t("Model"));
    auto pretty_with_alphabet = pretty_model_t(with_alphabet);
    assert(pretty_with_alphabet.show_main() == "f(x)");
}

}

// Runs the focused model AST regression checks in a deterministic order.
int main()
{
    test_model_type_ast();
    test_copy_independence();
    test_accessors_and_traversal();
    test_invariants();
    test_absent_argument_values();
    test_parser_wrappers();
    test_untyped_pretty_printing();
    test_typed_pretty_printing();
    test_typed_substitution();
    test_typecheck_exprs();
    test_typecheck_tuple_pattern_lambda();
    test_typecheck_variable_function_used_args();
    test_typecheck_direct_errors();
    test_typecheck_decls();
    test_typecheck_rule_calls();
    test_extraction();
}
