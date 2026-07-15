#include "models/model-expr.H"
#include "models/compile.H"
#include "models/parse.H"
#include "models/rules.H"
#include "models/typecheck.H"
#include "test-util.H"

#include <chrono>
#include <filesystem>
#include <fstream>
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

    BALI_PHY_TEST_CHECK(int_type == type_con("Int"));
    BALI_PHY_TEST_CHECK(a_type == type_var("a"));
    BALI_PHY_TEST_CHECK(is_type_variable(a_type));
    BALI_PHY_TEST_CHECK(is_type_variable(fresh_type));
    BALI_PHY_TEST_CHECK(not is_type_variable(int_type));

    auto list_int = list_type(int_type);
    auto [list_head, list_args] = get_type_apps(list_int);
    BALI_PHY_TEST_CHECK(list_head == type_t("List"));
    BALI_PHY_TEST_CHECK(list_args.size() == 1);
    BALI_PHY_TEST_CHECK(list_args[0] == int_type);

    auto pair_type = CmdModel::tuple_type({int_type, type_t("Bool")});
    BALI_PHY_TEST_CHECK(unparse_type(pair_type) == "(Int,Bool)");
    BALI_PHY_TEST_CHECK(function_type(int_type, double_type) == parse_type("Int -> Double"));
    BALI_PHY_TEST_CHECK(CM::type_app("Distribution", double_type) == parse_type("Distribution<Double>"));
    BALI_PHY_TEST_CHECK(parse_type("a") == a_type);
    BALI_PHY_TEST_CHECK(type_atom("a#0") == fresh_type);

    std::set<type_t> ordered{double_type, int_type, list_int, pair_type};
    BALI_PHY_TEST_CHECK(ordered.count(int_type));
    BALI_PHY_TEST_CHECK(ordered.count(list_int));

    auto vars = find_variables_in_type(function_type(a_type, list_type(type_t("b#1"))));
    BALI_PHY_TEST_CHECK(vars == std::set<std::string>({"a", "b#1"}));
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

// Wraps an untyped expression as a positional call argument.
Arg<NoAnn> positional_arg(UntypedExpr value)
{
    return {"", std::move(value), false, false, std::nullopt};
}

// Builds an untyped call expression from a function name and argument edges.
UntypedExpr call_expr(std::string function, std::vector<Arg<NoAnn>> args)
{
    return {NoAnn{}, Call<NoAnn>{std::move(function), std::move(args)}};
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

// Protects the value semantics of CmdModel::Box, whose manual ownership must
// deep-copy recursive nodes instead of aliasing their mutable storage.
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

    BALI_PHY_TEST_CHECK(original_var.name == "x");
    BALI_PHY_TEST_CHECK(copied_var.name == "y");
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
    BALI_PHY_TEST_CHECK(false);
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
    BALI_PHY_TEST_CHECK(false);
}

// Exercises structural invariant checks that are independent of typechecking.
void test_invariants()
{
    check_invariants(tuple_expr({int_expr(1), int_expr(2)}));
    expect_invariant_failure(tuple_expr({int_expr(1)}));

    check_invariants(tuple_pattern({var_pattern("x"), var_pattern("y")}));
    expect_pattern_invariant_failure(tuple_pattern({var_pattern("x")}));
}

// Exercises untyped AST pretty-printing for representative syntax.
void test_untyped_pretty_printing()
{
    BALI_PHY_TEST_CHECK(unparse(int_expr(1)) == "1");
    BALI_PHY_TEST_CHECK(unparse(double_expr(1.5)) == "1.5");
    BALI_PHY_TEST_CHECK(unparse(bool_expr(true)) == "true");
    BALI_PHY_TEST_CHECK(unparse(string_expr("abc")) == "\"abc\"");
    BALI_PHY_TEST_CHECK(unparse(var_expr("x")) == "x");
    BALI_PHY_TEST_CHECK(unparse(arg_ref_expr("arg")) == "@arg");

    BALI_PHY_TEST_CHECK(unparse(call_expr("f", {positional_arg(int_expr(1)), positional_arg(var_expr("x"))})) == "f(1, x)");
    BALI_PHY_TEST_CHECK(unparse(call_expr("+", {positional_arg(int_expr(1)), positional_arg(int_expr(2))})) == "1+2");
    BALI_PHY_TEST_CHECK(unparse(sample_expr(call_expr("normal", {positional_arg(int_expr(0)), positional_arg(int_expr(1))}))) == "~normal(0, 1)");

    auto expr = let_expr({{"x", int_expr(1)}}, var_expr("x"));
    BALI_PHY_TEST_CHECK(unparse(expr) == "x where {x = 1}");

    Decls<NoAnn> decls{
        {"x", int_expr(1)},
        {"y", call_expr("+", {positional_arg(var_expr("x")), positional_arg(int_expr(1))})}
    };
    BALI_PHY_TEST_CHECK(unparse(decls) == "x = 1; y = x+1");

    BALI_PHY_TEST_CHECK(show_model(sample_expr(var_expr("normal"))) == "~ normal");
    BALI_PHY_TEST_CHECK(show_model(var_expr("x")) == "= x");
}

// Exercises user-visible typed AST pretty-printing and argument metadata.
void test_typed_pretty_printing()
{
    BALI_PHY_TEST_CHECK(unparse_annotated(typed_expr(type_t("Int"), IntLiteral{1})) == "1");
    BALI_PHY_TEST_CHECK(unparse_annotated(typed_expr(type_t("String"), StringLiteral{"abc"})) == "\"abc\"");

    auto add = typed_call_expr("+", {
        typed_positional_arg(typed_expr(type_t("Int"), IntLiteral{1})),
        typed_positional_arg(typed_expr(type_t("Int"), IntLiteral{2}))
    }, type_t("Int"));
    BALI_PHY_TEST_CHECK(unparse_annotated(add) == "1+2");

    auto pair_type = CM::type_apps("Tuple", {type_t("Text"), type_t("Int")});
    auto list_type = CM::type_app("List", pair_type);
    TypedExpr pairs{
        expr_ann(list_type),
        List<Ann>{{
            typed_expr(pair_type, Tuple<Ann>{{typed_expr(type_t("Text"), Var{"a"}), typed_expr(type_t("Int"), IntLiteral{1})}}),
            typed_expr(pair_type, Tuple<Ann>{{typed_expr(type_t("Text"), Var{"b"}), typed_expr(type_t("Int"), IntLiteral{2})}})
        }}
    };
    BALI_PHY_TEST_CHECK(unparse_annotated(pairs) == "{a: 1, b: 2}");

    auto default_state = typed_expr(type_t("Alphabet"), GetState{"alphabet"});
    auto hidden_default = typed_expr(type_t("Int"), IntLiteral{1});
    auto call = typed_call_expr("f", {
        typed_named_arg("alphabet", std::move(default_state), true),
        typed_named_arg("n", std::move(hidden_default), true, true)
    }, type_t("Model"));
    BALI_PHY_TEST_CHECK(unparse_annotated(call) == "f");

    auto sample = typed_expr(
        type_t("Double"),
        Sample<Ann>{typed_expr(CM::type_app("Distribution", type_t("Double")), Var{"normal"})}
    );
    BALI_PHY_TEST_CHECK(show_model_annotated(sample) == "~ normal");
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
    BALI_PHY_TEST_CHECK(eqs);

    substitute_annotated(eqs, expr);

    BALI_PHY_TEST_CHECK(expr.ann.type == type_t("Double"));
    auto& call = expr.as<Call<Ann>>();
    BALI_PHY_TEST_CHECK(require_arg_value(call.args[0]).ann.type == type_t("Int"));
    BALI_PHY_TEST_CHECK(call.args[0].alphabet);
    BALI_PHY_TEST_CHECK(call.args[0].alphabet->ann.type == type_t("Alphabet"));
    BALI_PHY_TEST_CHECK(require_arg_value(call.args[1]).ann.type == CM::type_app("List", type_t("String")));

    auto& list = require_arg_value(call.args[1]).as<List<Ann>>();
    BALI_PHY_TEST_CHECK(list.elements[0].ann.type == type_t("String"));

    TypedDecls decls{
        {"x", TypedExpr{Ann{type_t("decl#4"), {}, false, {}}, Var{"x"}}}
    };
    auto decl_eqs = unify(type_t("decl#4"), type_t("Int"));
    BALI_PHY_TEST_CHECK(decl_eqs);
    substitute_annotated(decl_eqs, decls);
    BALI_PHY_TEST_CHECK(decls[0].second.ann.type == type_t("Int"));
}

// Builds a TypecheckingState fixture with optional identifiers and state vars.
TypecheckingState test_typechecker(const Rules& rules, const std::map<std::string,type_t>& identifiers = {}, const std::map<std::string,type_t>& state = {})
{
    return TypecheckingState(rules, std::make_shared<FVSource>(), identifiers, state);
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

    BALI_PHY_TEST_CHECK(typed.ann.type == required_type);
    auto& lambda = typed.as<Lambda<Ann>>();
    auto& pattern = lambda.pattern.as<TuplePattern<Ann>>();
    BALI_PHY_TEST_CHECK(pattern.elements.size() == 2);
    BALI_PHY_TEST_CHECK(pattern.elements[0].ann.type == type_t("Int"));
    BALI_PHY_TEST_CHECK(pattern.elements[1].ann.type == type_t("Bool"));
    BALI_PHY_TEST_CHECK(pattern.elements[0].as<VarPattern>().name == "x");
    BALI_PHY_TEST_CHECK(pattern.elements[1].as<VarPattern>().name == "flag");
    BALI_PHY_TEST_CHECK(lambda.body.ann.type == type_t("Int"));
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
        BALI_PHY_TEST_CHECK(typed.ann.used_args == expected);
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

// Test workaround: Rules load only from binding files, so the shadowing
// regression needs one temporary global rule. Remove this fixture when Rules
// can construct a rule in memory or the regression moves to an integration test.
std::filesystem::path make_shadowing_rule_fixture()
{
    auto stamp = std::chrono::steady_clock::now().time_since_epoch().count();
    auto root = std::filesystem::temp_directory_path() / ("bali-phy-model-expr-test-" + std::to_string(stamp));
    auto functions = root / "bindings" / "functions";
    std::filesystem::create_directories(functions);

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

// Checks that a local function shadows a binding-file rule with the same name
// throughout parsing, typechecking, and code generation.
void test_local_function_shadowing()
{
    auto root = make_shadowing_rule_fixture();
    try
    {
        Rules rules({root});
        auto model = compile_model(
            rules,
            test_typechecker(rules),
            CodeGenState(rules),
            type_t("Int"),
            "f(1) where {f = |x:x|}",
            "shadowing compile test"
        );
        BALI_PHY_TEST_CHECK(model.type == type_t("Int"));
    }
    catch (...)
    {
        std::filesystem::remove_all(root);
        throw;
    }
    std::filesystem::remove_all(root);
}

// Checks tuple-pattern syntax through the complete model compilation path.
void test_tuple_pattern_compile()
{
    Rules rules({});
    auto model = compile_model(
        rules,
        test_typechecker(rules),
        CodeGenState(rules),
        type_t("Int"),
        "f((1,2)) where {f = |(x,y):x|}",
        "tuple-pattern compile test"
    );
    BALI_PHY_TEST_CHECK(model.type == type_t("Int"));
}

}

// Runs the focused model AST regression checks in a deterministic order.
int main()
{
    test_model_type_ast();
    test_copy_independence();
    test_invariants();
    test_untyped_pretty_printing();
    test_typed_pretty_printing();
    test_typed_substitution();
    test_typecheck_tuple_pattern_lambda();
    test_typecheck_variable_function_used_args();
    test_local_function_shadowing();
    test_tuple_pattern_compile();
}
