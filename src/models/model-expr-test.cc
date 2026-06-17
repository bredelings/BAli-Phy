#include "models/model-expr.H"
#include "models/model-expr-ptree.H"
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

namespace
{

using namespace CmdModel;

// Builds a small untyped variable expression for AST unit tests.
UntypedExpr var_expr(const std::string& name)
{
    return {NoAnn{}, Var{name}};
}

// Builds a small untyped integer expression for AST unit tests.
UntypedExpr int_expr(int value)
{
    return {NoAnn{}, IntLiteral{value}};
}

// Wraps an untyped expression as a positional call argument.
Arg<NoAnn> positional_arg(UntypedExpr value)
{
    return {"", CmdModel::Box<UntypedExpr>(std::move(value)), false, false, std::nullopt};
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

    auto& copied_call = std::get<Call<NoAnn>>(copied.node);
    auto& copied_var = std::get<Var>(require_arg_value(copied_call.args[0]).node);
    copied_var.name = "y";

    auto& original_call = std::get<Call<NoAnn>>(original.node);
    auto& original_var = std::get<Var>(require_arg_value(original_call.args[0]).node);

    assert(original_var.name == "x");
    assert(copied_var.name == "y");
}

// Checks basic node predicates and child traversal for a simple sample node.
void test_accessors_and_traversal()
{
    UntypedExpr expr{
        NoAnn{},
        Sample<NoAnn>{CmdModel::Box<UntypedExpr>(var_expr("dist"))}
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

// Exercises structural invariant checks that are independent of typechecking.
void test_invariants()
{
    UntypedExpr tuple{
        NoAnn{},
        Tuple<NoAnn>{{int_expr(1), int_expr(2)}}
    };
    check_invariants(tuple);

    UntypedExpr one_element_tuple{
        NoAnn{},
        Tuple<NoAnn>{{int_expr(1)}}
    };
    expect_invariant_failure(one_element_tuple);

}

// Requires one untyped ptree expression to survive AST conversion and back.
void expect_round_trip(const ptree& p)
{
    auto expr = model_expr_from_ptree(p);
    auto p2 = ptree_from_model_expr(expr);
    assert(p2 == p);
}

// Verifies that a null ptree is not a model expression, while null call
// arguments remain round-trippable absent values.
void test_absent_argument_values()
{
    try
    {
        (void)model_expr_from_ptree(ptree());
        assert(false);
    }
    catch(const std::logic_error&)
    {}

    auto expr = model_expr_from_ptree(ptree("f", {
        {"x", ptree()},
        {"y", ptree(2)}
    }));
    auto& call = std::get<Call<NoAnn>>(expr.node);
    assert(not call.args[0].value);
    assert(call.args[1].value);
    assert(ptree_from_model_expr(expr) == ptree("f", {
        {"x", ptree()},
        {"y", ptree(2)}
    }));
}

// Exercises untyped AST round trips for scalar literals and variable-like nodes.
void test_scalar_round_trips()
{
    expect_round_trip(ptree(1));
    expect_round_trip(ptree(1.5));
    expect_round_trip(ptree(true));
    expect_round_trip(ptree("\"abc\""));
    expect_round_trip(ptree("x"));
    expect_round_trip(ptree("@arg"));
    expect_round_trip(ptree("_"));
}

// Exercises untyped AST round trips for ordinary and operator call nodes.
void test_call_round_trips()
{
    expect_round_trip(ptree("f", {
        {"", ptree("x")},
        {"y", ptree(2)}
    }));

    expect_round_trip(ptree("f", {
        {"x", ptree()},
        {"y", ptree(2)}
    }));

    expect_round_trip(ptree("+", {
        {"", ptree(1)},
        {"", ptree(2)}
    }));
}

// Exercises untyped AST round trips for list and tuple nodes.
void test_collection_round_trips()
{
    expect_round_trip(ptree("List", {
        {"", ptree(1)},
        {"", ptree("x")}
    }));

    expect_round_trip(ptree("List", {
        {"", ptree("Tuple", {{"", ptree("a")}, {"", ptree(1)}})},
        {"", ptree("Tuple", {{"", ptree("b")}, {"", ptree(2)}})}
    }));

    expect_round_trip(ptree("Tuple", {
        {"", ptree(1)},
        {"", ptree("\"two\"")}
    }));
}

// Exercises untyped AST round trips for let, lambda, sample, and get_state.
void test_special_form_round_trips()
{
    expect_round_trip(ptree("sample", {
        {"", ptree("normal", {{"", ptree(0)}, {"", ptree(1)}})}
    }));

    expect_round_trip(ptree("!let", {
        {"decls", ptree("!Decls", {{"x", ptree(1)}})},
        {"body", ptree("x")}
    }));

    expect_round_trip(ptree("function", {
        {"", ptree("x")},
        {"", ptree("+", {{"", ptree("x")}, {"", ptree(1)}})}
    }));

    expect_round_trip(ptree("get_state", {
        {"", ptree("tree")}
    }));
}

// Requires one malformed untyped ptree to fail AST conversion.
void expect_conversion_failure(const ptree& p)
{
    try
    {
        (void)model_expr_from_ptree(p);
    }
    catch (const std::logic_error&)
    {
        return;
    }
    assert(false);
}

// Checks malformed legacy ptree shapes are rejected by the AST converter.
void test_malformed_ptree_rejections()
{
    expect_conversion_failure(ptree("Tuple", {{"", ptree(1)}}));
    expect_conversion_failure(ptree("!let", {{"decls", ptree("!Decls")}}));
    expect_conversion_failure(ptree("function", {{"", ptree("x")}}));
    expect_conversion_failure(ptree("sample", {{"", ptree("x")}, {"", ptree("y")}}));
    expect_conversion_failure(ptree("get_state", {{"", ptree(1)}}));
}

// Builds a legacy used_args annotation ptree for typed round-trip tests.
ptree used_args(std::initializer_list<std::string> args)
{
    ptree result;
    for(auto& arg: args)
        result.children().push_back({"", ptree(arg)});
    return result;
}

// Builds a minimal annotated expression ptree for typed converter tests.
ptree ann(ptree value, ptree type, std::initializer_list<std::string> args = {})
{
    return ptree({
        {"value", value},
        {"type", type},
        {"used_args", used_args(args)}
    });
}

// Builds a minimal annotated call-argument ptree for typed converter tests.
ptree arg_ann(ptree value, ptree type, std::initializer_list<std::string> args = {})
{
    auto result = ann(std::move(value), std::move(type), args);
    result.children().push_back({"is_default_value", ptree(false)});
    return result;
}

// Requires one annotated ptree expression to survive typed AST conversion.
void expect_typed_round_trip(const ptree& p)
{
    auto expr = typed_model_expr_from_annotated_ptree(p);
    auto p2 = annotated_ptree_from_typed_model_expr(expr);
    assert(p2 == p);
}

// Exercises typed AST round trips for scalar literals and variable-like nodes.
void test_typed_scalar_round_trips()
{
    expect_typed_round_trip(ann(ptree(1), ptree("Int")));
    expect_typed_round_trip(ann(ptree(1.5), ptree("Double")));
    expect_typed_round_trip(ann(ptree(true), ptree("Bool")));
    expect_typed_round_trip(ann(ptree("\"abc\""), ptree("String")));
    expect_typed_round_trip(ann(ptree("x"), ptree("Int"), {"x"}));
    expect_typed_round_trip(ann(ptree("@arg"), ptree("Double"), {"arg"}));
    expect_typed_round_trip(ann(ptree("_"), ptree("a")));
}

// Checks that typed argument metadata survives conversion through the AST.
void test_typed_argument_metadata_round_trip()
{
    auto arg_x = ann(ptree("x"), ptree("Double"), {"x"});
    arg_x.children().push_back({"is_default_value", ptree(true)});
    arg_x.children().push_back({"suppress_default", ptree(true)});

    auto alphabet = ann(ptree("dna"), ptree("Alphabet"));
    arg_x.children().push_back({"alphabet", alphabet});

    auto arg_y = ann(ptree(2), ptree("Int"));
    arg_y.children().push_back({"is_default_value", ptree(false)});

    auto model = ann(ptree("f", {
        {"x", arg_x},
        {"", arg_y}
    }), ptree("Model"), {"x"});
    model.children().push_back({"no_log", ptree(true)});
    model.children().push_back({"extract", ptree("all")});

    expect_typed_round_trip(model);
}

// Exercises typed AST round trips for collections and model special forms.
void test_typed_collection_and_special_round_trips()
{
    expect_typed_round_trip(ann(ptree("List", {
        {"", ann(ptree(1), ptree("Int"))},
        {"", ann(ptree(2), ptree("Int"))}
    }), ptree("List", {{"", ptree("Int")}})));

    expect_typed_round_trip(ann(ptree("Tuple", {
        {"", ann(ptree(1), ptree("Int"))},
        {"", ann(ptree("\"two\""), ptree("String"))}
    }), ptree("Tuple", {{"", ptree("Int")}, {"", ptree("String")}})));

    expect_typed_round_trip(ann(ptree("sample", {
        {"dist", arg_ann(ptree("normal", {
            {"", arg_ann(ptree(0), ptree("Double"))},
            {"", arg_ann(ptree(1), ptree("Double"))}
        }), ptree("Distribution", {{"", ptree("Double")}}))}
    }), ptree("Double")));

    expect_typed_round_trip(ann(ptree("!let", {
        {"decls", ptree("!Decls", {{"x", ann(ptree(1), ptree("Int"))}})},
        {"body", ann(ptree("x"), ptree("Int"), {"x"})}
    }), ptree("Int"), {"x"}));

    expect_typed_round_trip(ann(ptree("function", {
        {"", ann(ptree("x"), ptree("Int"))},
        {"", ann(ptree("x"), ptree("Int"), {"x"})}
    }), ptree("Function", {{"", ptree("Int")}, {"", ptree("Int")}}), {"x"}));

    expect_typed_round_trip(ann(ptree("get_state", {
        {"", ann(ptree("tree"), ptree("String"))}
    }), ptree("Tree")));
}

// Checks parser wrappers produce the same ASTs as direct ptree conversion.
void test_parser_wrappers()
{
    Rules rules({});

    auto expr = parse_model_expr(rules, "~normal(0, 1)", "test expression");
    assert(is_sample(expr));

    auto decls = parse_model_decls(rules, "x = 1; y = x");
    assert(decls.size() == 2);
    assert(decls[0].first == "x");
    assert(decls[1].first == "y");
}

// Requires untyped AST unparsing to match legacy ptree unparsing.
void expect_unparse_parity(const ptree& p)
{
    auto expr = model_expr_from_ptree(p);
    assert(unparse(expr) == unparse(p));
}

// Exercises untyped AST pretty-printing parity for representative syntax.
void test_untyped_pretty_printing()
{
    expect_unparse_parity(ptree(1));
    expect_unparse_parity(ptree(1.5));
    expect_unparse_parity(ptree(true));
    expect_unparse_parity(ptree("\"abc\""));
    expect_unparse_parity(ptree("x"));
    expect_unparse_parity(ptree("f", {{"", ptree(1)}, {"", ptree("x")}}));
    expect_unparse_parity(ptree("+", {{"", ptree(1)}, {"", ptree(2)}}));
    expect_unparse_parity(ptree("sample", {{"", ptree("normal", {{"", ptree(0)}, {"", ptree(1)}})}}));

    auto expr = model_expr_from_ptree(ptree("!let", {
        {"decls", ptree("!Decls", {{"x", ptree(1)}})},
        {"body", ptree("x")}
    }));
    assert(unparse(expr) == "x where {x = 1}");

    auto decls = model_decls_from_ptree(ptree("!Decls", {
        {"x", ptree(1)},
        {"y", ptree("+", {{"", ptree("x")}, {"", ptree(1)}})}
    }));
    assert(unparse(decls) == "x = 1; y = x+1");

    assert(show_model(model_expr_from_ptree(ptree("sample", {{"", ptree("normal")}}))) == "~ normal");
    assert(show_model(model_expr_from_ptree(ptree("x"))) == "= x");
}

// Requires typed AST unparsing to match legacy annotated-ptree unparsing.
void expect_typed_unparse_parity(const ptree& p)
{
    auto expr = typed_model_expr_from_annotated_ptree(p);
    assert(unparse_annotated(expr) == unparse_annotated(p));
}

// Exercises typed AST pretty-printing parity for representative syntax.
void test_typed_pretty_printing()
{
    expect_typed_unparse_parity(ann(ptree(1), ptree("Int")));
    expect_typed_unparse_parity(ann(ptree("\"abc\""), ptree("String")));
    expect_typed_unparse_parity(ann(ptree("+", {
        {"", arg_ann(ptree(1), ptree("Int"))},
        {"", arg_ann(ptree(2), ptree("Int"))}
    }), ptree("Int")));

    expect_typed_unparse_parity(ann(ptree("List", {
        {"", ann(ptree("Tuple", {
            {"", ann(ptree("a"), ptree("Text"))},
            {"", ann(ptree(1), ptree("Int"))}
        }), ptree("Tuple", {{"", ptree("Text")}, {"", ptree("Int")}}))},
        {"", ann(ptree("Tuple", {
            {"", ann(ptree("b"), ptree("Text"))},
            {"", ann(ptree(2), ptree("Int"))}
        }), ptree("Tuple", {{"", ptree("Text")}, {"", ptree("Int")}}))}
    }), ptree("List", {{"", ptree("Tuple")}})));

    auto default_state = ann(ptree("get_state", {
        {"", ann(ptree("alphabet"), ptree("String"))}
    }), ptree("Alphabet"));
    default_state.children().push_back({"is_default_value", ptree(true)});

    auto suppressed = arg_ann(ptree(1), ptree("Int"));
    suppressed.get_child("is_default_value") = ptree(true);
    suppressed.children().push_back({"suppress_default", ptree(true)});

    expect_typed_unparse_parity(ann(ptree("f", {
        {"alphabet", default_state},
        {"n", suppressed}
    }), ptree("Model")));

    auto sample = ann(ptree("sample", {{"dist", arg_ann(ptree("normal"), ptree("Distribution", {{"", ptree("Double")}}))}}), ptree("Double"));
    assert(show_model_annotated(typed_model_expr_from_annotated_ptree(sample)) == show_model_annotated(sample));
}

// Checks that solved type equations are substituted through typed AST nodes,
// argument edges, optional alphabets, and declarations.
// Checks solved type equations propagate through typed AST annotations.
void test_typed_substitution()
{
    auto a = ptree("a#0");
    auto b = ptree("b#1");
    auto c = ptree("c#2");
    auto alphabet_type = ptree("alphabet#3");

    TypedExpr list_expr{
        Ann{ptree("List", {{"", c}}), {}, false, {}},
        List<Ann>{{TypedExpr{Ann{c, {}, false, {}}, IntLiteral{1}}}}
    };

    TypedExpr expr{
        Ann{a, {}, false, {}},
        Call<Ann>{
            "f",
            {
                Arg<Ann>{
                    "x",
                    CmdModel::Box<TypedExpr>(TypedExpr{Ann{b, {}, false, {}}, Var{"x"}}),
                    false,
                    false,
                    CmdModel::Box<TypedExpr>(TypedExpr{Ann{alphabet_type, {}, false, {}}, Var{"dna"}})
                },
                Arg<Ann>{
                    "items",
                    CmdModel::Box<TypedExpr>(std::move(list_expr)),
                    false,
                    false,
                    std::nullopt
                }
            }
        }
    };

    auto eqs = unify(a, ptree("Double"))
             && unify(b, ptree("Int"))
             && unify(c, ptree("String"))
             && unify(alphabet_type, ptree("Alphabet"));
    assert(eqs);

    substitute_annotated(eqs, expr);

    assert(expr.ann.type == ptree("Double"));
    auto& call = std::get<Call<Ann>>(expr.node);
    assert(require_arg_value(call.args[0]).ann.type == ptree("Int"));
    assert(call.args[0].alphabet);
    assert(call.args[0].alphabet->get().ann.type == ptree("Alphabet"));
    assert(require_arg_value(call.args[1]).ann.type == ptree("List", {{"", ptree("String")}}));

    auto& list = std::get<List<Ann>>(require_arg_value(call.args[1]).node);
    assert(list.elements[0].ann.type == ptree("String"));

    TypedDecls decls{
        {"x", TypedExpr{Ann{ptree("decl#4"), {}, false, {}}, Var{"x"}}}
    };
    auto decl_eqs = unify(ptree("decl#4"), ptree("Int"));
    assert(decl_eqs);
    substitute_annotated(decl_eqs, decls);
    assert(decls[0].second.ann.type == ptree("Int"));
}

// Builds a TypecheckingState fixture with optional identifiers and state vars.
TypecheckingState test_typechecker(const Rules& rules, const std::map<std::string,ptree>& identifiers = {}, const std::map<std::string,ptree>& state = {})
{
    return TypecheckingState(rules, std::make_shared<FVSource>(), identifiers, state);
}

// Compares AST typechecking with the normalized legacy annotated-ptree result
// for one expression.
// Requires AST expression typechecking to match the legacy ptree typechecker.
void expect_typecheck_expr_parity(const Rules& rules, const ptree& required_type, const ptree& model, const std::map<std::string,ptree>& identifiers = {}, const std::map<std::string,ptree>& state = {})
{
    auto untyped = model_expr_from_ptree(model);

    auto ast_TC = test_typechecker(rules, identifiers, state);
    auto typed = typecheck_model_expr(ast_TC, required_type, untyped);

    auto ptree_TC = test_typechecker(rules, identifiers, state);
    auto expected = annotated_ptree_from_typed_model_expr(
        typed_model_expr_from_annotated_ptree(ptree_TC.typecheck_and_annotate(required_type, model))
    );

    auto actual = annotated_ptree_from_typed_model_expr(typed);
    if (not (actual == expected))
    {
        std::cerr<<"typecheck wrapper parity mismatch for "<<model.show(false)<<"\n";
        std::cerr<<"actual:\n"<<actual.show(false)<<"\n";
        std::cerr<<"expected:\n"<<expected.show(false)<<"\n";
        assert(false);
    }
}

// Requires AST expression typechecking parity using the standard test rules.
void expect_typecheck_expr_parity(const ptree& required_type, const ptree& model, const std::map<std::string,ptree>& identifiers = {}, const std::map<std::string,ptree>& state = {})
{
    Rules rules({});
    expect_typecheck_expr_parity(rules, required_type, model, identifiers, state);
}

// Exercises representative expression typechecking cases against the legacy
// ptree typechecker oracle.
// Exercises AST typechecker parity for constants, variables, collections, and
// direct function-variable cases.
void test_typecheck_expr_wrapper_parity()
{
    expect_typecheck_expr_parity(ptree("Int"), ptree("x"), {{"x", ptree("Int")}});
    expect_typecheck_expr_parity(ptree("Int"), ptree(1));
    expect_typecheck_expr_parity(
        make_type_apps("List", {ptree("Int")}),
        ptree("List", {{"", ptree(1)}, {"", ptree(2)}})
    );
    expect_typecheck_expr_parity(
        make_type_apps("Tuple", {ptree("Int"), ptree("Bool")}),
        ptree("Tuple", {{"", ptree(1)}, {"", ptree(true)}})
    );
    expect_typecheck_expr_parity(
        ptree("Alphabet"),
        ptree("get_state", {{"", ptree("alphabet")}}),
        {},
        {{"alphabet", ptree("Alphabet")}}
    );
    expect_typecheck_expr_parity(
        ptree("Int"),
        ptree("!let", {
            {"decls", ptree("!Decls", {{"x", ptree(1)}})},
            {"body", ptree("x")}
        })
    );
    expect_typecheck_expr_parity(
        make_type_apps("Function", {ptree("Int"), ptree("Int")}),
        ptree("function", {{"", ptree("x")}, {"", ptree("x")}})
    );
    expect_typecheck_expr_parity(
        ptree("Double"),
        ptree("f", {{"", ptree(1)}}),
        {{"f", make_type_apps("Function", {ptree("Int"), ptree("Double")})}}
    );
}

// Checks that function-valued variables propagate used_args from positional
// arguments, including when the callee is itself an @arg reference.
void test_typecheck_variable_function_used_args()
{
    Rules rules({});

    // Typechecks one variable-function call and checks the exact used_args set
    // on the typed result.
    auto expect_used_args = [&](const ptree& model, const std::map<std::string,ptree>& identifiers, const std::map<std::string,ptree>& args, const std::set<std::string>& expected)
    {
        auto TC = test_typechecker(rules, identifiers);
        TC.args = args;
        auto typed = typecheck_model_expr(TC, ptree("Double"), model_expr_from_ptree(model));
        assert(typed.ann.used_args == expected);
    };

    auto f_type = make_type_apps("Function", {ptree("Int"), ptree("Double")});
    expect_used_args(
        ptree("f", {{"", ptree("@x")}}),
        {{"f", f_type}},
        {{"x", ptree("Int")}},
        {"x"}
    );

    expect_used_args(
        ptree("@f", {{"", ptree("@x")}}),
        {},
        {{"f", f_type}, {"x", ptree("Int")}},
        {"f", "x"}
    );
}

// Verifies expression typechecker failures that are intentionally clearer
// than the old fallback-to-ptree behavior.
// Checks direct AST typechecker diagnostics for unsupported expression cases.
void test_typecheck_direct_errors()
{
    Rules rules({});
    // Checks that one direct AST typecheck attempt fails with the expected
    // diagnostic fragment.
    auto expect_error = [&](const ptree& required_type, const CM::UntypedExpr& expr, const std::string& message)
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

    expect_error(ptree("Int"), CM::UntypedExpr{NoAnn{}, Placeholder{}}, "Placeholder '_'");
    expect_error(ptree("Int"), model_expr_from_ptree(ptree("unknown", {{"", ptree(1)}})), "No direct typechecker");
}

// Test workaround: Rules currently load only from binding files, so these
// parity tests create a tiny temporary package.  Replace with an in-memory
// Rules builder if the production loader grows one.
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

void test_typecheck_decls_wrapper(const Rules& rules);

// Verifies rule-backed calls, defaults, alphabets, and conversion calls against
// a temporary binding-file fixture.
// Exercises AST rule-call typechecker parity for defaults, alphabets, and
// conversion cases using the temporary binding fixture.
void test_typecheck_rule_wrapper_parity()
{
    auto root = make_rule_fixture();
    try
    {
        Rules rules({root});
        expect_typecheck_expr_parity(
            rules,
            ptree("Int"),
            ptree("fixture_model", {{"x", ptree(1)}, {"z", ptree(3)}}),
            {{"dna", ptree("Alphabet")}}
        );
        expect_typecheck_expr_parity(rules, ptree("Double"), ptree(1));
        expect_typecheck_expr_parity(rules, ptree("Int"), ptree("zero"));
        expect_typecheck_expr_parity(rules, ptree("Double"), ptree("sample", {
            {"dist", ptree("normal", {{"mu", ptree(0)}, {"sigma", ptree(1)}})}
        }));
        expect_typecheck_expr_parity(
            rules,
            make_type_app("DiscreteDist", ptree("Int")),
            ptree("List", {
                {"", ptree("Tuple", {{"", ptree(1)}, {"", ptree(0.25)}})},
                {"", ptree("Tuple", {{"", ptree(2)}, {"", ptree(0.75)}})}
            })
        );
        expect_typecheck_expr_parity(
            rules,
            make_type_app("Distribution", ptree("Int")),
            ptree("d"),
            {{"d", make_type_app("DiscreteDist", ptree("Int"))}}
        );
        expect_typecheck_expr_parity(
            rules,
            make_type_app("DiscreteDist", make_type_app("CTMC", ptree("AA"))),
            ptree("m"),
            {{"m", make_type_app("CTMC", ptree("AA"))}}
        );
        expect_typecheck_expr_parity(
            rules,
            make_type_app("MultiMixtureModel", ptree("AA")),
            ptree("m"),
            {{"m", make_type_app("CTMC", ptree("AA"))}}
        );
        expect_typecheck_expr_parity(
            rules,
            make_type_app("CTMC", ptree("AA")),
            ptree("s"),
            {{"s", make_type_app("ExchangeModel", ptree("AA"))}}
        );
        test_typecheck_decls_wrapper(rules);
    }
    catch (...)
    {
        std::filesystem::remove_all(root);
        throw;
    }
    std::filesystem::remove_all(root);
}

// Exercises declaration typechecking directly through the AST declaration path.
void test_typecheck_decls_wrapper(const Rules& rules)
{
    // Checks that a declaration block typechecks and round-trips through the
    // codegen-facing annotated ptree conversion.
    auto expect_typed_decls = [&](const ptree& decls_ptree, std::vector<std::pair<std::string,ptree>> expected)
    {
        auto decls = model_decls_from_ptree(decls_ptree);

        auto ast_TC = test_typechecker(rules);
        auto typed = typecheck_model_decls(ast_TC, decls);
        substitute_annotated(ast_TC.eqs, typed);

        auto actual = annotated_ptree_from_typed_model_decls(typed);
        auto round_trip = annotated_ptree_from_typed_model_decls(
            typed_model_decls_from_annotated_ptree(actual)
        );

        assert(actual == round_trip);
        assert(typed.size() == expected.size());
        for(std::size_t i = 0; i < expected.size(); i++)
        {
            assert(typed[i].first == expected[i].first);
            assert(typed[i].second.ann.type == expected[i].second);
        }
    };

    expect_typed_decls(ptree("!Decls", {{"x", ptree(1)}}), {{"x", ptree("Int")}});
    expect_typed_decls(ptree("!Decls", {
        {"x", ptree(1)},
        {"y", ptree("x")}
    }), {{"x", ptree("Int")}, {"y", ptree("Int")}});
    expect_typed_decls(ptree("!Decls", {
        {"xs", ptree("List", {{"", ptree(1)}, {"", ptree(2)}})},
        {"pair", ptree("Tuple", {{"", ptree("xs")}, {"", ptree(true)}})}
    }), {
        {"xs", make_type_app("List", ptree("Int"))},
        {"pair", make_type_apps("Tuple", {make_type_app("List", ptree("Int")), ptree("Bool")})}
    });
    expect_typed_decls(ptree("!Decls", {
        {"x", ptree("!let", {
            {"decls", ptree("!Decls", {{"y", ptree(1)}})},
            {"body", ptree("y")}
        })}
    }), {{"x", ptree("Int")}});
    if (rules.get_rule_for_func("intToDouble"))
    {
        expect_typed_decls(ptree("!Decls", {
            {"x", ptree("intToDouble", {{"x", ptree(1)}})}
        }), {{"x", ptree("Double")}});
    }
}

// Exercises declaration typechecking for declarations that do not need
// binding-file rules.
void test_typecheck_decls_wrapper()
{
    Rules rules({});
    test_typecheck_decls_wrapper(rules);
}

// Checks that typed pretty extraction is stable across explicit ptree
// conversion, without keeping a separate ptree pretty implementation.
void expect_extraction_parity(const ptree& model)
{
    auto typed = typed_model_expr_from_annotated_ptree(model);
    auto roundtrip = typed_model_expr_from_annotated_ptree(annotated_ptree_from_typed_model_expr(typed));
    auto ast_pretty = pretty_model_t(typed);
    auto roundtrip_pretty = pretty_model_t(roundtrip);

    assert(ast_pretty.show() == roundtrip_pretty.show());
    assert(ast_pretty.show_main() == roundtrip_pretty.show_main());
    assert(ast_pretty.show_extracted() == roundtrip_pretty.show_extracted());
}

// Exercises AST extraction for non-models, random args, no_log, extract=all,
// lets, and argument-edge alphabet metadata.
void test_extraction_parity()
{
    expect_extraction_parity(ann(ptree(1), ptree("Int")));

    auto sampled_arg = arg_ann(ptree("sample", {
        {"dist", arg_ann(ptree("normal", {
            {"", arg_ann(ptree(0), ptree("Double"))},
            {"", arg_ann(ptree(1), ptree("Double"))}
        }), make_type_app("Distribution", ptree("Double")))}
    }), ptree("Double"));
    expect_extraction_parity(ann(ptree("f", {{"x", sampled_arg}}), ptree("Model")));

    auto no_log = ann(ptree("f", {{"x", sampled_arg}}), ptree("Model"));
    no_log.children().push_back({"no_log", ptree(true)});
    expect_extraction_parity(no_log);

    auto extract_all = ann(ptree("f", {{"x", arg_ann(ptree(1), ptree("Int"))}}), ptree("Model"));
    extract_all.children().push_back({"extract", ptree("all")});
    expect_extraction_parity(extract_all);

    expect_extraction_parity(ann(ptree("!let", {
        {"decls", ptree("!Decls", {{"x", ann(ptree(1), ptree("Int"))}})},
        {"body", ann(ptree("f", {{"x", sampled_arg}}), ptree("Model"))}
    }), ptree("Model")));

    auto arg_with_alphabet = arg_ann(ptree("x"), ptree("Int"), {"x"});
    arg_with_alphabet.children().push_back({"alphabet", ann(ptree("dna"), ptree("Alphabet"))});
    expect_extraction_parity(ann(ptree("f", {{"x", arg_with_alphabet}}), ptree("Model"), {"x"}));
}

}

// Runs the focused model AST regression checks in a deterministic order.
int main()
{
    test_copy_independence();
    test_accessors_and_traversal();
    test_invariants();
    test_absent_argument_values();
    test_scalar_round_trips();
    test_call_round_trips();
    test_collection_round_trips();
    test_special_form_round_trips();
    test_malformed_ptree_rejections();
    test_typed_scalar_round_trips();
    test_typed_argument_metadata_round_trip();
    test_typed_collection_and_special_round_trips();
    test_parser_wrappers();
    test_untyped_pretty_printing();
    test_typed_pretty_printing();
    test_typed_substitution();
    test_typecheck_expr_wrapper_parity();
    test_typecheck_variable_function_used_args();
    test_typecheck_direct_errors();
    test_typecheck_decls_wrapper();
    test_typecheck_rule_wrapper_parity();
    test_extraction_parity();
}
