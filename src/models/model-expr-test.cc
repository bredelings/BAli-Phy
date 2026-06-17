#include "models/model-expr.H"
#include "models/model-expr-ptree.H"
#include "models/parse.H"
#include "models/rules.H"
#include "models/typecheck.H"

#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>

namespace
{

using namespace CmdModel;

UntypedExpr var_expr(const std::string& name)
{
    return {NoAnn{}, Var{name}};
}

UntypedExpr int_expr(int value)
{
    return {NoAnn{}, IntLiteral{value}};
}

Arg<NoAnn> positional_arg(UntypedExpr value)
{
    return {"", Box<UntypedExpr>(std::move(value)), false, false, std::nullopt};
}

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
    auto& copied_var = std::get<Var>(copied_call.args[0].value->node);
    copied_var.name = "y";

    auto& original_call = std::get<Call<NoAnn>>(original.node);
    auto& original_var = std::get<Var>(original_call.args[0].value->node);

    assert(original_var.name == "x");
    assert(copied_var.name == "y");
}

void test_accessors_and_traversal()
{
    UntypedExpr expr{
        NoAnn{},
        Sample<NoAnn>{Box<UntypedExpr>(var_expr("dist"))}
    };

    assert(is_sample(expr));
    assert(not is_list(expr));

    int n_children = 0;
    for_each_child(expr, [&](const UntypedExpr&) { n_children++; });
    assert(n_children == 1);
}

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

    UntypedExpr missing_arg{
        NoAnn{},
        MissingArg{}
    };
    expect_invariant_failure(missing_arg);
}

void expect_round_trip(const ptree& p)
{
    auto expr = model_expr_from_ptree(p);
    auto p2 = ptree_from_model_expr(expr);
    assert(p2 == p);
}

void test_scalar_round_trips()
{
    expect_round_trip(ptree(1));
    expect_round_trip(ptree(1.5));
    expect_round_trip(ptree(true));
    expect_round_trip(ptree("\"abc\""));
    expect_round_trip(ptree("x"));
    expect_round_trip(ptree("@arg"));
    expect_round_trip(ptree("_"));
    expect_round_trip(ptree());
}

void test_call_round_trips()
{
    expect_round_trip(ptree("f", {
        {"", ptree("x")},
        {"y", ptree(2)}
    }));

    expect_round_trip(ptree("+", {
        {"", ptree(1)},
        {"", ptree(2)}
    }));
}

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

void test_malformed_ptree_rejections()
{
    expect_conversion_failure(ptree("Tuple", {{"", ptree(1)}}));
    expect_conversion_failure(ptree("!let", {{"decls", ptree("!Decls")}}));
    expect_conversion_failure(ptree("function", {{"", ptree("x")}}));
    expect_conversion_failure(ptree("sample", {{"", ptree("x")}, {"", ptree("y")}}));
    expect_conversion_failure(ptree("get_state", {{"", ptree(1)}}));
}

ptree used_args(std::initializer_list<std::string> args)
{
    ptree result;
    for(auto& arg: args)
        result.children().push_back({"", ptree(arg)});
    return result;
}

ptree ann(ptree value, ptree type, std::initializer_list<std::string> args = {})
{
    return ptree({
        {"value", value},
        {"type", type},
        {"used_args", used_args(args)}
    });
}

ptree arg_ann(ptree value, ptree type, std::initializer_list<std::string> args = {})
{
    auto result = ann(std::move(value), std::move(type), args);
    result.children().push_back({"is_default_value", ptree(false)});
    return result;
}

void expect_typed_round_trip(const ptree& p)
{
    auto expr = typed_model_expr_from_annotated_ptree(p);
    auto p2 = annotated_ptree_from_typed_model_expr(expr);
    assert(p2 == p);
}

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
        {"", ann(ptree("normal", {
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

void expect_unparse_parity(const ptree& p)
{
    auto expr = model_expr_from_ptree(p);
    assert(unparse(expr) == unparse(p));
}

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

void expect_typed_unparse_parity(const ptree& p)
{
    auto expr = typed_model_expr_from_annotated_ptree(p);
    assert(unparse_annotated(expr) == unparse_annotated(p));
}

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

    auto sample = ann(ptree("sample", {{"", ann(ptree("normal"), ptree("Distribution", {{"", ptree("Double")}}))}}), ptree("Double"));
    assert(show_model_annotated(typed_model_expr_from_annotated_ptree(sample)) == show_model_annotated(sample));
}

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
                    Box<TypedExpr>(TypedExpr{Ann{b, {}, false, {}}, Var{"x"}}),
                    false,
                    false,
                    Box<TypedExpr>(TypedExpr{Ann{alphabet_type, {}, false, {}}, Var{"dna"}})
                },
                Arg<Ann>{
                    "items",
                    Box<TypedExpr>(std::move(list_expr)),
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
    assert(call.args[0].value->ann.type == ptree("Int"));
    assert(call.args[0].alphabet);
    assert(call.args[0].alphabet->get().ann.type == ptree("Alphabet"));
    assert(call.args[1].value->ann.type == ptree("List", {{"", ptree("String")}}));

    auto& list = std::get<List<Ann>>(call.args[1].value->node);
    assert(list.elements[0].ann.type == ptree("String"));

    TypedDecls decls{
        {"x", TypedExpr{Ann{ptree("decl#4"), {}, false, {}}, Var{"x"}}}
    };
    auto decl_eqs = unify(ptree("decl#4"), ptree("Int"));
    assert(decl_eqs);
    substitute_annotated(decl_eqs, decls);
    assert(decls[0].second.ann.type == ptree("Int"));
}

TypecheckingState test_typechecker(const Rules& rules, const std::map<std::string,ptree>& identifiers = {})
{
    return TypecheckingState(rules, std::make_shared<FVSource>(), identifiers);
}

void expect_typecheck_expr_parity(const ptree& required_type, const ptree& model, const std::map<std::string,ptree>& identifiers = {})
{
    Rules rules({});
    auto untyped = model_expr_from_ptree(model);

    auto ast_TC = test_typechecker(rules, identifiers);
    auto typed = typecheck_model_expr(ast_TC, required_type, untyped);

    auto ptree_TC = test_typechecker(rules, identifiers);
    auto expected = ptree_TC.typecheck_and_annotate(required_type, model);

    assert(annotated_ptree_from_typed_model_expr(typed) == expected);
}

void test_typecheck_expr_wrapper_parity()
{
    expect_typecheck_expr_parity(ptree("Int"), ptree("x"), {{"x", ptree("Int")}});
    expect_typecheck_expr_parity(ptree("Int"), ptree(1));
}

void test_typecheck_decls_wrapper_parity()
{
    Rules rules({});
    auto decls_ptree = ptree("!Decls", {{"x", ptree(1)}});
    auto decls = model_decls_from_ptree(decls_ptree);

    auto ast_TC = test_typechecker(rules);
    auto typed = typecheck_model_decls(ast_TC, decls);

    auto ptree_TC = test_typechecker(rules);
    auto expected = ptree_TC.typecheck_and_annotate_decls(decls_ptree);

    assert(annotated_ptree_from_typed_model_decls(typed) == expected);
}

}

int main()
{
    test_copy_independence();
    test_accessors_and_traversal();
    test_invariants();
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
    test_typecheck_decls_wrapper_parity();
}
