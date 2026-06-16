#include "models/model-expr.H"
#include "models/model-expr-ptree.H"

#include <cassert>
#include <optional>
#include <stdexcept>
#include <string>

namespace
{

UntypedModelExpr var_expr(const std::string& name)
{
    return {UntypedAnn{}, Var{name}};
}

UntypedModelExpr int_expr(int value)
{
    return {UntypedAnn{}, IntLiteral{value}};
}

ModelArg<UntypedAnn> positional_arg(UntypedModelExpr value)
{
    return {"", Box<UntypedModelExpr>(std::move(value)), false, false, std::nullopt};
}

void test_copy_independence()
{
    UntypedModelExpr original{
        UntypedAnn{},
        Call<UntypedAnn>{
            "f",
            {positional_arg(var_expr("x"))}
        }
    };

    auto copied = original;

    auto& copied_call = std::get<Call<UntypedAnn>>(copied.node);
    auto& copied_var = std::get<Var>(copied_call.args[0].value->node);
    copied_var.name = "y";

    auto& original_call = std::get<Call<UntypedAnn>>(original.node);
    auto& original_var = std::get<Var>(original_call.args[0].value->node);

    assert(original_var.name == "x");
    assert(copied_var.name == "y");
}

void test_accessors_and_traversal()
{
    UntypedModelExpr expr{
        UntypedAnn{},
        Sample<UntypedAnn>{Box<UntypedModelExpr>(var_expr("dist"))}
    };

    assert(is_sample(expr));
    assert(not is_list(expr));

    int n_children = 0;
    for_each_child(expr, [&](const UntypedModelExpr&) { n_children++; });
    assert(n_children == 1);
}

void expect_invariant_failure(const UntypedModelExpr& expr)
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
    UntypedModelExpr tuple{
        UntypedAnn{},
        Tuple<UntypedAnn>{{int_expr(1), int_expr(2)}}
    };
    check_invariants(tuple);

    UntypedModelExpr one_element_tuple{
        UntypedAnn{},
        Tuple<UntypedAnn>{{int_expr(1)}}
    };
    expect_invariant_failure(one_element_tuple);

    UntypedModelExpr missing_arg{
        UntypedAnn{},
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
}
