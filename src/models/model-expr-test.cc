#include "models/model-expr.H"

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

}

int main()
{
    test_copy_independence();
    test_accessors_and_traversal();
    test_invariants();
}
