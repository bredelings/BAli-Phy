#include "computation/preprocess.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/indexify.H"
#include "computation/runtime/trim.H"
#include "test-util.H"

namespace bali_phy_test
{
namespace
{
    // Constructs a Core integer constant for transformation test expressions.
    Core::Exp<> int_constant(int x)
    {
        Core::Constant c;
        c.value = x;
        return Core::Exp<>(c);
    }

    // Constructs nested binding and pattern scopes for inverse-preprocessing tests.
    Core::Exp<> inverse_preprocess_test_core()
    {
        Core::Var<> x("x");
        Core::Var<> z("z");
        Core::Var<> y("y");
        Core::Var<> w("w");
        Core::Var<> p("p");
        Core::Var<> q("q");

        Core::Decls<> decls;
        decls.push_back({y, Core::ConApp<>{"Pair", {z, x}}});

        Core::Pattern<> pair_pat;
        pair_pat.head = "Pair";
        pair_pat.args = {p, q};

        Core::Alt<> pair_alt{pair_pat, Core::ConApp<>{"Pair", {q, p}}};
        Core::Alt<> wildcard_alt{{}, z};

        Core::Exp<> body = Core::Case<>{w, {pair_alt, wildcard_alt}};
        body = Core::Let<>{Core::NonRec<>{{w, y}}, std::move(body)};
        body = Core::Let<>{Core::Rec<>{decls}, std::move(body)};
        body = Core::Let<>{
            Core::NonRec<>{{z, Core::ConApp<>{"Pair", {x, int_constant(3)}}}},
            std::move(body)};
        return Core::Lambda<>{x, body};
    }

    // Checks inverse preprocessing against nested Core scopes and Runtime trimming.
    void check_inverse_preprocess_round_trip()
    {
        auto original = inverse_preprocess_test_core();

        FreshVarState fresh1;
        auto prepared = prepare_for_translation(fresh1, original);

        auto lambda = prepared.to<Runtime::Lambda>();
        require(bool(lambda), "mixed binding test should prepare to a Runtime lambda");
        auto let = lambda->body.to<Runtime::Let>();
        require(bool(let), "mixed binding test should retain its Runtime let");
        require(let->binds.size() == 3,
                "contiguous Core lets should form one Runtime let chain");
        require(std::holds_alternative<Runtime::NonRec>(let->binds[0]) and
                    std::holds_alternative<Runtime::Rec>(let->binds[1]) and
                    std::holds_alternative<Runtime::NonRec>(let->binds[2]),
                "flattened Runtime let should preserve binding-group order");

        auto untrimmed = Runtime::trim_unnormalize(prepared);
        auto deindexed = deindexify(untrimmed);
        auto reindexed = indexify(deindexed);
        require(reindexed == untrimmed,
                "deindexify should round-trip with indexify after trim_unnormalize");

        auto recovered = unprepare_for_translation(prepared);

        FreshVarState fresh2;
        auto prepared_again = prepare_for_translation(fresh2, recovered);
        require(prepared_again == prepared,
                "inverse preprocess final Core should prepare back to the same Runtime expression");
    }

}

// Runs the direct Runtime indexing and Core conversion tests.
void run_transform_tests()
{
    check_inverse_preprocess_round_trip();
}
}
