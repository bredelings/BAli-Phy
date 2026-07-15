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
        Core::Var<> y("y");
        Core::Var<> p("p");
        Core::Var<> q("q");

        Core::Decls<> decls;
        decls.push_back({y, Core::ConApp<>{"Pair", {x, int_constant(3)}}});

        Core::Pattern<> pair_pat;
        pair_pat.head = "Pair";
        pair_pat.args = {p, q};

        Core::Alt<> pair_alt{pair_pat, Core::ConApp<>{"Pair", {q, p}}};
        Core::Alt<> wildcard_alt{{}, x};

        Core::Exp<> body = Core::Let<>{
            Core::Rec<>{decls}, Core::Exp<>(Core::Case<>{y, {pair_alt, wildcard_alt}})};
        return Core::Lambda<>{x, body};
    }

    // Checks inverse preprocessing against nested Core scopes and Runtime trimming.
    void check_inverse_preprocess_round_trip()
    {
        auto original = inverse_preprocess_test_core();

        FreshVarState fresh1;
        auto prepared = prepare_for_translation(fresh1, original);

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
