#include "computation/preprocess.H"
#include "computation/machine/graph_register.H"
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

    // Checks index shifting across recursive, non-recursive, and Trim scopes.
    void check_shift_free_indices()
    {
        Runtime::Exp e = Runtime::Let(Runtime::Rec({Runtime::IndexVar(1)}),
                                      Runtime::apply(Runtime::IndexVar(1),
                                                     {Runtime::IndexVar(0), Runtime::RegRef(7)}));

        auto shifted = Runtime::shift_free_indices(e, 1);
        auto let = shifted.to<Runtime::Let>();
        require(bool(let), "shifted expression should remain a Let");

        auto bind = let->to_rec()->rhss[0].to<Runtime::IndexVar>();
        require(bool(bind), "shifted Let bind should remain an IndexVar");
        require(bind->index == 2, "shifted Let bind index mismatch");

        auto app = let->body.to<Runtime::FunctionApp>();
        require(bool(app), "shifted Let body should remain an App");
        auto fn = app->head.to<Runtime::IndexVar>();
        auto bound_arg = app->args[0].to<Runtime::IndexVar>();
        auto reg_ref = app->args[1].to<Runtime::RegRef>();
        require(bool(fn), "shifted App function should remain an IndexVar");
        require(bool(bound_arg), "bound App argument should remain an IndexVar");
        require(bool(reg_ref), "RegRef App argument should remain a RegRef");
        require(fn->index == 2, "shifted App function index mismatch");
        require(bound_arg->index == 0, "bound App argument index mismatch");
        require(reg_ref->target == 7, "RegRef App argument target mismatch");

        Runtime::Exp nonrec = Runtime::Let(Runtime::NonRec{Runtime::IndexVar(0)},
                                           Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}));
        auto shifted_nonrec_exp = Runtime::shift_free_indices(nonrec, 1);
        auto shifted_nonrec = shifted_nonrec_exp.to<Runtime::Let>();
        require(shifted_nonrec and shifted_nonrec->to_nonrec(),
                "shifted NonRec should retain its binding form");
        require(shifted_nonrec->to_nonrec()->rhs.to<Runtime::IndexVar>()->index == 1,
                "NonRec shifting should treat the RHS at outer depth");
        auto nonrec_app = shifted_nonrec->body.to<Runtime::FunctionApp>();
        require(nonrec_app->head.to<Runtime::IndexVar>()->index == 2,
                "NonRec shifting should shift free body indices through the binder");
        require(nonrec_app->args[0].to<Runtime::IndexVar>()->index == 0,
                "NonRec shifting should preserve the bound body index");

        Runtime::Exp trim_exp = Runtime::Trim({0, 2}, Runtime::IndexVar(1));
        auto shifted_trim = Runtime::shift_free_indices(trim_exp, 1);
        auto trim = shifted_trim.to<Runtime::Trim>();
        require(bool(trim), "shifted trim expression should remain a Trim");
        require((trim->indices == std::vector<int>{1, 3}), "shifted Trim indices mismatch");
        auto trim_body = trim->body.to<Runtime::IndexVar>();
        require(bool(trim_body), "shifted Trim body should remain an IndexVar");
        require(trim_body->index == 1, "shifted Trim body index mismatch");
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
    check_shift_free_indices();
    check_inverse_preprocess_round_trip();
}
}
