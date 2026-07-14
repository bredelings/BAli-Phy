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

    // Checks counting and removing a requested number of leading lambdas.
    void check_lambda_peeling()
    {
        Runtime::Exp body{Runtime::IndexVar(2)};
        Runtime::Exp one_lambda{Runtime::Lambda(body)};
        Runtime::Exp e{Runtime::Lambda(one_lambda)};

        require(Runtime::count_lambdas(e) == 2, "runtime lambda count mismatch");

        auto once = Runtime::peel_lambdas(e, 1);
        require(bool(once.to<Runtime::Lambda>()), "peeling one lambda should leave one lambda");

        auto twice = Runtime::peel_lambdas(e, 2);
        auto peeled_body = twice.to<Runtime::IndexVar>();
        require(bool(peeled_body), "peeling two lambdas should expose the body");
        require(peeled_body->index == 2, "peeled lambda body index mismatch");
    }

    // Checks that translated references survive untranslation, deindexing, and reindexing.
    void check_untranslate_vars()
    {
        Runtime::Exp translated = Runtime::apply(
            Runtime::RegRef(3), {Runtime::Trim({0}, Runtime::RegRef(4))});
        auto un = Runtime::untranslate_vars(
            translated, std::map<int, std::string>{{3, "Test.f"}, {4, "Test.x"}});
        auto untrimmed = Runtime::trim_unnormalize(un);
        auto core = deindexify(untrimmed);
        auto reindexed = indexify(core);

        require(reindexed == untrimmed,
                "runtime untranslate/deindexify should round-trip through indexify");
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

    // Checks diagnostic Core forms produced for references and Runtime-only values.
    void check_deindexify_diagnostic_terms()
    {
        auto direct_reg = deindexify(Runtime::RegRef(7));
        require(direct_reg.to_var() and direct_reg.to_var()->name == "<7>",
                "direct Runtime::RegRef should become a diagnostic Core variable");

        closure C(Runtime::IndexVar(0), {152});
        auto env_reg = deindexify(C);
        require(env_reg.to_var() and env_reg.to_var()->name == "[152]",
                "closure Runtime::IndexVar should resolve through Env to a diagnostic Core variable");

        auto free_index = deindexify(Runtime::IndexVar(0));
        require(free_index.to_var() and free_index.to_var()->name == "[?0]",
                "free Runtime::IndexVar should become a diagnostic fallback Core variable");

        object_ptr<R::RVector> vec(new R::RVector(std::vector<int>{1, 2}));
        auto object_value = deindexify(Runtime::Exp(vec));
        require(object_value.to_runtimeOnly() and object_value.print() == vec->print(),
                "Runtime::ObjectValue should become a RuntimeOnly Core diagnostic");

        auto log_double = deindexify(Runtime::LogDouble(log_double_t(0.25)));
        require(log_double.to_runtimeOnly() and log_double.print().ends_with("L#"),
                "Runtime::LogDouble should become a RuntimeOnly Core diagnostic");
    }
}

// Runs the direct Runtime indexing and Core conversion tests.
void run_transform_tests()
{
    check_shift_free_indices();
    check_lambda_peeling();
    check_untranslate_vars();
    check_inverse_preprocess_round_trip();
    check_deindexify_diagnostic_terms();
}
}
