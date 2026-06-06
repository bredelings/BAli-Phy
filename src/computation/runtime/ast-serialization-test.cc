#include "computation/runtime/ast.H"
#include "computation/runtime/trim.H"
#include "computation/runtime/indexify.H"
#include "computation/preprocess.H"
#include "computation/loader.H"
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "computation/operation.H"
#include "computation/program.H"
#include <limits>
#include <stdexcept>
#include <sstream>
#include <cereal/archives/binary.hpp>

namespace
{
    void require(bool condition, const std::string& message)
    {
        if (not condition)
            throw std::runtime_error(message);
    }

    Core::Exp<> int_constant(int x)
    {
        Core::Constant c;
        c.value = x;
        return Core::Exp<>(c);
    }

    Core::Exp<> inverse_preprocess_test_core()
    {
        Core::Var<> x("x");
        Core::Var<> y("y");
        Core::Var<> p("p");
        Core::Var<> q("q");

        Core::Decls<> decls;
        decls.push_back({y, Core::ConApp<>{ "Pair", {x, int_constant(3)} }});

        Core::Pattern<> pair_pat;
        pair_pat.head = "Pair";
        pair_pat.args = {p, q};

        Core::Alt<> pair_alt{pair_pat, Core::ConApp<>{ "Pair", {q, p} }};
        Core::Alt<> wildcard_alt{{}, x};

        Core::Exp<> body = Core::Let<>{decls, Core::Exp<>(Core::Case<>{y, {pair_alt, wildcard_alt}})};
        return Core::Lambda<>{x, body};
    }

    closure runtime_only_test_operation(OperationArgs&)
    {
        return {};
    }

    void check_pinned_global_translation(const std::shared_ptr<module_loader>& loader)
    {
        auto io_module = loader->load_module("Compiler.IO");
        auto program = std::make_unique<Program>(loader, std::vector<std::shared_ptr<Module>>{io_module}, "Compiler.Prim.seq");
        reg_heap heap(std::move(program));

        int global_reg = heap.add_identifier("Test.global");

        closure::Env_t global_env;
        auto global = heap.translate_refs(Runtime::GlobalVar("Test.global"), global_env);
        auto global_ref = global.to<Runtime::RegRef>();
        require(bool(global_ref), "global variable should translate to a RegRef");
        require(global_ref->target == global_reg, "global variable RegRef target mismatch");
        require(global_env.empty(), "global variable should not be captured in the closure environment");

        int local_reg = heap.allocate();
        closure::Env_t local_env;
        auto local = heap.translate_refs(Runtime::RegRef(local_reg), local_env);
        auto local_ref = local.to<Runtime::IndexVar>();
        require(bool(local_ref), "local RegRef should translate to an IndexVar");
        require(local_ref->index == 0, "local RegRef IndexVar slot mismatch");
        require(local_env.size() == 1, "local RegRef should add one closure environment entry");
        require(local_env[0] == local_reg, "local RegRef closure environment entry mismatch");
    }

    void check_local_reg_refs_are_captured_before_trimming(const std::shared_ptr<module_loader>& loader)
    {
        auto io_module = loader->load_module("Compiler.IO");
        auto program = std::make_unique<Program>(loader, std::vector<std::shared_ptr<Module>>{io_module}, "Compiler.Prim.seq");
        reg_heap heap(std::move(program));

        int local_reg = heap.allocate();
        Runtime::Exp bind = Runtime::RegRef(local_reg);
        Runtime::Exp body = Runtime::IndexVar(0);
        Runtime::Exp local_let = Runtime::Let({bind}, body);

        auto C = heap.preprocess(local_let);
        require(C.Env.size() == 1, "preprocess should capture one local RegRef");
        require(C.Env[0] == local_reg, "preprocess captured the wrong local RegRef");
    }

    void check_runtime_closure_trim()
    {
        auto C = get_trimmed(Runtime::Trim({0, 2}, Runtime::IndexVar(1)), {10, 20, 30});

        require(C.has_code(), "trimmed runtime closure should preserve Runtime::Exp code");
        auto body = C.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trimmed runtime closure should use the Runtime::Trim body");
        require(body->index == 1, "trimmed runtime closure body index mismatch");
        require((C.Env == closure::Env_t{10, 30}), "trimmed runtime closure environment mismatch");
    }

    void check_runtime_closure_trim_unnormalize()
    {
        closure C(Runtime::Trim({0, 2}, Runtime::IndexVar(1)), {10, 20, 30});

        auto C2 = trim_unnormalize(C);

        require(C2.has_code(), "trim_unnormalize should preserve Runtime::Exp code");
        auto body = C2.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trim_unnormalize should remove the Runtime::Trim wrapper");
        require(body->index == 2, "trim_unnormalize should remap body indices through Trim indices");
        require((C2.Env == closure::Env_t{10, 20, 30}), "trim_unnormalize should preserve the closure environment");
    }

    void check_runtime_closure_slots()
    {
        closure C(Runtime::App(Runtime::FunctionApply{},
                               {Runtime::IndexVar(1),
                                Runtime::IndexVar(0),
                                Runtime::RegRef(30)}),
                  {10, 20});

        require(C.n_slots() == 3, "runtime closure App slot count mismatch");
        require(C.reg_for_slot(0) == 10, "runtime slot 0 should resolve through the closure environment");
        require(C.reg_for_slot(1) == 20, "runtime slot 1 should resolve through the closure environment");
        require(C.reg_for_slot(2) == 30, "runtime RegRef slot should preserve its target");

        require(closure(Runtime::IndexVar(1), {10, 20}).reg_for_ref_maybe() == 10,
                "runtime closure IndexVar ref should resolve through the closure environment");
        require(closure(Runtime::RegRef(30), {10, 20}).reg_for_ref_maybe() == 30,
                "runtime closure RegRef should preserve its target");
        require(not closure(Runtime::Int(1)).reg_for_ref_maybe(),
                "non-reference runtime closure should not have a referenced reg");

        auto slot = C.slot(1);
        auto slot_ref = slot.to<Runtime::RegRef>();
        require(bool(slot_ref), "runtime IndexVar slot should become a RegRef");
        require(slot_ref->target == 20, "runtime slot RegRef target mismatch");
    }

    void check_shift_free_indices()
    {
        Runtime::Exp e = Runtime::Let({Runtime::IndexVar(1)},
                                      Runtime::apply(Runtime::IndexVar(1),
                                                     {Runtime::IndexVar(0),
                                                      Runtime::RegRef(7)}));

        auto shifted = Runtime::shift_free_indices(e, 1);
        auto let = shifted.to<Runtime::Let>();
        require(bool(let), "shifted expression should remain a Let");

        auto bind = let->binds[0].to<Runtime::IndexVar>();
        require(bool(bind), "shifted Let bind should remain an IndexVar");
        require(bind->index == 2, "shifted Let bind index mismatch");

        auto app = let->body.to<Runtime::App>();
        require(bool(app), "shifted Let body should remain an App");
        auto fn = app->args[0].to<Runtime::IndexVar>();
        auto bound_arg = app->args[1].to<Runtime::IndexVar>();
        auto reg_ref = app->args[2].to<Runtime::RegRef>();
        require(bool(fn), "shifted App function should remain an IndexVar");
        require(bool(bound_arg), "bound App argument should remain an IndexVar");
        require(bool(reg_ref), "RegRef App argument should remain a RegRef");
        require(fn->index == 2, "shifted App function index mismatch");
        require(bound_arg->index == 0, "bound App argument index mismatch");
        require(reg_ref->target == 7, "RegRef App argument target mismatch");

        Runtime::Exp trim_exp = Runtime::Trim({0, 2}, Runtime::IndexVar(1));
        auto shifted_trim = Runtime::shift_free_indices(trim_exp, 1);
        auto trim = shifted_trim.to<Runtime::Trim>();
        require(bool(trim), "shifted trim expression should remain a Trim");
        require((trim->indices == std::vector<int>{1, 3}), "shifted Trim indices mismatch");
        auto trim_body = trim->body.to<Runtime::IndexVar>();
        require(bool(trim_body), "shifted Trim body should remain an IndexVar");
        require(trim_body->index == 1, "shifted Trim body index mismatch");
    }

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

    void check_runtime_untranslate_vars()
    {
        Runtime::Exp translated = Runtime::apply(Runtime::RegRef(3),
                                                 {Runtime::Trim({0}, Runtime::RegRef(4))});
        auto un = Runtime::untranslate_vars(translated, std::map<int,std::string>{{3, "Test.f"}, {4, "Test.x"}});
        auto untrimmed = Runtime::trim_unnormalize(un);
        auto core = deindexify(untrimmed);
        auto reindexed = indexify(core);

        require(reindexed == untrimmed, "runtime untranslate/deindexify should round-trip through indexify");
    }

    void check_runtime_inverse_preprocess_round_trip()
    {
        auto original = inverse_preprocess_test_core();

        FreshVarState fresh1;
        auto prepared = prepare_for_translation(fresh1, original);

        auto untrimmed = Runtime::trim_unnormalize(prepared);
        auto deindexed = deindexify(untrimmed);
        auto reindexed = indexify(deindexed);
        require(reindexed == untrimmed, "deindexify should round-trip with indexify after trim_unnormalize");

        auto recovered = unprepare_for_translation(prepared);

        FreshVarState fresh2;
        auto prepared_again = prepare_for_translation(fresh2, recovered);
        require(prepared_again == prepared, "inverse preprocess final Core should prepare back to the same Runtime expression");
    }

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

    void check_runtime_atomic_values()
    {
        Runtime::Exp i = 7;
        require(i.is_atomic_value(), "runtime Int should be atomic");
        require(i.as_int() == 7, "runtime Int value mismatch");

        Runtime::Exp d = 2.5;
        require(d.is_atomic_value(), "runtime Double should be atomic");
        require(d.as_double() == 2.5, "runtime Double value mismatch");

        auto ld_value = log_double_t(0.25);
        Runtime::Exp ld = ld_value;
        require(ld.is_atomic_value(), "runtime LogDouble should be atomic");
        require(ld.as_log_double() == ld_value, "runtime LogDouble value mismatch");

        Runtime::Exp c = 'x';
        require(c.is_atomic_value(), "runtime Char should be atomic");
        require(c.as_char() == 'x', "runtime Char value mismatch");

        Runtime::Exp s = std::string("runtime");
        require(s.is_atomic_value(), "runtime String should be atomic");
        require(s.as_string() == "runtime", "runtime String value mismatch");

        Runtime::Exp n = integer("12345678901234567890");
        require(n.is_atomic_value(), "runtime Integer should be atomic");
        require(n.as_integer() == integer("12345678901234567890"), "runtime Integer value mismatch");

        Runtime::Exp b = Runtime::Constructor("Data.Bool.True", 0);
        require(b.is_atomic_value(), "runtime Constructor should be atomic");
        require(b.as_constructor().name() == "Data.Bool.True", "runtime Constructor name mismatch");
        require(b.as_constructor().n_args() == 0, "runtime Constructor arity mismatch");

        Runtime::Exp vector_value = R::RVector(std::vector<int>{1, 2});
        require(vector_value.is_atomic_value(), "runtime ObjectValue should be atomic");
        require(vector_value.as_<R::RVector>().size() == 2, "runtime ObjectValue value mismatch");

        Runtime::Exp pair = Runtime::App(Runtime::ConstructorApp("Pair", 2),
                                         {Runtime::Exp(1), Runtime::Exp("field")});
        require(pair.is_value(), "constructor applications should be runtime values");
        auto app = pair.to<Runtime::App>();
        require(bool(app), "constructor application should convert to Runtime::App");
        require(std::holds_alternative<Runtime::ConstructorApp>(app->head), "constructor application should use ConstructorApp");
        require(app->args.size() == 2, "constructor application arity mismatch");
        require(app->args[0].as_int() == 1, "constructor application int field mismatch");
        require(app->args[1].as_string() == "field", "constructor application string field mismatch");
    }

    void check_runtime_vector_conversions()
    {
        Runtime::RVector ints(std::vector<int>{1, 2, 3});
        require(((std::vector<int>)ints == std::vector<int>{1, 2, 3}), "Runtime::RVector int conversion mismatch");

        Runtime::RVector doubles(std::vector<double>{1.5, 2.5});
        require(((std::vector<double>)doubles == std::vector<double>{1.5, 2.5}), "Runtime::RVector double conversion mismatch");

        Runtime::RVector chars(std::vector<char>{'a', 'b'});
        require(((std::vector<char>)chars == std::vector<char>{'a', 'b'}), "Runtime::RVector char conversion mismatch");
    }

    Runtime::Exp archive_roundtrip(const Runtime::Exp& before)
    {
        std::stringstream buffer;
        {
            cereal::BinaryOutputArchive archive(buffer);
            archive(before);
        }

        Runtime::Exp after;
        {
            cereal::BinaryInputArchive archive(buffer);
            archive(after);
        }

        return after;
    }

    void check_archive_roundtrip(const Runtime::Exp& before)
    {
        auto after = archive_roundtrip(before);
        if (Runtime::print(after) != Runtime::print(before))
            throw std::runtime_error("Runtime AST archive roundtrip mismatch");
    }

    void check_constructor_serialization()
    {
        Runtime::Exp literal = Runtime::Constructor("Data.Bool.True", 0);
        check_archive_roundtrip(literal);

        Runtime::Exp app = Runtime::App(Runtime::ConstructorApp("Data.Bool.True", 0), {});
        check_archive_roundtrip(app);

        Runtime::Exp case_ = Runtime::Case(Runtime::Constructor("Data.Bool.True", 0),
                                           {Runtime::Alt(Runtime::ConstructorPattern("Data.Bool.True", 0),
                                                         Runtime::Int(1))});
        check_archive_roundtrip(case_);
    }

    void check_intrusive_ptr_serialization_preserves_aliases()
    {
        Runtime::Exp shared = Runtime::String("shared");
        Runtime::Exp before = Runtime::App(Runtime::ConstructorApp("Pair", 2), {shared, shared});

        auto before_app = before.to<Runtime::App>();
        require(before_app->args[0].to<Runtime::String>() == before_app->args[1].to<Runtime::String>(),
                "test setup should share one boxed Runtime::String node");

        auto after = archive_roundtrip(before);
        auto after_app = after.to<Runtime::App>();
        require(bool(after_app), "alias serialization test should round-trip to a Runtime::App");
        require(after_app->args[0].to<Runtime::String>() == after_app->args[1].to<Runtime::String>(),
                "Runtime intrusive pointer serialization should preserve aliases");
    }

    void check_runtime_only_operation_app_serialization()
    {
        Runtime::Exp app = Runtime::App(Runtime::OperationApp(std::make_shared<Operation>(runtime_only_test_operation, "runtimeOnly")),
                                        {});

        bool threw = false;
        try
        {
            archive_roundtrip(app);
        }
        catch(const std::runtime_error& e)
        {
            threw = std::string(e.what()) == "Runtime-only OperationApp is not serializable";
        }

        require(threw, "runtime-only OperationApp serialization should throw a clear error");
    }

    void check_runtime_exp_equality()
    {
        Runtime::Exp e1 = Runtime::Let({Runtime::Int(1), Runtime::String("field")},
                                       Runtime::App(Runtime::ConstructorApp("Pair", 2),
                                                    {Runtime::IndexVar(0), Runtime::IndexVar(1)}));
        Runtime::Exp e2 = Runtime::Let({Runtime::Int(1), Runtime::String("field")},
                                       Runtime::App(Runtime::ConstructorApp("Pair", 2),
                                                    {Runtime::IndexVar(0), Runtime::IndexVar(1)}));
        Runtime::Exp e3 = Runtime::Let({Runtime::Int(2), Runtime::String("field")},
                                       Runtime::App(Runtime::ConstructorApp("Pair", 2),
                                                    {Runtime::IndexVar(0), Runtime::IndexVar(1)}));

        require(e1 == e2, "matching Runtime::Exp trees should compare equal");
        require(not (e1 == e3), "different Runtime::Exp trees should not compare equal");
        require(not (Runtime::Constructor("Pair", 2) == Runtime::Constructor("Pair", 1)),
                "Runtime constructor equality should include arity");

        const double nan = std::numeric_limits<double>::quiet_NaN();
        require(Runtime::Exp(Runtime::Double(nan)) == Runtime::Exp(Runtime::Double(nan)),
                "Runtime::Double NaNs should compare equal");
        require(Runtime::Exp(Runtime::LogDouble(log_double_t(nan))) == Runtime::Exp(Runtime::LogDouble(log_double_t(nan))),
                "Runtime::LogDouble NaNs should compare equal");
    }

}

int main(int argc, char** argv)
{
    require(argc == 3, "expected builtin and package paths");

    auto loader = std::make_shared<module_loader>(std::optional<std::filesystem::path>{}, std::vector<std::filesystem::path>{argv[1], argv[2]});
    auto op = loader->load_builtin_ptr("Num", "add_int", "ecall");

    std::vector<Core::Exp<>> args = {int_constant(1), int_constant(2)};
    Core::BuiltinOp<> builtin("Num", "add_int", "ecall", args, op);

    auto before = indexify(Core::Exp<>(builtin));
    Runtime::check_invariants(before);

    std::stringstream buffer;
    {
        cereal::BinaryOutputArchive archive(buffer);
        archive(before);
    }

    Runtime::Exp after;
    {
        cereal::UserDataAdapter<const module_loader, cereal::BinaryInputArchive> archive(*loader, buffer);
        archive(after);
    }

    Runtime::check_invariants(after);

    require(before == after, "Runtime AST serialization changed the expression");

    auto recovered_builtin = unprepare_for_translation(before);
    auto reindexed_builtin = indexify(recovered_builtin);
    require(reindexed_builtin == before, "runtime builtin deindexify should round-trip through indexify");

    check_runtime_closure_trim();
    check_runtime_closure_trim_unnormalize();
    check_runtime_closure_slots();
    check_shift_free_indices();
    check_lambda_peeling();
    check_runtime_untranslate_vars();
    check_runtime_inverse_preprocess_round_trip();
    check_deindexify_diagnostic_terms();
    check_runtime_atomic_values();
    check_runtime_vector_conversions();
    check_constructor_serialization();
    check_intrusive_ptr_serialization_preserves_aliases();
    check_runtime_only_operation_app_serialization();
    check_runtime_exp_equality();
}
