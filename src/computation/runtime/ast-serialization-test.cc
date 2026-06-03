#include "computation/runtime/ast.H"
#include "computation/expression/indexify.H"
#include "computation/loader.H"
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "computation/program.H"
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

    void check_pinned_global_translation(const std::shared_ptr<module_loader>& loader)
    {
        auto io_module = loader->load_module("Compiler.IO");
        auto program = std::make_unique<Program>(loader, std::vector<std::shared_ptr<Module>>{io_module}, "Compiler.Prim.seq");
        reg_heap heap(std::move(program));

        int global_reg = heap.add_identifier("Test.global");

        closure::Env_t global_env;
        auto global = heap.translate_refs(Runtime::GlobalVar(var("Test.global")), global_env);
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
        closure C(Runtime::Trim({0, 2}, Runtime::IndexVar(1)), {10, 20, 30});

        do_trim(C);

        require(C.has_code(), "trimmed runtime closure should preserve Runtime::Exp code");
        auto body = C.get_code().to<Runtime::IndexVar>();
        require(bool(body), "trimmed runtime closure should use the Runtime::Trim body");
        require(body->index == 1, "trimmed runtime closure body index mismatch");
        require((C.Env == closure::Env_t{10, 30}), "trimmed runtime closure environment mismatch");
        require(Runtime::to_expression_ref(C.get_code()) == C.exp, "trimmed runtime closure cache mismatch");
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
        require(Runtime::to_expression_ref(C2.get_code()) == C2.exp, "trim_unnormalized runtime closure cache mismatch");
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

    void check_deindexify_reg_refs()
    {
        auto reg_ref = deindexify(Runtime::to_expression_ref(Runtime::RegRef(7)));
        require(reg_ref.is_reg_var(), "deindexify should preserve RegRef as reg_var");
        require(reg_ref.as_reg_var() == 7, "deindexified reg_var target mismatch");
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
        constructor c("Data.Bool.True", 0);

        Runtime::Exp literal = Runtime::Constructor(c);
        check_archive_roundtrip(literal);

        Runtime::Exp app = Runtime::App(Runtime::ConstructorApp(c), {});
        check_archive_roundtrip(app);

        Runtime::Exp case_ = Runtime::Case(Runtime::Constructor(c),
                                           {Runtime::Alt(Runtime::ConstructorPattern(c),
                                                         Runtime::Int(1))});
        check_archive_roundtrip(case_);
    }
}

int main(int argc, char** argv)
{
    require(argc == 3, "expected builtin and package paths");

    auto loader = std::make_shared<module_loader>(std::optional<std::filesystem::path>{}, std::vector<std::filesystem::path>{argv[1], argv[2]});
    auto op = loader->load_builtin_ptr("Num", "add_int", "ecall");

    std::vector<Core::Exp<>> args = {int_constant(1), int_constant(2)};
    Core::BuiltinOp<> builtin("Num", "add_int", "ecall", args, op);

    auto before = runtime_indexify(Core::Exp<>(builtin));
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

    auto before_ref = Runtime::to_expression_ref(before);
    auto after_ref = Runtime::to_expression_ref(after);

    require(before_ref.print() == after_ref.print(), "Runtime AST serialization changed the expression");

    check_runtime_closure_trim();
    check_runtime_closure_trim_unnormalize();
    check_shift_free_indices();
    check_deindexify_reg_refs();
    check_constructor_serialization();
}
