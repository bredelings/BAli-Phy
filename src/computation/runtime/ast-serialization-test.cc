#include "computation/runtime/ast.H"
#include "computation/expression/indexify.H"
#include "computation/loader.H"
#include "computation/machine/graph_register.H"
#include "computation/module.H"
#include "computation/program.H"
#include <cassert>
#include <sstream>
#include <cereal/archives/binary.hpp>

namespace
{
    Core2::Exp<> int_constant(int x)
    {
        Core2::Constant c;
        c.value = x;
        return Core2::Exp<>(c);
    }

    void check_pinned_global_translation(const std::shared_ptr<module_loader>& loader)
    {
        auto io_module = loader->load_module("Compiler.IO");
        auto program = std::make_unique<Program>(loader, std::vector<std::shared_ptr<Module>>{io_module}, "Compiler.Prim.seq");
        reg_heap heap(std::move(program));

        int global_reg = heap.add_identifier("Test.global");

        closure::Env_t global_env;
        auto global = heap.translate_refs(Runtime::make(Runtime::GlobalVar{var("Test.global")}), global_env);
        auto global_ref = std::get_if<Runtime::RegRef>(&global->value);
        assert(global_ref);
        assert(global_ref->target == global_reg);
        assert(global_env.empty());

        int local_reg = heap.allocate();
        closure::Env_t local_env;
        auto local = heap.translate_refs(Runtime::make(Runtime::RegRef{local_reg}), local_env);
        auto local_ref = std::get_if<Runtime::IndexVar>(&local->value);
        assert(local_ref);
        assert(local_ref->index == 0);
        assert(local_env.size() == 1);
        assert(local_env[0] == local_reg);
    }

    void check_local_reg_refs_are_captured_before_trimming(const std::shared_ptr<module_loader>& loader)
    {
        auto io_module = loader->load_module("Compiler.IO");
        auto program = std::make_unique<Program>(loader, std::vector<std::shared_ptr<Module>>{io_module}, "Compiler.Prim.seq");
        reg_heap heap(std::move(program));

        int local_reg = heap.allocate();
        auto bind = Runtime::make(Runtime::RegRef{local_reg});
        auto body = Runtime::make(Runtime::IndexVar{0});
        auto local_let = Runtime::make(Runtime::Let{{bind}, body});

        auto C = heap.preprocess(local_let);
        assert(C.Env.size() == 1);
        assert(C.Env[0] == local_reg);
    }
}

int main(int argc, char** argv)
{
    assert(argc == 3);

    auto loader = std::make_shared<module_loader>(std::optional<std::filesystem::path>{}, std::vector<std::filesystem::path>{argv[1], argv[2]});
    auto op = loader->load_builtin_ptr("Num", "add_int", "ecall");

    std::vector<Core2::Exp<>> args = {int_constant(1), int_constant(2)};
    Core2::BuiltinOp<> builtin("Num", "add_int", "ecall", args, op);

    auto before = runtime_indexify(Core2::Exp<>(builtin));
    Runtime::check_invariants(before);

    std::stringstream buffer;
    {
        cereal::BinaryOutputArchive archive(buffer);
        archive(before);
    }

    Runtime::ExpPtr after;
    {
        cereal::UserDataAdapter<const module_loader, cereal::BinaryInputArchive> archive(*loader, buffer);
        archive(after);
    }

    Runtime::check_invariants(after);

    auto before_ref = Runtime::to_expression_ref(before);
    auto after_ref = Runtime::to_expression_ref(after);

    assert(before_ref.print() == after_ref.print());

    check_pinned_global_translation(loader);
    check_local_reg_refs_are_captured_before_trimming(loader);
}
