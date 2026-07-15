#include "computation/loader.H"
#include "computation/machine/graph_register.H"
#include "computation/operation.H"
#include "computation/preprocess.H"
#include "computation/runtime/ast.H"
#include "computation/runtime/indexify.H"
#include "test-util.H"
#include <cereal/archives/binary.hpp>
#include <sstream>

namespace bali_phy_test
{
namespace
{
    // Constructs a Core integer constant for builtin serialization tests.
    Core::Exp<> int_constant(int x)
    {
        Core::Constant c;
        c.value = x;
        return Core::Exp<>(c);
    }

    closure runtime_only_test_operation(OperationArgs&)
    {
        return {};
    }

    // Serializes and deserializes a Runtime expression without external loader data.
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

    // Checks that a Runtime expression retains its printed representation after serialization.
    void check_archive_roundtrip(const Runtime::Exp& before)
    {
        auto after = archive_roundtrip(before);
        if (Runtime::print(after) != Runtime::print(before))
            throw std::runtime_error("Runtime AST archive roundtrip mismatch");
    }

    // Checks serialization of constructors, cases, and both Runtime binding forms.
    void check_constructor_serialization()
    {
        Runtime::Exp literal = Runtime::ConstructorApp("Data.Bool.True", 0, {});
        check_archive_roundtrip(literal);

        Runtime::Exp app = Runtime::ConstructorApp("Data.Bool.True", 0, {});
        check_archive_roundtrip(app);

        Runtime::Exp case_ = Runtime::Case(
            Runtime::ConstructorApp("Data.Bool.True", 0, {}),
            {Runtime::Alt(Runtime::ConstructorPattern("Data.Bool.True", 0), Runtime::Int(1))});
        check_archive_roundtrip(case_);

        Runtime::Exp nonrec = Runtime::Let(
            Runtime::NonRec{Runtime::Int(1)}, Runtime::IndexVar(0));
        require(archive_roundtrip(nonrec) == nonrec,
                "Runtime NonRec should survive serialization");

        Runtime::Exp rec = Runtime::Let(
            Runtime::Rec({Runtime::IndexVar(0)}), Runtime::IndexVar(0));
        require(archive_roundtrip(rec) == rec,
                "Runtime Rec should survive serialization");
    }

    // Checks that cereal preserves aliases between boxed Runtime nodes.
    void check_intrusive_ptr_serialization_preserves_aliases()
    {
        Runtime::Exp shared = Runtime::String("shared");
        Runtime::Exp before = Runtime::ConstructorApp("Pair", 2, {shared, shared});

        auto before_app = before.to<Runtime::ConstructorApp>();
        require(before_app->args[0].to<Runtime::String>() ==
                    before_app->args[1].to<Runtime::String>(),
                "test setup should share one boxed Runtime::String node");

        auto after = archive_roundtrip(before);
        auto after_app = after.to<Runtime::ConstructorApp>();
        require(bool(after_app),
                "alias serialization test should round-trip to a Runtime::ConstructorApp");
        require(after_app->args[0].to<Runtime::String>() ==
                    after_app->args[1].to<Runtime::String>(),
                "Runtime intrusive pointer serialization should preserve aliases");
    }

    // Checks that Runtime-only operations fail serialization with a stable diagnostic.
    void check_runtime_only_operation_app_serialization()
    {
        Runtime::Exp app = Runtime::OperationApp(
            new Operation(runtime_only_test_operation, "runtimeOnly"), {});

        bool threw = false;
        try
        {
            archive_roundtrip(app);
        }
        catch(const std::runtime_error& e)
        {
            threw = std::string(e.what()) ==
                    "Runtime-only OperationApp is not serializable";
        }

        require(threw, "runtime-only OperationApp serialization should throw a clear error");
    }

    // Checks builtin serialization using loader metadata and inverse preprocessing.
    void check_builtin_serialization(const std::shared_ptr<module_loader>& loader)
    {
        auto op = loader->load_builtin_ptr("Num", "add_int", "ecall");
        std::vector<Core::Exp<>> args = {int_constant(1), int_constant(2)};
        Core::BuiltinOp<> builtin("Num", "add_int", "ecall", args, op);

        auto before = indexify(Core::Exp<>(builtin));

        std::stringstream buffer;
        {
            cereal::BinaryOutputArchive archive(buffer);
            archive(before);
        }

        Runtime::Exp after;
        {
            cereal::UserDataAdapter<const module_loader, cereal::BinaryInputArchive> archive(
                *loader, buffer);
            archive(after);
        }

        require(before == after, "Runtime AST serialization changed the expression");

        auto recovered_builtin = unprepare_for_translation(before);
        auto reindexed_builtin = indexify(recovered_builtin);
        require(reindexed_builtin == before,
                "runtime builtin deindexify should round-trip through indexify");
    }
}

// Runs Runtime serialization tests, including builtins that require loader metadata.
void run_serialization_tests(const std::shared_ptr<module_loader>& loader)
{
    check_builtin_serialization(loader);
    check_constructor_serialization();
    check_intrusive_ptr_serialization_preserves_aliases();
    check_runtime_only_operation_app_serialization();
}
}
