#include "computation/runtime/ast.H"
#include "computation/expression/indexify.H"
#include "computation/loader.H"
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
}

int main(int argc, char** argv)
{
    assert(argc == 2);

    module_loader loader({}, {argv[1]});
    auto op = loader.load_builtin_ptr("Num", "add_int", "ecall");

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
        cereal::UserDataAdapter<const module_loader, cereal::BinaryInputArchive> archive(loader, buffer);
        archive(after);
    }

    Runtime::check_invariants(after);

    auto before_ref = Runtime::to_expression_ref(before);
    auto after_ref = Runtime::to_expression_ref(after);

    assert(before_ref.print() == after_ref.print());
}
