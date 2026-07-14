#include "func.H"

#include "computation/fresh_vars.H"

using std::tuple;
using std::vector;

Core::Exp<> safe_apply(const Core::Exp<>& head, const vector<Core::Exp<>>& args)
{
    auto E = head;
    for(auto& arg: args)
        E = Core::Apply<>{E,arg};

    return E;
}

namespace Core
{
    Var<> unpack_cpp_string()
    {
        return Var<>("Foreign.String.unpack_cpp_string");
    }

    Var<> error()
    {
        return Var<>("Compiler.Error.error");
    }

    Var<> unsafePerformIO()
    {
        return Var<>("Compiler.IO.unsafePerformIO");
    }

    // Bind a C++ string value before applying the Haskell unpacking function.
    Exp<> unpack_cpp_string(FreshVarSource& source, const std::string& s)
    {
        Exp<> rhs = Constant{s};
        auto x = source.get_fresh_core_var("x");
        return make_nonrec_let<>(Core::Decl<>{x, std::move(rhs)},
                                 Exp<>(Apply<>(unpack_cpp_string(), {x})));
    }

    // Bind an unpacked message before applying the Haskell error function.
    Exp<> error(FreshVarSource& source, const std::string& s)
    {
        auto rhs = unpack_cpp_string(source, s);
        auto x = source.get_fresh_core_var("x");
        return make_nonrec_let<>(Core::Decl<>{x, std::move(rhs)},
                                 Exp<>(Apply<>(error(), {x})));
    }

    // Bind an existing IO expression before applying unsafePerformIO.
    Exp<> unsafePerformIO(FreshVarSource& source, const Exp<>& e)
    {
        auto x = source.get_fresh_core_var("x");
        return make_nonrec_let<>(Core::Decl<>{x, e},
                                 Exp<>(Apply<>(unsafePerformIO(), {x})));
    }

    bool is_bool_true(const Core::Exp<>& E)
    {
        return E == Core::ConApp("Data.Bool.True",{});
    }

    bool is_bool_false(const Core::Exp<>& E)
    {
        return E == Core::ConApp("Data.Bool.False",{});
    }

    bool is_otherwise(const Core::Exp<>& E)
    {
        return E == Core::Var("Data.Bool.otherwise");
    }
}
