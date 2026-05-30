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

    Exp<> unpack_cpp_string(const std::string&s)
    {
        Var<> x("x");
        return Let<>({{x,Constant{s}}}, Apply<>(unpack_cpp_string(),{x}));
    }

    Exp<> error(const std::string& s)
    {
        Var<> x("x");
        return Let<>({{x, unpack_cpp_string(s)}}, Apply<>(error(),{x}));
    }

    Exp<> unsafePerformIO(const Exp<>& e)
    {
        Var<> x("x");
        return Let<>({{x, e}}, Apply<>(unsafePerformIO(),{x}));
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
