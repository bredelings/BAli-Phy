#include "func.H"

#include "computation/fresh_vars.H"

using std::tuple;
using std::vector;

std::tuple<Core2::Decls<>, vector<Core2::Var<>>>
args_to_vars(const vector<Core2::Exp<>>& args, FreshVarSource& source)
{
    vector<Core2::Var<>> vars;
    Core2::Decls<> decls;
    for(auto& arg: args)
    {
        if (auto v = arg.to_var())
            vars.push_back(*v);
        else
        {
            auto a = source.get_fresh_core_var("a");
            decls.push_back({a,arg});
            vars.push_back(a);
        }
    }
    return {decls, vars};
}

Core2::Exp<> safe_apply(const Core2::Exp<>& head, const vector<Core2::Exp<>>& args, FreshVarSource& source)
{
    auto [decls, vars] = args_to_vars(args, source);

    return Core2::Let(decls, make_apply(head,vars));
}

namespace Core2
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
        return Let<>({{x,Constant(s)}}, Apply<>(unpack_cpp_string(),{x}));
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

    bool is_bool_true(const Core2::Exp<>& E)
    {
        return E == Core2::ConApp("Data.Bool.True",{});
    }

    bool is_bool_false(const Core2::Exp<>& E)
    {
        return E == Core2::ConApp("Data.Bool.False",{});
    }

    bool is_otherwise(const Core2::Exp<>& E)
    {
        return E == Core2::Var("Data.Bool.otherwise");
    }
}
