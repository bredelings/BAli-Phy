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

    return Core2::Let(decls, Core2::Apply(head,vars));
}

