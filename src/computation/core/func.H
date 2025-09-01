#ifndef CORE_FUNC_H
#define CORE_FUNC_H

#include "util/myexception.H"
#include "computation/core/ast.H"
#include "computation/haskell/ids.H"

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core2::Exp<NoteV, NoteE> make_apply(const Core2::Exp<NoteV, NoteE>& E, const Core2::Var<NoteV>& x)
{
    if (auto app = E.to_apply())
    {
	auto A = *app;
	A.args.push_back(x);
	return A;
    }
    else
	return Core2::Apply<NoteV,NoteE>{E,{x}};
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core2::Exp<NoteV, NoteE> make_apply(const Core2::Exp<NoteV, NoteE>& E, const std::vector<Core2::Var<NoteV>>& xs)
{
    auto E2 = E;
    for(auto& x: xs)
	E2 = make_apply(E2,x);

    return E2;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core2::Exp<NoteV, NoteE> lambda_quantify(const std::vector<Core2::Var<NoteV>>& xs, Core2::Exp<NoteV, NoteE> E)
{
    for(auto& x: xs | ranges::views::reverse)
	E = Core2::Lambda<NoteV,NoteE>{x, E};

    return E;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core2::Exp<NoteV, NoteE> make_let(const Core2::Decls<NoteV,NoteE>& decls, const Core2::Exp<NoteV, NoteE>& body)
{
    if (decls.empty())
	return body;
    else
	return Core2::Let<NoteV,NoteE>{decls,body};
}

template <typename NoteV = std::monostate>
std::vector<Core2::Var<NoteV>> make_vars(int n, char prefix = 'x', int start=1)
{
    std::vector<Core2::Var<>> xs;
    for(int i=0;i<n;i++)
        xs.push_back({prefix+std::to_string(start+i)});
    return xs;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core2::Exp<NoteV> Tuple(const std::vector<Core2::Var<NoteV>>& vars)
{
    if (vars.size() == 1)
        return vars[0];
    else
        return Core2::ConApp<NoteV>{tuple_name(vars.size()),vars};
}

template <typename NoteV>
Core2::Pattern<NoteV> TuplePat(const std::vector<Core2::Var<NoteV>>& args)
{
    if (args.size() == 1)
        throw myexception()<<"TuplePat: can't construct a tuple of 1 element!";

    return {tuple_name(args.size()), args};
}

class FreshVarSource;

std::tuple<Core2::Decls<>, std::vector<Core2::Var<>>>
args_to_vars(const std::vector<Core2::Exp<>>& args, FreshVarSource& source);

Core2::Exp<> safe_apply(const Core2::Exp<>& head, const std::vector<Core2::Exp<>>& args, FreshVarSource& source);

namespace Core2
{
    Var<> unpack_cpp_string();
    Var<> error();
    Var<> unsafePerformIO();

    Exp<> unpack_cpp_string(const std::string&);
    Exp<> error(const std::string&);
    Exp<> unsafePerformIO(const Exp<>&);

    bool is_bool_true(const Core2::Exp<>&);
    bool is_bool_false(const Core2::Exp<>&);
    bool is_otherwise(const Core2::Exp<>&);
}
#endif
