#ifndef CORE_FUNC_H
#define CORE_FUNC_H

#include "util/myexception.H"
#include "computation/core/ast.H"
#include "computation/haskell/ids.H"

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_apply(const Core::Exp<NoteV, NoteE>& E, const Core::Var<NoteV>& x)
{
    return Core::Apply<NoteV,NoteE>{E,{x}};
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_apply(const Core::Exp<NoteV, NoteE>& E, const std::vector<Core::Var<NoteV>>& xs)
{
    auto E2 = E;
    for(auto& x: xs)
	E2 = make_apply(E2,x);

    return E2;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_apply(const Core::Exp<NoteV, NoteE>& E, const Core::Exp<NoteV, NoteE>& arg)
{
    return Core::Apply<NoteV,NoteE>{E,{arg}};
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_apply(const Core::Exp<NoteV, NoteE>& E, const std::vector<Core::Exp<NoteV, NoteE>>& args)
{
    auto E2 = E;
    for(auto& arg: args)
	E2 = make_apply(E2,arg);

    return E2;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> lambda_quantify(const std::vector<Core::Var<NoteV>>& xs, Core::Exp<NoteV, NoteE> E)
{
    for(auto& x: xs | ranges::views::reverse)
	E = Core::Lambda<NoteV,NoteE>{x, E};

    return E;
}

// Wrap one declaration whose binder is scoped only over the body.
template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_nonrec_let(Core::Decl<NoteV,NoteE> decl, Core::Exp<NoteV, NoteE> body)
{
    return Core::Let<NoteV,NoteE>{Core::NonRec<NoteV,NoteE>{std::move(decl)}, std::move(body)};
}

// Wrap a possibly recursive declaration batch, omitting an empty batch.
template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_rec_let(Core::Decls<NoteV,NoteE> decls, Core::Exp<NoteV, NoteE> body)
{
    if (decls.empty()) return body;
    return Core::Let<NoteV,NoteE>{Core::Rec<NoteV,NoteE>{std::move(decls)}, std::move(body)};
}

// Nest outer-to-inner bindings around a body without changing their forms.
template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV, NoteE> make_lets(Core::Binds<NoteV,NoteE> binds, Core::Exp<NoteV, NoteE> body)
{
    for(auto& bind: binds | ranges::views::reverse)
        body = Core::Let<NoteV,NoteE>{std::move(bind), std::move(body)};
    return body;
}

template <typename NoteV = std::monostate>
std::vector<Core::Var<NoteV>> make_vars(int n, char prefix = 'x', int start=1)
{
    std::vector<Core::Var<>> xs;
    for(int i=0;i<n;i++)
        xs.push_back({prefix+std::to_string(start+i)});
    return xs;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Core::Exp<NoteV> Tuple(const std::vector<Core::Exp<NoteV,NoteE>>& args)
{
    if (args.size() == 1)
        return args[0];
    else
        return Core::ConApp<NoteV>{tuple_name(args.size()),args};
}

template <typename NoteV>
Core::Pattern<NoteV> TuplePat(const std::vector<Core::Var<NoteV>>& args)
{
    if (args.size() == 1)
        throw myexception()<<"TuplePat: can't construct a tuple of 1 element!";

    return {tuple_name(args.size()), args};
}

class FreshVarSource;

Core::Exp<> safe_apply(const Core::Exp<>& head, const std::vector<Core::Exp<>>& args);

namespace Core
{
    Var<> unpack_cpp_string();
    Var<> error();
    Var<> unsafePerformIO();

    Exp<> unpack_cpp_string(FreshVarSource&, const std::string&);
    Exp<> error(FreshVarSource&, const std::string&);
    Exp<> unsafePerformIO(FreshVarSource&, const Exp<>&);

    bool is_bool_true(const Core::Exp<>&);
    bool is_bool_false(const Core::Exp<>&);
    bool is_otherwise(const Core::Exp<>&);
}
#endif
