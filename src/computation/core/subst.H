#ifndef CORE_SUBST_H
#define CORE_SUBST_H
#include "computation/core/ast.H"
#include <immer/map.hpp>

namespace Core2
{

template <typename NoteV = std::monostate>
using subst_t = immer::map<Var<NoteV>, Var<NoteV>>;

template <typename NoteV = std::monostate>
Var<NoteV> subst_var(const subst_t<NoteV>& S, const Var<NoteV>& V)
{
    if (auto iter = S.find(V))
        return *iter;
    else
        return V;
}

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Exp<NoteV, NoteE> subst(const subst_t<NoteV>& S, const Exp<NoteV,NoteE>& E)
{
    if (S.empty()) return E;

    if (auto V = E.to_var())
        return subst_var(S, *V);
    else if (auto L = E.to_lambda())
    {
        auto S2 = S.erase(L->x);
        return Lambda<NoteV,NoteE>{L->x, subst(S2, L->body)};
    }
    else if (auto A = E.to_apply())
    {
        return Apply<NoteV,NoteE>{subst(S, A->head), subst_var(S, A->arg)};
    }
    else if (auto L = E.to_let())
    {
        auto S2 = S;
        for(auto& [x,_]: L->decls)
            S2 = S2.erase(x);

        auto decls = L->decls;
        for(auto& [_,e]: decls)
            e = subst(S2,e);

        auto body = subst(S2, L->body);
        return Let<NoteV,NoteE>{decls, body};
    }
    else if (auto C = E.to_case())
    {
        auto object = subst(S, C->object);

        auto alts = C->alts;
        for(auto& alt: alts)
        {
            auto S2 = S;
            for(auto& arg:alt.pat.args)
                S2 = S2.erase(arg);
            alt.body = subst(S2, alt.body);
        }
        return Case<NoteV,NoteE>{object, alts};
    }
    else if (auto C = E.to_conApp())
    {
        auto c2 = *C;
        for(auto& arg: c2.args)
            arg = subst_var(S, arg);
        return c2;
    }
    else if (auto B = E.to_builtinOp())
    {
        auto b2 = *C;
        for(auto& arg: b2.args)
            arg = subst_var(S, arg);
        return b2;
    }
    else if (E.to_constant())
        return E;

    std::abort();
}

}
#endif
