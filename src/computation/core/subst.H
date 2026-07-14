#ifndef CORE_SUBST_H
#define CORE_SUBST_H
#include "computation/core/ast.H"
#include <immer/map.hpp>

namespace Core
{

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
using subst_t = immer::map<Var<NoteV>, Exp<NoteV,NoteE>>;

template <typename NoteV = std::monostate, typename NoteE = std::monostate>
Exp<NoteV, NoteE> subst(const subst_t<NoteV>& S, const Exp<NoteV,NoteE>& E)
{
    if (S.empty()) return E;

    if (auto V = E.to_var())
    {
        if (auto iter = S.find(*V))
            return *iter;
        else
            return E;
    }
    else if (auto L = E.to_lambda())
    {
        auto S2 = S.erase(L->x);
        return Lambda<NoteV,NoteE>{L->x, subst(S2, L->body)};
    }
    else if (auto A = E.to_apply())
    {
        return Apply<NoteV,NoteE>{subst(S, A->head), subst(S, A->arg)};
    }
    else if (auto L = E.to_let())
    {
        if (auto nonrec = L->to_nonrec())
        {
            auto decl = nonrec->decl;
            decl.body = subst(S, decl.body);
            auto body = subst(S.erase(decl.x), L->body);
            return Let<NoteV,NoteE>{NonRec<NoteV,NoteE>{std::move(decl)}, std::move(body)};
        }

        auto S2 = S;
        auto decls = L->to_rec()->decls;
        for(auto& [x,_]: decls)
            S2 = S2.erase(x);

        for(auto& [_,e]: decls)
            e = subst(S2,e);

        auto body = subst(S2, L->body);
        return Let<NoteV,NoteE>{Rec<NoteV,NoteE>{std::move(decls)}, std::move(body)};
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
            arg = subst(S, arg);
        return c2;
    }
    else if (auto B = E.to_builtinOp())
    {
        auto b2 = *B;
        for(auto& arg: b2.args)
            arg = subst(S, arg);
        return b2;
    }
    else if (E.to_constant())
        return E;
    else if (E.to_runtimeOnly())
        return E;

    std::abort();
}

}
#endif
