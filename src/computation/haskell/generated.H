#ifndef HASKELL_GENERATED_H
#define HASKELL_GENERATED_H

#include <vector>

#include "computation/haskell/haskell.H"

namespace Haskell::Generated
{
    LExp Loc(const Exp&);
    LPat LocPat(const Pat&);

    Exp Apply(const Exp& head, const std::vector<Exp>& args);
    Exp List(const std::vector<Exp>& elements);
    Exp Tuple(const std::vector<Exp>& elements);

    LPat VarPat(const Var& var);
    LPat ListPat(const std::vector<LPat>& elements);
    LPat TuplePat(const std::vector<LPat>& elements);

    void Bind(Hs::Stmts& stmts, const Hs::LPat& pat, const Exp& rhs);
    void Let(Hs::Stmts& stmts, const Hs::Var& var, const Exp& rhs);
    void Let(Hs::Stmts& stmts, const Hs::LPat& pat, const Exp& rhs);
    void Expr(Hs::Stmts& stmts, const Exp& exp);
    void Return(Hs::Stmts& stmts, const Exp& exp);
    Exp Do(const Hs::Stmts& stmts);
}

#endif
