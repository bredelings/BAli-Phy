#include "computation/haskell/generated.H"

namespace Haskell::Generated
{
    LExp Loc(const Exp& E)
    {
        return {noloc, E};
    }

    LPat LocPat(const Pat& P)
    {
        return {noloc, P};
    }

    Exp Apply(const Exp& head, const std::vector<Exp>& args)
    {
        Exp result = head;
        for(const auto& arg: args)
            result = Haskell::Apply(Loc(result), Loc(arg));
        return result;
    }

    Exp List(const std::vector<Exp>& elements)
    {
        std::vector<LExp> located_elements;
        for(const auto& element: elements)
            located_elements.push_back(Loc(element));
        return Haskell::List(located_elements);
    }

    Exp Tuple(const std::vector<Exp>& elements)
    {
        std::vector<LExp> located_elements;
        for(const auto& element: elements)
            located_elements.push_back(Loc(element));
        return Haskell::Tuple(located_elements);
    }

    LPat VarPat(const Var& var)
    {
        return {noloc, Haskell::VarPattern({noloc, var})};
    }

    LPat ListPat(const std::vector<LPat>& elements)
    {
        return {noloc, Haskell::ListPattern(elements)};
    }

    LPat TuplePat(const std::vector<LPat>& elements)
    {
        return {noloc, Haskell::TuplePattern(elements)};
    }

    // Appends a generated do-statement of the form `pat <- rhs`.
    void Bind(Hs::Stmts& stmts, const Hs::LPat& pat, const Exp& rhs)
    {
        stmts.stmts.push_back({noloc, Hs::PatQual(pat, Loc(rhs))});
    }

    // Appends a generated do-statement of the form `let var = rhs`.
    void Let(Hs::Stmts& stmts, const Hs::Var& var, const Exp& rhs)
    {
        Hs::Decls decls;
        decls.push_back({noloc, Hs::simple_decl({noloc, var}, Loc(rhs))});
        stmts.stmts.push_back({noloc, Hs::LetQual({noloc, Hs::Binds({decls})})});
    }

    // Appends a generated do-statement with an arbitrary pattern binding.
    void Let(Hs::Stmts& stmts, const Hs::LPat& pat, const Exp& rhs)
    {
        Hs::Decls decls;
        decls.push_back({noloc, Hs::PatDecl(pat, Hs::SimpleRHS(Loc(rhs)))});
        stmts.stmts.push_back({noloc, Hs::LetQual({noloc, Hs::Binds({decls})})});
    }

    void Expr(Hs::Stmts& stmts, const Exp& exp)
    {
        stmts.stmts.push_back({noloc, Hs::SimpleQual(Loc(exp))});
    }

    void Return(Hs::Stmts& stmts, const Exp& exp)
    {
        Expr(stmts, Apply(Hs::Var("return"), {exp}));
    }

    Exp Do(const Hs::Stmts& stmts)
    {
        return Hs::Do(stmts);
    }
}
