#include <string>
#include <vector>
#include <set>

#include "rename.H"
#include "haskell/ids.H"
#include "computation/module.H"
#include "util/set.H"

// We really should move apply expressions into the haskell ast.
#include "computation/expression/apply.H"

using std::string;
using std::vector;
using std::pair;
using std::set;

expression_ref rename_infix(const Module& m, const expression_ref& E)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename_infix(m, element);
        return L;
    }
    else if (E.is_a<Haskell::ListFrom>())
    {
        auto L = E.as_<Haskell::ListFrom>();
        L.from = rename_infix(m, L.from);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThen>())
    {
        auto L = E.as_<Haskell::ListFromThen>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        return L;
    }
    else if (E.is_a<Haskell::ListFromTo>())
    {
        auto L = E.as_<Haskell::ListFromTo>();
        L.from = rename_infix(m, L.from);
        L.to   = rename_infix(m, L.to);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThenTo>())
    {
        auto L = E.as_<Haskell::ListFromThenTo>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        L.to   = rename_infix(m, L.to);
        return L;
    }
    else if (E.is_a<Haskell::ListComprehension>())
    {
        auto L = E.as_<Haskell::ListComprehension>();
        L.body = rename_infix(m, L.body);
        for(auto& qual: L.quals)
            qual = rename_infix(m, qual);
        return L;
    }
    else if (E.is_a<Haskell::LeftSection>())
    {
        auto S = E.as_<Haskell::LeftSection>();
        S.l_arg = rename_infix(m, S.l_arg);
        return S;
    }
    else if (E.is_a<Haskell::RightSection>())
    {
        auto S = E.as_<Haskell::RightSection>();
        S.r_arg = rename_infix(m, S.r_arg);
        return S;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = rename_infix(m, element);
        return T;
    }
    else if (E.is_a<Haskell::PatQual>())
    {
        auto PQ = E.as_<Haskell::PatQual>();

        PQ.bindpat = rename_infix(m, PQ.bindpat);
        PQ.bindpat = unapply(PQ.bindpat);

        PQ.exp = rename_infix(m, PQ.exp);

        return PQ;
    }
    else if (E.is_a<Haskell::SimpleQual>())
    {
        auto SQ = E.as_<Haskell::SimpleQual>();
        SQ.exp = rename_infix(m, SQ.exp);
        return SQ;
    }
    else if (E.is_a<Haskell::LetQual>())
    {
        auto LQ = E.as_<Haskell::LetQual>();
        unloc(LQ.binds) = rename_infix(m, unloc(LQ.binds));
        return LQ;
    }
    else if (E.is_a<Haskell::AsPattern>())
    {
        auto& AP = E.as_<Haskell::AsPattern>();
        return Haskell::AsPattern(AP.var, rename_infix(m,AP.pattern));
    }
    else if (E.is_a<Haskell::LazyPattern>())
    {
        auto LP = E.as_<Haskell::LazyPattern>();
        return Haskell::LazyPattern(rename_infix(m,LP.pattern));
    }
    else if (E.is_a<Haskell::StrictPattern>())
    {
        auto SP = E.as_<Haskell::StrictPattern>();
        SP.pattern = rename_infix(m, SP.pattern);
        return SP;
    }
    else if (E.is_a<Haskell::RecStmt>())
    {
        auto R = E.as_<Haskell::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return R;
    }
    else if (E.is_a<Haskell::Do>())
    {
        auto D = E.as_<Haskell::Do>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return D;
    }
    else if (E.is_a<Haskell::MDo>())
    {
        throw myexception()<<"mdo is not handled yet!";
        auto D = E.as_<Haskell::MDo>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        return D;
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();
        for(auto& pat: L.match.patterns)
            pat = unapply(rename_infix(m, pat));
        L.match.rhs = rename_infix(m, L.match.rhs);

        return L;
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto L = E.as_<Haskell::LetExp>();

        unloc(L.binds) = rename_infix(m, unloc(L.binds));
        unloc(L.body)  = rename_infix(m, unloc(L.body));

        return L;
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto I = E.as_<Haskell::IfExp>();
        unloc(I.condition) = rename_infix(m, unloc(I.condition));
        unloc(I.true_branch) = rename_infix(m, unloc(I.true_branch));
        unloc(I.false_branch) = rename_infix(m, unloc(I.false_branch));
        return I;
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto C = E.as_<Haskell::CaseExp>();

        C.object = rename_infix(m, C.object);

        for(auto& [patterns, rhs]: C.alts)
        {
            patterns[0] = rename_infix(m, patterns[0]);
            patterns[0] = unapply(patterns[0]);
            rhs = rename_infix(m, rhs);
        }

        return C;
    }
    else if (E.is_a<Hs::Literal>())
    {
        return E;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename_infix(m, TE.exp);
        // Nothing to do for TE.type, since there are no type operators unless extensions are enabled.
        return TE;
    }
    else if (auto I = E.to<Hs::InfixExp>())
    {
        auto terms = I->terms;
        for(auto& term: terms)
            term = rename_infix(m, term);
	return desugar_infix(m, terms);
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;

        App.head = rename_infix(m, App.head);
        for(auto& arg: App.args)
            arg = rename_infix(m, arg);

	return App;
    }
    else if (E.is_a<Hs::WildcardPattern>())
        return E;
    else if (E.is_a<Hs::Var>())
        return E;
    else if (E.is_a<Hs::Con>())
        return E;
    else if (E.head().is_a<Hs::Neg>())
        return E;
    else
        std::abort();
}

expression_ref renamer_state::rename(const expression_ref& E, const bound_var_info& bound, set<string>& free_vars)
{
    if (E.is_a<Haskell::List>())
    {
        auto L = E.as_<Haskell::List>();
        for(auto& element: L.elements)
            element = rename(element, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFrom>())
    {
        auto L = E.as_<Haskell::ListFrom>();
        L.from = rename(L.from, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThen>())
    {
        auto L = E.as_<Haskell::ListFromThen>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromTo>())
    {
        auto L = E.as_<Haskell::ListFromTo>();
        L.from = rename(L.from, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListFromThenTo>())
    {
        auto L = E.as_<Haskell::ListFromThenTo>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::ListComprehension>())
    {
        auto L = E.as_<Haskell::ListComprehension>();

        bound_var_info binders;
        for(auto& qual: L.quals)
            add(binders, rename_stmt(qual, bound, binders, free_vars));

        L.body = rename(L.body, bound, binders, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::LeftSection>())
    {
        auto S = E.as_<Haskell::LeftSection>();
        S.l_arg = rename(S.l_arg, bound, free_vars);
        S.op = rename(S.op, bound, free_vars);
        return S;
    }
    else if (E.is_a<Haskell::RightSection>())
    {
        auto S = E.as_<Haskell::RightSection>();
        S.op = rename(S.op, bound, free_vars);
        S.r_arg = rename(S.r_arg, bound, free_vars);
        return S;
    }
    else if (E.is_a<Haskell::Tuple>())
    {
        auto T = E.as_<Haskell::Tuple>();
        for(auto& element: T.elements)
            element = rename(element, bound, free_vars);
        return T;
    }
    else if (E.is_a<Hs::Var>())
    {
        auto V = E.as_<Hs::Var>();
        auto& name = unloc(V.name);
        auto& loc = V.name.loc;

        // Local vars bind id's tighter than global vars.
        if (includes(bound,name))
        {
            free_vars.insert(name);
            return E;
        }
        // If the variable is free, then try top-level names.
        else if (m.is_declared(name))
        {
            const symbol_info& S = m.lookup_symbol(name);
            string qualified_name = S.name;
            name = qualified_name;
            if (get_module_name(qualified_name) == m.name)
                free_vars.insert(qualified_name);
            return V;
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find id '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find id '"<<name<<"'";
        }
    }
    else if (E.is_a<Hs::Con>())
    {
        auto C = E.as_<Haskell::Con>();
        auto& name = unloc(C.name);
        auto& loc = C.name.loc;

        // FIXME: we should look the constructor up in a constructor environment
        // Does that mean that we look up constructors in a different table?
        if (m.is_declared(name))
        {
            const symbol_info& S = m.lookup_symbol(name);
            name = S.name; // use the qualified name
            // We return a reference to a lambda function, in case the constructor isn't fully applied.
            C.arity = S.arity;
            return C;
        }
        else
        {
            if (loc)
                throw myexception()<<"Can't find id '"<<name<<"' at "<<*loc;
            else
                throw myexception()<<"Can't find id '"<<name<<"'";
        }
    }
    else if (E.is_a<Haskell::RecStmt>())
    {
        bound_var_info binders;
        auto R = E.as_<Haskell::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return R;
    }
    else if (E.is_a<Haskell::Do>())
    {
        bound_var_info binders;
        auto D = E.as_<Haskell::Do>();
        for(auto& stmt: D.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return D;
    }
    else if (E.is_a<Haskell::MDo>())
    {
            /*
         * See "The mdo notation" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * mdo { a <- getChar      ===> do { a <- getChar
         *     ; b <- f a c                ; rec { b <- f a c
         *     ; c <- f b a                ;     ; c <- f b a }
         *     ; z <- h a b                ; z <- h a b
         *     ; d <- g d e                ; rec { d <- g d e
         *     ; e <- g a z                ;     ; e <- g a z }
         *     ; putChar c }               ; putChar c }
         */

        /* So... you start the the last statement (e <- g a z) and then find the first statement that has an "e" in the
           free vars of the rhs.  Thats a rec block, regardless of what anything else does, since you can reorder the stmts.
           Then, presumably, you start with the last ungrouped statement, and continue the algorithm.

           The statement itself could have an "e" in the rhs: e <- g a e.

           If NO statement has a e in the rhs, then we move to the second-to-last stmt and continue the algorithm.

           If a statement has e in the lhs, then no other stmts can have e in the lhs, because of the "name shadowing
           not allowed" rule.
         */

        /*
         * See ghc/compiler/rename/RnExpr.hs: Note [Segmenting mdo]
         * What does rec {a;c  mean here?
         *                b;d}
         * I think it means rec {a;c;b;d}, but emphasizes that we have
         * to issue all the stmts from the first recursive group a;c, before
         * we issue any of the stmts from the second group b;d}.
         */

        // Hmm... as a dumb segmentation, we could take all the stmts except the last one and put them in a giant rec...
        // FIXME: implement segmentation, and insert recs.

        bound_var_info binders;
        auto MD = E.as_<Haskell::MDo>();
        for(auto& stmt: MD.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        return MD;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename(TE.exp, bound, free_vars);
        TE.type = rename_type(TE.type);
        return TE;
    }
    else if (E.is_a<Haskell::CaseExp>())
    {
        auto C = E.as_<Haskell::CaseExp>();

        C.object = rename(C.object, bound, free_vars);
        C.alts   = rename(C.alts,   bound, free_vars);

        return C;
    }
    else if (E.is_a<Haskell::LambdaExp>())
    {
        auto L = E.as_<Haskell::LambdaExp>();
        L.match = rename(L.match, bound, free_vars);
        return L;
    }
    else if (E.is_a<Haskell::LetExp>())
    {
        auto L = E.as_<Haskell::LetExp>();

        auto binders = rename_decls(unloc(L.binds), bound, free_vars);
        unloc(L.body) = rename(unloc(L.body), bound, binders, free_vars);

        return L;
    }
    else if (E.is_a<Haskell::IfExp>())
    {
        auto I = E.as_<Haskell::IfExp>();

        unloc(I.condition)    = rename(unloc(I.condition), bound, free_vars);
        unloc(I.true_branch)  = rename(unloc(I.true_branch), bound, free_vars);
        unloc(I.false_branch) = rename(unloc(I.false_branch), bound, free_vars);

        return I;
    }
    else if (E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
    {
        return E;
    }
    else if (E.is_a<Hs::Literal>())
    {
        return E;
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;
        App.head = rename(App.head, bound, free_vars);
        for(auto& arg: App.args)
            arg = rename(arg, bound, free_vars);
        return App;
    }

    std::abort();
}

