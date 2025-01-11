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

Hs::LExp rename_infix(const Module& m, Hs::LExp LE)
{
    auto& E = unloc(LE);
    if (E.is_a<Hs::List>())
    {
        auto L = E.as_<Hs::List>();
        for(auto& element: L.elements)
            element = rename_infix(m, element);
        E = L;
    }
    else if (E.is_a<Hs::ListFrom>())
    {
        auto L = E.as_<Hs::ListFrom>();
        L.from = rename_infix(m, L.from);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThen>())
    {
        auto L = E.as_<Hs::ListFromThen>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        E = L;
    }
    else if (E.is_a<Hs::ListFromTo>())
    {
        auto L = E.as_<Hs::ListFromTo>();
        L.from = rename_infix(m, L.from);
        L.to   = rename_infix(m, L.to);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThenTo>())
    {
        auto L = E.as_<Hs::ListFromThenTo>();
        L.from = rename_infix(m, L.from);
        L.then = rename_infix(m, L.then);
        L.to   = rename_infix(m, L.to);
        E = L;
    }
    else if (E.is_a<Hs::ListComprehension>())
    {
        auto L = E.as_<Hs::ListComprehension>();
        L.body = rename_infix(m, L.body);
        for(auto& qual: L.quals)
            qual = rename_infix(m, qual);
        E = L;
    }
    else if (E.is_a<Hs::LeftSection>())
    {
        auto S = E.as_<Hs::LeftSection>();
        S.l_arg = rename_infix(m, S.l_arg);
        E = S;
    }
    else if (E.is_a<Hs::RightSection>())
    {
        auto S = E.as_<Hs::RightSection>();
        S.r_arg = rename_infix(m, S.r_arg);
        E = S;
    }
    else if (E.is_a<Hs::Tuple>())
    {
        auto T = E.as_<Hs::Tuple>();
        for(auto& element: T.elements)
            element = rename_infix(m, element);
        E = T;
    }
    else if (E.is_a<Hs::PatQual>())
    {
        auto PQ = E.as_<Hs::PatQual>();

        PQ.bindpat = rename_infix(m, PQ.bindpat);
        PQ.bindpat = unapply(PQ.bindpat);

        PQ.exp = rename_infix(m, PQ.exp);

        E = PQ;
    }
    else if (E.is_a<Hs::SimpleQual>())
    {
        auto SQ = E.as_<Hs::SimpleQual>();
        SQ.exp = rename_infix(m, SQ.exp);
        E = SQ;
    }
    else if (E.is_a<Hs::LetQual>())
    {
        auto LQ = E.as_<Hs::LetQual>();
        unloc(LQ.binds) = rename_infix(m, unloc(LQ.binds));
        E = LQ;
    }
    else if (E.is_a<Hs::AsPattern>())
    {
        auto& AP = E.as_<Hs::AsPattern>();
        E = Hs::AsPattern(AP.var, rename_infix(m,AP.pattern));
    }
    else if (E.is_a<Hs::LazyPattern>())
    {
        auto LP = E.as_<Hs::LazyPattern>();
        E = Hs::LazyPattern(rename_infix(m,LP.pattern));
    }
    else if (E.is_a<Hs::StrictPattern>())
    {
        auto SP = E.as_<Hs::StrictPattern>();
        SP.pattern = rename_infix(m, SP.pattern);
        E = SP;
    }
    else if (E.is_a<Hs::RecStmt>())
    {
        auto R = E.as_<Hs::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            stmt = rename_infix(m, stmt);
        E = R;
    }
    else if (E.is_a<Hs::Do>())
    {
        auto D = E.as_<Hs::Do>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        E = D;
    }
    else if (E.is_a<Hs::MDo>())
    {
        throw myexception()<<"mdo is not handled yet!";
        auto D = E.as_<Hs::MDo>();
        for(auto& stmt: D.stmts.stmts)
            stmt = rename_infix(m, stmt);
        E = D;
    }
    else if (E.is_a<Hs::LambdaExp>())
    {
        auto L = E.as_<Hs::LambdaExp>();
        for(auto& pat: L.match.patterns)
            pat = unapply(rename_infix(m, pat));
        L.match.rhs = rename_infix(m, L.match.rhs);

        E = L;
    }
    else if (E.is_a<Hs::LetExp>())
    {
        auto L = E.as_<Hs::LetExp>();

        unloc(L.binds) = rename_infix(m, unloc(L.binds));
        L.body  = rename_infix(m, L.body);

        E = L;
    }
    else if (E.is_a<Hs::IfExp>())
    {
        auto I = E.as_<Hs::IfExp>();
        I.condition = rename_infix(m, I.condition);
        I.true_branch = rename_infix(m, I.true_branch);
        I.false_branch = rename_infix(m, I.false_branch);
        E = I;
    }
    else if (auto c = E.to<Hs::CaseExp>())
    {
        auto C = *c;

        C.object = rename_infix(m, C.object);

        for(auto& [patterns, rhs]: C.alts)
        {
            patterns[0] = rename_infix(m, patterns[0]);
            patterns[0] = unapply(patterns[0]);
            rhs = rename_infix(m, rhs);
        }

        E = C;
    }
    else if (E.is_a<Hs::Literal>())
    {
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename_infix(m, TE.exp);
        // Nothing to do for TE.type, since there are no type operators unless extensions are enabled.
        E = TE;
    }
    else if (auto I = E.to<Hs::InfixExp>())
    {
        auto terms = I->terms;
        for(auto& term: terms)
            term = rename_infix(m, term);
	E = unloc(desugar_infix(m, terms));
    }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;

        App.head = rename_infix(m, App.head);
        App.arg  = rename_infix(m, App.arg );

	E = App;
    }
    else if (E.is_a<Hs::WildcardPattern>())
    {}
    else if (E.is_a<Hs::Var>())
    {}
    else if (E.is_a<Hs::Con>())
    {}
    else if (E.head().is_a<Hs::Neg>())
    {}
    else
        std::abort();
    return LE;
}

Hs::Exp renamer_state::rename(const Hs::Exp& E, const bound_var_info& bound, set<string>& free_vars)
{
    return unloc(rename({noloc, E}, bound, free_vars));
}


Hs::LExp renamer_state::rename(Hs::LExp LE, const bound_var_info& bound, set<string>& free_vars)
{
    auto& E = unloc(LE);
    auto& loc = LE.loc;

    if (E.is_a<Hs::List>())
    {
        auto L = E.as_<Hs::List>();
        for(auto& element: L.elements)
            element = rename(element, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFrom>())
    {
        auto L = E.as_<Hs::ListFrom>();
        L.from = rename(L.from, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThen>())
    {
        auto L = E.as_<Hs::ListFromThen>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromTo>())
    {
        auto L = E.as_<Hs::ListFromTo>();
        L.from = rename(L.from, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListFromThenTo>())
    {
        auto L = E.as_<Hs::ListFromThenTo>();
        L.from = rename(L.from, bound, free_vars);
        L.then = rename(L.then, bound, free_vars);
        L.to   = rename(L.to  , bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::ListComprehension>())
    {
        auto L = E.as_<Hs::ListComprehension>();

        bound_var_info binders;
        for(auto& qual: L.quals)
            add(binders, rename_stmt(qual, bound, binders, free_vars));

        L.body = rename(L.body, bound, binders, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::LeftSection>())
    {
        auto S = E.as_<Hs::LeftSection>();
        S.l_arg = rename(S.l_arg, bound, free_vars);
        S.op = rename(S.op, bound, free_vars);
        E = S;
    }
    else if (E.is_a<Hs::RightSection>())
    {
        auto S = E.as_<Hs::RightSection>();
        S.op = rename(S.op, bound, free_vars);
        S.r_arg = rename(S.r_arg, bound, free_vars);
        E = S;
    }
    else if (E.is_a<Hs::Tuple>())
    {
        auto T = E.as_<Hs::Tuple>();
        for(auto& element: T.elements)
            element = rename(element, bound, free_vars);
        E = T;
    }
    else if (E.is_a<Hs::Var>())
    {
        auto V = E.as_<Hs::Var>();
        auto& name = V.name;

        // Local vars bind id's tighter than global vars.
        if (includes(bound,name))
        {
            free_vars.insert(name);
        }
        // If the variable is free, then try top-level names.
        else if (m.is_declared(name))
        {
            try
            {
                auto S = m.lookup_symbol(name);
                string qualified_name = S->name;
                name = qualified_name;
                if (get_module_name(qualified_name) == m.name)
                    free_vars.insert(qualified_name);
                E = V;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
        else
        {
            error(loc, Note()<<"Variable `"<<name<<"` not in scope.");
        }
    }
    else if (auto con = E.to<Hs::Con>())
    {
        auto C = *con;
        auto& name = C.name;

        // FIXME: we should look the constructor up in a constructor environment
        // Does that mean that we look up constructors in a different table?
        if (m.is_declared(name))
        {
            try
            {
                auto S = m.lookup_symbol(name);
                name = S->name; // use the qualified name
                // We return a reference to a lambda function, in case the constructor isn't fully applied.
                C.arity = S->arity;
                E = C;
            }
            catch (myexception& e)
            {
                error(loc, Note()<<e.what());
            }
        }
        else
        {
            error(loc, Note()<<"Data constructor `"<<name<<"` not in scope.");
        }
    }
    else if (E.is_a<Hs::RecStmt>())
    {
        bound_var_info binders;
        auto R = E.as_<Hs::RecStmt>();
        for(auto& stmt: R.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));

        E = R;
    }
    else if (E.is_a<Hs::Do>())
    {
        bound_var_info binders;
        auto D = E.as_<Hs::Do>();
        for(auto& stmt: D.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));

        if (D.stmts.stmts.empty())
            error(loc, Note()<<"Empty do block.");
        else
        {
            auto& last = D.stmts.stmts.back();
            if (not unloc(last).is_a<Hs::SimpleQual>())
                error(last.loc, Note()<<"Do block does not end in an expression.");
        }
        E = D;
    }
    else if (E.is_a<Hs::MDo>())
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
        auto MD = E.as_<Hs::MDo>();
        for(auto& stmt: MD.stmts.stmts)
            add(binders, rename_stmt(stmt, bound, binders, free_vars));
        E = MD;
    }
    else if (auto te = E.to<Hs::TypedExp>())
    {
        auto TE = *te;
        TE.exp = rename(TE.exp, bound, free_vars);
        TE.type = rename_type(TE.type);
        E = TE;
    }
    else if (auto c = E.to<Hs::CaseExp>())
    {
        auto C = *c;

        C.object = rename(C.object, bound, free_vars);
        C.alts   = rename(C.alts,    bound, free_vars);

        E = C;
    }
    else if (E.is_a<Hs::LambdaExp>())
    {
        auto L = E.as_<Hs::LambdaExp>();
        L.match = rename(L.match, bound, free_vars);
        E = L;
    }
    else if (E.is_a<Hs::LetExp>())
    {
        auto L = E.as_<Hs::LetExp>();

        auto binders = rename_decls(unloc(L.binds), bound, free_vars);
        L.body = rename(L.body, bound, binders, free_vars);

        E = L;
    }
    else if (E.is_a<Hs::IfExp>())
    {
        auto I = E.as_<Hs::IfExp>();

        I.condition    = rename(I.condition, bound, free_vars);
        I.true_branch  = rename(I.true_branch, bound, free_vars);
        I.false_branch = rename(I.false_branch, bound, free_vars);

        E = I;
    }
    else if (E.is_int() or E.is_log_double() or E.is_double() or E.is_char())
    { }
    else if (E.is_a<Hs::Literal>())
    { }
    else if (auto app = E.to<Hs::ApplyExp>())
    {
        auto App = *app;
        App.head = rename(App.head, bound, free_vars);
        App.arg  = rename(App.arg,  bound, free_vars);
        E = App;
    }
    else if (E.is_a<Hs::WildcardPattern>())
    {
        error(loc, Note()<<"Wildcard pattern in expression!");
    }
    else
    {
        error(loc, Note()<<"Unknown syntax in expression!");
    }

    return LE;
}

