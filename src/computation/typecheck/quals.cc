#include "typecheck.H"
#include "kindcheck.H"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Figure 22. Rules for quals
//
// The implementation is rather... different?
// * the original figure doesn't have let quals.
// * the original figure seems to assume that quals only occur in list comprehensions?

void
TypeChecker::infer_quals_type(vector<Located<Hs::Qual>>& quals)
{
    local_value_env binders;
    for(auto& qual: quals)
        infer_qual_type(qual);
}

void
TypeChecker::infer_qual_type(Located<Hs::Qual>& lqual)
{
    auto& [loc, qual ] = lqual;
    if (loc) push_source_span(*loc);

	// FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        checkRho(SQ.exp, bool_type());
        qual = SQ;
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto exp_type = inferRho(PQ.exp);
        local_value_env lve;
        auto pat_type= inferPat(lve, PQ.bindpat);

        // type(pat) = type(exp)
        unify(list_type(pat_type), exp_type);

        add_binders(lve);

        qual = PQ;
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        infer_type_for_binds(LQ.binds);
        qual = LQ;
    }
    else
        std::abort();

    if (loc) pop_source_span();
}


void
TypeChecker::infer_guard_type(Located<Hs::Qual>& lguard)
{
    auto& [loc, guard] = lguard;
    if (loc) push_source_span(*loc);

    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto cond_type = inferRho(SQ.exp);
        unify( cond_type, bool_type() );
        guard = SQ;
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        auto PQ = *pq;

        // pat <- body
        auto body_type = inferRho(PQ.exp);
        local_value_env lve;
        checkPat(lve, PQ.bindpat, body_type);
        guard = PQ;

        add_binders(lve);
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        infer_type_for_binds(LQ.binds);
        guard = LQ;
    }
    else
        std::abort();

    if (loc) pop_source_span();
}


void TypeChecker::tcRhoStmts(int i, vector<Located<Hs::Qual>>& stmts, const Expected& expected_type)
{
    auto& stmt = unloc(stmts[i]);
    // Last statement -- an expression.
    if (i == stmts.size() - 1)
    {
        auto last = stmt.to<Hs::SimpleQual>();
        if (not last)
            throw myexception()<<"stmts list does not end in an expression";

        auto Last = *last;
        tcRho(Last.exp, expected_type);
        stmt = Last;
    }
    // Bind stmt
    else if (auto pq = stmt.to<Hs::PatQual>())
    {
        // FIXME!  There should be a better way to do this -- that allows us to use checkRho or checkSigma
        // - eliminate set_expected_type( )
        // - unwind two interations of checkRho(App )
        // - we would need to record wrappers for the first and second argument of (>>=)

        // pat <- exp ; stmts    ~>    (>>=) exp (\pat -> stmts)
        auto PQ = *pq;

        // 1. Typecheck (>>=)
        auto bind_op_type = inferRho(PQ.bindOp);

        // (>>=) :: Monad m => m a -> ( a -> m b) -> m b

        // bind_op = exp_type -> tmp
        //         = exp_type -> arg2_type -> result_type
        //         = exp_type -> (pat_type -> stmts_type) -> result_type
        //         = m a      -> (a        -> m b)        -> m b
        auto [exp_type,  tmp        ] = unify_function(bind_op_type);
        auto [arg2_type, result_type] = unify_function(tmp);
        auto [pat_type, stmts_type  ] = unify_function(arg2_type);
        set_expected_type( expected_type, result_type );

        // 2. Check exp
        checkRho(PQ.exp, exp_type);

        // 3. Check pat
        local_value_env pat_binders;
        checkPat(pat_binders, PQ.bindpat, pat_type);

        // 4. Check stmts
        auto state2 = copy_clear_wanteds();
        state2.add_binders(pat_binders);
        state2.tcRhoStmts(i+1, stmts, Check(stmts_type));

        // 5. if pat is failable, also typecheck "fail".
        // Desugaring always emits fail right now, I think...
        //   but if we typecheck this without unifying it with anything, it will create an ambiguous constraint.
        if (false) 
        {
            PQ.failOp = Hs::Var("Control.Monad.fail");
            auto fail_op_type = state2.inferRho(*PQ.failOp);
        }
        current_wanteds() += state2.current_wanteds();

        stmt = PQ;
    }
    else if (auto sq = stmt.to<Hs::SimpleQual>())
    {
        // exp ; stmts  =>  exp >> stmts
        auto SQ = *sq;

        // 1. Typecheck (>>)
        auto and_then_op_type = inferRho(SQ.andThenOp);

        auto [exp_type,   tmp        ] = unify_function(and_then_op_type);
        auto [stmts_type, result_type] = unify_function(tmp);
        set_expected_type( expected_type, result_type);

        // 2. Typecheck exp
        checkRho(SQ.exp, exp_type);

        // 3. Typecheck stmts
        tcRhoStmts(i+1, stmts, Check(stmts_type));

        stmt = SQ;
    }
    else if (auto lq = stmt.to<Hs::LetQual>())
    {
        // let binds ; stmts  =>  let binds in stmts
        auto LQ = *lq;

        // 1. Typecheck binds and add values to global env
        auto state2 = copy_clear_wanteds();
        state2.infer_type_for_binds(LQ.binds);

        // 2. Typecheck stmts
        state2.tcRhoStmts(i+1, stmts, expected_type);

        current_wanteds() += state2.current_wanteds();

        stmt = LQ;
    }
    else if (stmt.to<Hs::RecStmt>())
    {
        /*
         * See "Recursive binding groups" in https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/recursive_do.html
         *
         * "Like let and where bindings, name shadowing is not allowed within an mdo-expression or a rec-block"
         *
         * As an example:           ===>
         *   rec { b <- f a c              (b,c) <- mfix (\ ~(b,c) -> do { b <- f a c
         *       ; c <- f b a }                                          ; c <- f b a
         *                                                               ; return (b,c) } )
         *
         * See ghc/compiler/rename/RnExpr.hs
         */

        // currently we desugar rec stmts in rename.

        // So... we would need to typecheck "return" and "mfix", in order to collect the dictionaries for them.
        // We would need to typecheck the individual stmts, but with "return ((b,i,n,d,e,r,s)" as the magic last statement.

        // Then we'd have to typecheck mfix (\ ~(b,c) -> that)
        // and all without actually constructing the expressions, I think, like how we decompose the application
        // of (>>=) above...
        std::abort();
    }
    else
        std::abort();
}
