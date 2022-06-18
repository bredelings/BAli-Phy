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
typechecker_state::infer_quals_type(vector<Hs::Qual>& quals)
{
    local_value_env binders;
    for(auto& qual: quals)
        infer_qual_type(qual);
}

void
typechecker_state::infer_qual_type(Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto cond_type = inferRho(SQ.exp);
        unify( cond_type, bool_type() );
        qual = SQ;
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto exp_type = inferRho(PQ.exp);
        auto [pat_type, lve] = infer_pattern_type(PQ.bindpat);

        // type(pat) = type(exp)
        unify(Hs::ListType(pat_type), exp_type);

        add_binders(lve);

        qual = PQ;
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        infer_type_for_binds(unloc(LQ.binds));
        qual = LQ;
    }
    else
        std::abort();
}


void
typechecker_state::infer_guard_type(Hs::Qual& guard)
{
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
        // pat <- exp
        auto [pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto exp_type = inferRho(PQ.exp);

        // type(pat) = type(exp)
        unify(pat_type,exp_type);

        add_binders(lve);

        guard = PQ;
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        infer_type_for_binds(unloc(LQ.binds));
        guard = LQ;
    }
    else
        std::abort();
}


void typechecker_state::tcRhoStmts(int i, vector<Hs::Qual>& stmts, const Expected& expected_type)
{
    auto& stmt = stmts[i];
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

        // pat <- exp ; stmts  =>  exp >>= (\pat -> stmts)
        auto PQ = *pq;

        // 1. Typecheck (>>=)
        // (>>=) ::            exp_type -> (pat_type -> stmts_type) -> b
        // (>>=) :: Monad m => m a      -> ( a       -> m b)        -> b
        PQ.bindOp = Hs::Var({noloc,"Compiler.Base.>>="});
        auto bind_op_type = inferRho(PQ.bindOp);

        // 2. Typecheck exp
        auto exp_type = inferRho(PQ.exp);

        // 3. bind_op_type ~ (exp_type -> a)
        auto a = fresh_meta_type_var( kind_star() );
        unify(bind_op_type, Hs::make_arrow_type(exp_type, a));

        // 4. Typecheck pat
        auto [pat_type, pat_binders] = infer_pattern_type(PQ.bindpat);

        auto new_state = copy_clear_lie();
        new_state.add_binders(pat_binders);

        // 5. Typecheck stmts
        Hs::Type stmts_type;
        new_state.tcRhoStmts(i+1, stmts, Infer(stmts_type));

        // 6. a ~ (pat_type -> stmts_type) -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(Hs::make_arrow_type(pat_type, stmts_type), b));

        // 7. if pat is failable, also typecheck "fail"
        // desugaring always emits fail right now, I think...
        // but if we typecheck this without unifying it with anything, it will create an ambiguous constraint.
        if (false) 
        {
            PQ.failOp = Hs::Var({noloc,"Compiler.Base.fail"});
            auto fail_op_type = new_state.inferRho(PQ.failOp);
        }
        current_lie() += new_state.current_lie();

        stmt = PQ;
        expected_type.infer_type( b );
    }
    else if (auto sq = stmt.to<Hs::SimpleQual>())
    {
        // exp ; stmts  =>  exp >> stmts
        auto SQ = *sq;

        // 1. Typecheck (>>)
        SQ.andThenOp = Hs::Var({noloc,"Compiler.Base.>>"});
        auto and_then_op_type = inferRho(SQ.andThenOp);

        // 2. Typecheck exp
        auto exp_type = inferRho(SQ.exp);

        // 3. and_then_op_type ~ (exp_type -> a)
        auto a = fresh_meta_type_var( kind_star() );
        unify(and_then_op_type, Hs::make_arrow_type(exp_type, a));

        // 4. Typecheck stmts
        Hs::Type stmts_type;
        tcRhoStmts(i+1, stmts, Infer(stmts_type));

        // 5. a ~ stmts_type -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(stmts_type, b));

        stmt = SQ;
        expected_type.infer_type( b );
    }
    else if (auto lq = stmt.to<Hs::LetQual>())
    {
        // let binds ; stmts  =>  let binds in stmts
        auto LQ = *lq;

        // 1. Typecheck binds and add values to global env
        auto state2 = copy_clear_lie();
        state2.infer_type_for_binds(unloc(LQ.binds));

        // 2. Typecheck stmts
        state2.tcRhoStmts(i+1, stmts, expected_type);

        current_lie() += state2.current_lie();

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
