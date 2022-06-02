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

vector<Hs::Qual>
typechecker_state::infer_quals_type(global_value_env& env, vector<Hs::Qual> quals)
{
    local_value_env binders;
    for(auto& qual: quals)
        qual = infer_qual_type(env, qual);
    return quals;
}

Hs::Qual
typechecker_state::infer_qual_type(global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;
        unify( cond_type, bool_type() );
        return SQ;
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [exp, exp_type] = infer_type(env, PQ.exp);
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        unify(Hs::ListType(pat_type), exp_type);

        env = add_binders(env,lve);

        return PQ;
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        unloc(LQ.binds) = infer_type_for_binds(env, unloc(LQ.binds));
        return LQ;
    }
    else
        std::abort();
}


Hs::Qual
typechecker_state::infer_guard_type(global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = cond_exp;
        unify( cond_type, bool_type() );
        return SQ;
    }
    else if (auto pq = guard.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        unify(pat_type,exp_type);

        env = add_binders(env, lve);

        return PQ;
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        unloc(LQ.binds) = infer_type_for_binds(env, unloc(LQ.binds));
        return LQ;
    }
    else
        std::abort();
}


Hs::Type
typechecker_state::infer_stmts_type(const global_value_env& env, int i, vector<Hs::Qual>& stmts)
{
    auto& stmt = stmts[i];
    // Last statement -- an expression.
    if (i == stmts.size() - 1)
    {
        auto last = stmt.to<Hs::SimpleQual>();
        if (not last)
            throw myexception()<<"stmts list does not end in an expression";

        auto Last = *last;
        auto [exp, type] = infer_type(env, Last.exp);
        Last.exp = exp;

        stmt = Last;
        return type;
    }
    // Bind stmt
    else if (auto pq = stmt.to<Hs::PatQual>())
    {
        // pat <- exp ; stmts  =>  exp >>= (\pat -> stmts)
        auto PQ = *pq;

        // 1. Typecheck (>>=)
        auto [bind_op, bind_op_type] = infer_type(gve, Hs::Var({noloc,"Compiler.Base.>>="}));
        PQ.bindOp = bind_op;

        // 2. Typecheck exp
        auto [exp, exp_type] = infer_type(env, PQ.exp);
        PQ.exp = exp;

        // 3. bind_op_type ~ (exp_type -> a)
        auto a = fresh_meta_type_var( kind_star() );
        unify(bind_op_type, Hs::make_arrow_type(exp_type, a));

        // 4. Typecheck pat
        auto [bindpat, pat_type, pat_binders] = infer_pattern_type(PQ.bindpat);

        auto [new_state, env2] = copy_add_binders(env, pat_binders);

        PQ.bindpat = bindpat;

        // 5. Typecheck stmts
        auto stmts_type = new_state.infer_stmts_type(env2, i+1, stmts);

        // 6. a ~ (pat_type -> stmts_type) -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(Hs::make_arrow_type(pat_type, stmts_type), b));

        // 7. if pat is failable, also typecheck "fail"
        // desugaring always emits fail right now, I think...
        // but if we typecheck this without unifying it with anything, it will create an ambiguous constraint.
        if (false) 
        {
            auto [fail_op, fail_op_type] = new_state.infer_type(gve, Hs::Var({noloc,"Compiler.Base.fail"}));
            PQ.failOp = fail_op;
        }
        current_lie() += new_state.current_lie();

        stmt = PQ;
        return b;
    }
    else if (auto sq = stmt.to<Hs::SimpleQual>())
    {
        // exp ; stmts  =>  exp >> stmts
        auto SQ = *sq;

        // 1. Typecheck (>>)
        auto [and_then_op, and_then_op_type] = infer_type(gve, Hs::Var({noloc,"Compiler.Base.>>"}));
        SQ.andThenOp = and_then_op;

        // 2. Typecheck exp
        auto [exp, exp_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;

        // 3. and_then_op_type ~ (exp_type -> a)
        auto a = fresh_meta_type_var( kind_star() );
        unify(and_then_op_type, Hs::make_arrow_type(exp_type, a));

        // 4. Typecheck stmts
        auto stmts_type = infer_stmts_type(env, i+1, stmts);

        // 5. a ~ stmts_type -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(stmts_type, b));

        stmt = SQ;
        return b;
    }
    else if (auto lq = stmt.to<Hs::LetQual>())
    {
        // let binds ; stmts  =>  let binds in stmts
        auto LQ = *lq;

        // 1. Typecheck binds and add values to global env
        auto [state2,env2] = copy_clear_lie(env);
        unloc(LQ.binds) = state2.infer_type_for_binds(env2, unloc(LQ.binds));

        // 2. Typecheck stmts
        auto stmts_type = state2.infer_stmts_type(env2, i+1, stmts);

        current_lie() += state2.current_lie();

        stmt = LQ;
        return stmts_type;
    }
    else if (auto rq = stmt.to<Hs::RecStmt>())
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
