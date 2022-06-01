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

tuple<vector<Hs::Qual>, value_env>
typechecker_state::infer_quals_type(const global_value_env& env, vector<Hs::Qual> quals)
{
    auto env2 = env;
    local_value_env binders;
    for(auto& qual: quals)
    {
        auto [qual_exp, qual_binders] = infer_qual_type(env2, qual);

        qual = qual_exp;
        env2 = plus_prefer_right(env2, qual_binders);
        binders = plus_prefer_right(binders, qual_binders);
    }
    return {quals, binders};
}

tuple<Hs::Qual, value_env>
typechecker_state::infer_qual_type(const global_value_env& env, const Hs::Qual& qual)
{
    // FILTER
    if (auto sq = qual.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
    }
    // GENERATOR.
    else if (auto pq = qual.to<Hs::PatQual>())
    {
        auto PQ = *pq;
        // pat <- exp
        auto [bindpat, pat_type, lve] = infer_pattern_type(PQ.bindpat);
        auto [exp, exp_type] = infer_type(env, PQ.exp);

        PQ.bindpat = bindpat;
        PQ.exp = exp;

        // type(pat) = type(exp)
        unify(Hs::ListType(pat_type), exp_type);

        return {PQ, lve};
    }
    else if (auto lq = qual.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
    }
    else
        std::abort();
}


tuple<Hs::Qual, value_env>
typechecker_state::infer_guard_type(const global_value_env& env, const Hs::Qual& guard)
{
    if (auto sq = guard.to<Hs::SimpleQual>())
    {
        auto SQ = *sq;
        auto [cond_exp, cond_type] = infer_type(env, SQ.exp);
        SQ.exp = cond_exp;
        unify( cond_type, bool_type() );
        return {SQ, {}};
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

        return {PQ, lve};
    }
    else if (auto lq = guard.to<Hs::LetQual>())
    {
        auto LQ = *lq;
        auto [binds, t] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;
        return {LQ, t};
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
        auto env2 = plus_prefer_right( env, pat_binders );
        PQ.bindpat = bindpat;

        // 5. Typecheck stmts
        auto stmts_type = infer_stmts_type(env2, i+1, stmts);

        // 6. a ~ (pat_type -> stmts_type) -> b
        auto b = fresh_meta_type_var( kind_star() );
        unify(a, Hs::make_arrow_type(Hs::make_arrow_type(pat_type, stmts_type), b));

        // 7. if pat is failable, also typecheck "fail"
        // desugaring always emits fail right now, I think...
        // but if we typecheck this without unifying it with anything, it will create an ambiguous constraint.
        if (false) 
        {
            auto [fail_op, fail_op_type] = infer_type(gve, Hs::Var({noloc,"Compiler.Base.fail"}));
            PQ.failOp = fail_op;
        }

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

        // 1. Typecheck binds.
        auto LQ = *lq;
        auto [binds, binders] = infer_type_for_binds(env, unloc(LQ.binds));
        unloc(LQ.binds) = binds;

        auto env2 = plus_prefer_right(env, binders);

        // 2. Typecheck stmts
        auto stmts_type = infer_stmts_type(env2, i+1, stmts);

        stmt = LQ;
        return stmts_type;
    }
    else if (rq = stmt.to<Hs::RecStmt>())
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
        std::abort();
    }
    else
        std::abort();
}
