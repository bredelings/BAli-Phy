#include "typecheck.H"
#include "kindcheck.H"

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


