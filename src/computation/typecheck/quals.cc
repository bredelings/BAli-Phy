#include "typecheck.H"
#include "kindcheck.H"
#include "computation/haskell/ids.H"

#include "range/v3/all.hpp"

namespace views = ranges::views;

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

namespace
{
    vector<string> rec_stmt_binder_names(const Hs::RecStmt& R)
    {
        set<string> names;
        for(const auto& var: Hs::vars_bound_in_stmts(R.stmts))
            names.insert(unloc(var).name);
        return names | ranges::to<vector<string>>();
    }

    vector<Hs::LExp> rec_stmt_binder_exps(const vector<string>& names)
    {
        vector<Hs::LExp> vars;
        for(auto& name: names)
            vars.push_back({noloc, Hs::Var(name)});
        return vars;
    }

    Hs::LPats rec_stmt_binder_pats(const vector<string>& names)
    {
        Hs::LPats pats;
        for(auto& name: names)
            pats.push_back({noloc, Hs::VarPattern({noloc, Hs::Var(name)})});
        return pats;
    }

    Hs::LExp rec_stmt_as_mfix_exp(const Hs::RecStmt& R)
    {
        auto binder_names = rec_stmt_binder_names(R);
        auto rec_tuple = Hs::tuple(rec_stmt_binder_exps(binder_names));
        Hs::LPat rec_tuple_pattern = {noloc, Hs::tuple_pattern(rec_stmt_binder_pats(binder_names))};

        auto rec_return_stmt = Hs::apply({noloc, R.returnOp}, {{noloc, rec_tuple}});
        auto stmts = R.stmts.stmts;
        stmts.push_back({noloc, Hs::SimpleQual(rec_return_stmt)});
        auto rec_do = Haskell::Do(Haskell::Stmts(stmts));

        Hs::Exp rec_lambda = Haskell::LambdaExp({{noloc, Haskell::LazyPattern(rec_tuple_pattern)}}, {noloc, rec_do});
        return Hs::apply({noloc, R.mfixOp}, std::vector<Hs::LExp>{{noloc, rec_lambda}});
    }

    void copy_checked_rec_stmt_from_mfix_exp(Hs::RecStmt& R, const Hs::LExp& mfix_exp)
    {
        auto app1 = unloc(mfix_exp).as_<Hs::ApplyExp>();
        R.mfixOp = unloc(app1.head).as_<Hs::Var>();

        auto lambda = unloc(app1.arg).as_<Hs::LambdaExp>();
        auto rec_do = unloc(lambda.match.rhs.guarded_rhss[0].body).as_<Hs::Do>();
        auto checked_stmts = rec_do.stmts.stmts;
        auto return_stmt = unloc(checked_stmts.back()).as_<Hs::SimpleQual>();
        auto return_app = unloc(return_stmt.exp).as_<Hs::ApplyExp>();

        R.returnOp = unloc(return_app.head).as_<Hs::Var>();
        checked_stmts.pop_back();
        R.stmts = Hs::Stmts(checked_stmts);
    }
}

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
    auto span = source_span_scope(loc);

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
        PQ.bindpat_can_fail = this_mod().is_refutable_pattern(PQ.bindpat);

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

}


void
TypeChecker::infer_guard_type(Located<Hs::Qual>& lguard)
{
    auto& [loc, guard] = lguard;
    auto span = source_span_scope(loc);

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
        PQ.bindpat_can_fail = this_mod().is_refutable_pattern(PQ.bindpat);
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
        PQ.bindpat_can_fail = this_mod().is_refutable_pattern(PQ.bindpat);

        // 4. if pat is failable, also typecheck "fail".
        if (*PQ.bindpat_can_fail)
        {
            auto span = source_span_scope(pq->bindpat.loc);
            auto fail_op_type = inferRho(*PQ.failOp);
            auto [fail_arg_type, fail_result_type] = unify_function(fail_op_type);
            unify(fail_result_type, stmts_type);
        }
        else
            PQ.failOp = {};

        // 5. Check stmts
        auto state2 = copy_clear_wanteds();
        state2.add_binders(pat_binders);
        state2.tcRhoStmts(i+1, stmts, Check(stmts_type));

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
    else if (auto rec = stmt.to<Hs::RecStmt>())
    {
        auto R = *rec;
        auto mfix_symbol = this_mod().lookup_resolved_symbol(mfix_name);
        if (not mfix_symbol)
            throw note_exception()<<"RecursiveDo requires hidden symbol '"<<mfix_name<<"'.";
        R.mfixOp = Hs::Var(mfix_symbol->name);

        auto bindpat = Hs::LPat{noloc, Hs::tuple_pattern(rec_stmt_binder_pats(rec_stmt_binder_names(R)))};
        auto mfix_exp = rec_stmt_as_mfix_exp(R);

        auto synthetic_pq = Hs::PatQual(bindpat, mfix_exp);
        synthetic_pq.bindOp = R.bindOp;
        synthetic_pq.failOp = {};
        synthetic_pq.bindpat_can_fail = false;
        auto synthetic_stmt = Hs::LStmt{stmts[i].loc, synthetic_pq};

        vector<Located<Hs::Qual>> synthetic_stmts;
        synthetic_stmts.push_back(synthetic_stmt);
        for(int j = i+1; j < stmts.size(); j++)
            synthetic_stmts.push_back(stmts[j]);

        tcRhoStmts(0, synthetic_stmts, expected_type);

        auto checked_pq = unloc(synthetic_stmts[0]).as_<Hs::PatQual>();
        R.bindOp = checked_pq.bindOp;
        copy_checked_rec_stmt_from_mfix_exp(R, checked_pq.exp);
        R.checked_rec = std::make_shared<Hs::CheckedRecStmt>(synthetic_stmts[0]);

        for(int j = i+1; j < stmts.size(); j++)
            stmts[j] = synthetic_stmts[j-i];

        stmt = R;
    }
    else
        std::abort();
}
