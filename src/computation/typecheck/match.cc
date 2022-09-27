#include "typecheck.H"
#include "kindcheck.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Figure 25. Rules for match, mrule, and grhs
void typechecker_state::tcRho(Hs::GuardedRHS& rhs, const Expected& exp_type, int i)
{
    if (i < rhs.guards.size())
    {
        // Fig 25. GUARD
        auto state2 = copy_clear_wanteds();
        state2.infer_guard_type(rhs.guards[i]);

        state2.tcRho(rhs, exp_type, i+1);

        current_wanteds() += state2.current_wanteds();
    }
    else
        tcRho(rhs.body, exp_type);
}

// Fig 25. GUARD-OR
void typechecker_state::tcRho(Hs::MultiGuardedRHS& rhs, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();
    if (rhs.decls)
        state2.infer_type_for_binds(unloc(*rhs.decls));

    auto etype = expTypeToType(exp_type);
    for(auto& guarded_rhs: rhs.guarded_rhss)
        state2.tcRho(guarded_rhs, Check(etype));

    current_wanteds() += state2.current_wanteds();
}

void typechecker_state::tcRho(Hs::MRule& rule, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();

    Hs::Type type;
    if (exp_type.check())
        type = exp_type.check_type();

    vector<Hs::Type> pat_types;

    for(int i=0; i< rule.patterns.size(); i++)
    {
        if (exp_type.infer())
        {
            auto [pat_type, lve1] = state2.inferPat(rule.patterns[i]);

            state2.add_binders( lve1 );

            pat_types.push_back(pat_type);
        }
        else
        {
            auto [arg_type, result_type] = unify_function(type);

            auto lve1 = state2.checkPat(rule.patterns[i], arg_type);

            state2.add_binders(lve1);

            type = result_type;
        }
    }

    // Here, 'type' is the rhs type.
    if (exp_type.infer())
    {
        Expected exp_type2 = Infer();
        state2.tcRho(rule.rhs, exp_type2);
        type = exp_type2.read_type();
    }
    else
        state2.tcRho(rule.rhs, Check(type));

    current_wanteds() += state2.current_wanteds();

    if (exp_type.infer())
        exp_type.infer_type( function_type( pat_types, type ) );
}

void typechecker_state::tcRho(Hs::Matches& m, const Expected& exp_type)
{
    // Idea: Change tcRho(MRule, exp_type) to take a list of
    //       expected pattern types, and an expected rhs type.
    //
    //       Then we could pass Infer(NOTYPE) in for the patterns.
    //       We would need to handle passing an Infer( ) object to tcPat( ) twice.
    //       - if the variable has no signature, then use expTypeToType
    //       - if the variable has a signature, and is empty then... just record it?
    //         + but what if the variable has a signature and is NOT empty?
    //         + see Gen/Pat.hs > tcPatBndr, which calls tc_sub_type

    if (exp_type.infer())
    {
        Hs::Type result_type = fresh_meta_type_var( kind_star() );

        for(auto& rule: m.rules)
        {
            auto t1 = inferRho(rule);
            unify(result_type, t1);
        }

        exp_type.infer_type( result_type );
    }
    else
    {
        for(auto& rule: m.rules)
            tcRho(rule, Check(exp_type.check_type()));
    }
}

void typechecker_state::tcMatch(Hs::MRule& m, const vector<Expected>& pat_types, const Expected& result_type)
{
    assert(m.patterns.size() == pat_types.size());

    auto state2 = copy_clear_wanteds();

    for(int i=0; i<m.patterns.size(); i++)
    {
        auto pat_binders = state2.tcPat(m.patterns[i], pat_types[i]);
        state2.add_binders( pat_binders );
    }

    state2.tcRho(m.rhs, result_type);

    current_wanteds() += state2.current_wanteds();
}

void typechecker_state::tcMatches(Hs::Matches& ms, const vector<Expected>& pat_types, const Expected& result_type)
{
    if (ms.rules.empty())
    {
        for(auto& pat_type: pat_types)
            expTypeToType(pat_type);
        expTypeToType(result_type);

        // record types on Match object?
    }
    else
    {
        for(auto& m: ms.rules)
            tcMatch(m, pat_types, result_type);

        // record types on Match object?
    }
}

vector<Hs::Type> read_types(const vector<Expected>& exp_types)
{
    vector<Hs::Type> types;
    for(auto& exp_type: exp_types)
        types.push_back( exp_type.read_type() );
    return types;
}


Core::wrapper typechecker_state::tcMatchesFunInfer(Hs::Matches& m, vector<Expected>& arg_types, int arity, const Expected& fun_type)
{
    // pad out with FlexiTyVars ... what's that?
    while(arg_types.size() < arity)
        arg_types.push_back( Infer() );
    Expected result_type = Infer();

    tcMatches(m, arg_types, result_type);

    auto unif_fun_type = Hs::function_type( read_types(arg_types), result_type.read_type());

//    subsumptionCheck(unif_fun_type, fun_type);
    auto sub_wrapper = subsumptionCheck(unif_fun_type, fun_type.read_type());

    return sub_wrapper;
}

Core::wrapper typechecker_state::tcMatchesFunCheck(Hs::Matches& m, vector<Expected>& arg_types, int arity, Hs::Type type)
{
    // There should also be a wrapper for skolemize -- gen_wrapper.
    // It will handle cases like forall a. C1 => forall b. C2 => E.
    // Until we add type arguments, it should be Core::wrapper_id though.

    if (auto [tvs, givens, rho_type] = skolemize(type,true); not tvs.empty() or not givens.empty())  // type has any foralls or a context.  // skolemize
    {
        assert(not tvs.size());
        assert(not givens.size());

        // push level here -- mark all variables untouchable
        // we don't have levels yet, so record all variables in the untouchable set.

        auto tc2 = copy_clear_wanteds();
        auto match_wrapper = tc2.tcMatchesFunCheck(m, arg_types, arity, type);

        current_wanteds().implications.push_back( std::make_shared<Implication>(tvs, givens, tc2.current_wanteds()) );

        return match_wrapper;
    }
    else if (arg_types.size() == arity)
    {
        // If we got here, then the Check type had enough arguments.
        // We don't need to construct a result type, we just need to check things.
        tcMatches(m, arg_types, Check(type));
        return Core::wrapper_id;
    }
    // look through type synonyms
    else if (auto type2 = is_type_synonym(type))
    {
        return tcMatchesFunCheck(m, arg_types, arity, *type2);
    }
    // get the next argument type
    else if (auto ftype = Hs::is_function_type(type))
    {
        auto [arg_type, result_type] = *ftype;
        arg_types.push_back(Check(arg_type));
        return tcMatchesFunCheck(m, arg_types, arity, result_type);
    }
    // look through filled MetaTypeVars
    else if (auto mtv = type.to<Hs::MetaTypeVar>())
    {
        if (mtv->filled())
            return tcMatchesFunCheck(m, arg_types, arity, *mtv->filled());
        else
        {
            // We don't know the remaining argument types.
            return tcMatchesFunInfer(m, arg_types, arity, Check(type));
        }
    }
    // 
    else
    {
        // This is different from the case above, because we know that the number
        // of arguments doesn't match the arity.  Which is not NECESSARILY wrong.

        return tcMatchesFunInfer(m, arg_types, arity, Check(type));
    }

    std::abort();
}

Core::wrapper typechecker_state::tcMatchesFun(Hs::Matches& m, Expected fun_type)
{
    vector<Expected> arg_types;

    int arity = m.rules[0].patterns.size();
    for(int i=0;i<m.rules.size();i++)
        if (m.rules[i].patterns.size() != arity)
            throw myexception()<<"Got "<<m.rules[i].patterns.size()<<" patterns but expected "<<arity<<":\n   "<<m.rules[i].print();

    if (fun_type.check())
        return tcMatchesFunCheck(m, arg_types, arity, fun_type.read_type());
    else
        return tcMatchesFunInfer(m, arg_types, arity, fun_type);
}
