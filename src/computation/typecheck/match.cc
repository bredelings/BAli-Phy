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
tuple<Hs::GuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::GuardedRHS rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty())
    {
        auto [body, type] = infer_type(env, rhs.body);
        rhs.body = body;
        return {rhs, type};
    }

    // Fig 25. GUARD
    auto [state2, env2] = copy_clear_lie(env);
    auto guard1 = state2.infer_guard_type(env2, rhs.guards[0]);

    rhs.guards.erase(rhs.guards.begin());
    auto [rhs2, t2] = state2.infer_type(env2, rhs);

    rhs2.guards.insert(rhs2.guards.begin(), guard1);

    current_lie() += state2.current_lie();

    Hs::Type type = t2;
    return {rhs2, type};
}

// Fig 25. GUARD-OR
tuple<Hs::MultiGuardedRHS, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MultiGuardedRHS rhs)
{
    substitution_t s;
    Hs::Type type = fresh_meta_type_var( kind_star() );

    auto [state2,env2] = copy_clear_lie(env);
    if (rhs.decls)
        unloc(*rhs.decls) = state2.infer_type_for_binds(env2, unloc(*rhs.decls)); 

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto [guarded_rhs2, t1] = state2.infer_type(env2, guarded_rhs);
        guarded_rhs = guarded_rhs2;
        unify(t1,type);
    }
    current_lie() += state2.current_lie();
    
    return {rhs, type};
};

tuple<Hs::MRule, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::MRule rule)
{

    if (rule.patterns.empty())
    {
        auto [rhs, type] = infer_type(env, rule.rhs);
        rule.rhs = rhs;
        return {rule, type};
    }
    else
    {
        auto [pat, t1, lve1] = infer_pattern_type(rule.patterns.front());

        auto [new_state,env2] = copy_add_binders(env, lve1);

        // Remove the first pattern in the rule
        rule.patterns.erase(rule.patterns.begin());

        auto [rule2, t2] = new_state.infer_type(env2, rule);
        current_lie() += new_state.current_lie();

        rule2.patterns.insert(rule2.patterns.begin(), pat);

        Hs::Type type = Hs::make_arrow_type(t1,t2);

        return {rule2, type};
    }
}

tuple<Hs::Match, Hs::Type>
typechecker_state::infer_type(const global_value_env& env, Hs::Match m)
{
    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    for(auto& rule: m.rules)
    {
        auto [rule1, t1] = infer_type(env, rule);
        rule = rule1;
        unify(result_type, t1);
    }

    return {m, result_type};
}


