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
Hs::Type
typechecker_state::infer_type(Hs::GuardedRHS& rhs)
{
    // Fig 25. GUARD-DEFAULT
    if (rhs.guards.empty())
    {
        auto type = infer_type(rhs.body);
        return type;
    }

    // Fig 25. GUARD
    auto state2 = copy_clear_lie();
    auto guard1 = state2.infer_guard_type(rhs.guards[0]);

    rhs.guards.erase(rhs.guards.begin());
    auto result_type = state2.infer_type(rhs);
    rhs.guards.insert(rhs.guards.begin(), guard1);

    current_lie() += state2.current_lie();

    return result_type;
}

// Fig 25. GUARD-OR
Hs::Type
typechecker_state::infer_type(Hs::MultiGuardedRHS& rhs)
{
    substitution_t s;
    Hs::Type type = fresh_meta_type_var( kind_star() );

    auto state2 = copy_clear_lie();
    if (rhs.decls)
        unloc(*rhs.decls) = state2.infer_type_for_binds(unloc(*rhs.decls)); 

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto t1 = state2.infer_type(guarded_rhs);
        unify(t1,type);
    }
    current_lie() += state2.current_lie();
    
    return type;
};

Hs::Type
typechecker_state::infer_type(Hs::MRule& rule)
{
    if (rule.patterns.empty())
        return infer_type(rule.rhs);
    else
    {
        auto [t1, lve1] = infer_pattern_type(rule.patterns.front());
        auto pat = rule.patterns.front();

        auto new_state = copy_add_binders( lve1 );

        // REMOVE the first pattern in the rule
        rule.patterns.erase(rule.patterns.begin());

        auto t2 = new_state.infer_type(rule);
        current_lie() += new_state.current_lie();

        // PUT BACK the first pattern in the rule (but the typechecked version)
        rule.patterns.insert(rule.patterns.begin(), pat);

        return Hs::make_arrow_type(t1,t2);
    }
}

Hs::Type
typechecker_state::infer_type(Hs::Match& m)
{
    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    for(auto& rule: m.rules)
    {
        auto t1 = infer_type(rule);
        unify(result_type, t1);
    }

    return result_type;
}


