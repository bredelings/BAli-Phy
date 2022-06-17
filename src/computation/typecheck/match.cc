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
        auto state2 = copy_clear_lie();
        state2.infer_guard_type(rhs.guards[i]);

        state2.tcRho(rhs, exp_type, i+1);

        current_lie() += state2.current_lie();
    }
    else
        tcRho(rhs.body, exp_type);
}

// Fig 25. GUARD-OR
Hs::Type
typechecker_state::inferRho(Hs::MultiGuardedRHS& rhs)
{
    substitution_t s;
    Hs::Type type = fresh_meta_type_var( kind_star() );

    auto state2 = copy_clear_lie();
    if (rhs.decls)
        state2.infer_type_for_binds(unloc(*rhs.decls)); 

    for(auto& guarded_rhs: rhs.guarded_rhss)
    {
        auto t1 = state2.inferRho(guarded_rhs);
        unify(t1,type);
    }
    current_lie() += state2.current_lie();
    
    return type;
};

Hs::Type
typechecker_state::inferRho(Hs::MRule& rule, int i)
{
    if (i < rule.patterns.size())
    {
        auto [t1, lve1] = infer_pattern_type(rule.patterns[i]);

        auto new_state = copy_add_binders( lve1 );

        auto t2 = new_state.inferRho(rule, i+1);
        current_lie() += new_state.current_lie();

        return Hs::make_arrow_type(t1,t2);
    }
    else
        return inferRho(rule.rhs);
}

Hs::Type
typechecker_state::inferRho(Hs::Match& m)
{
    Hs::Type result_type = fresh_meta_type_var( kind_star() );

    for(auto& rule: m.rules)
    {
        auto t1 = inferRho(rule);
        unify(result_type, t1);
    }

    return result_type;
}


