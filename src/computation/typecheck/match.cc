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
void typechecker_state::tcRho(Hs::MultiGuardedRHS& rhs, const Expected& exp_type)
{
    auto state2 = copy_clear_lie();
    if (rhs.decls)
        state2.infer_type_for_binds(unloc(*rhs.decls));

    auto etype = expTypeToType(exp_type);
    for(auto& guarded_rhs: rhs.guarded_rhss)
        state2.tcRho(guarded_rhs, Check(etype));

    current_lie() += state2.current_lie();
}

void typechecker_state::tcRho(Hs::MRule& rule, const Expected& exp_type, int i)
{
    if (i < rule.patterns.size())
    {
        if (exp_type.infer())
        {
            auto [pat_type, lve1] = infer_pattern_type(rule.patterns[i]);

            auto tc2 = copy_add_binders( lve1 );

            Hs::Type body_type;
            tc2.tcRho(rule, Infer(body_type), i+1);
            current_lie() += tc2.current_lie();

            exp_type.infer_type( Hs::make_arrow_type(pat_type, body_type) );
        }
        else
        {
            auto [arg_type, result_type] = unify_function(exp_type.check_type());

            auto lve1 = checkPat(rule.patterns[i], arg_type);

            auto tc2 = copy_add_binders(lve1);

            tc2.tcRho(rule, Check(result_type), i + 1);

            current_lie() += tc2.current_lie();
        }
    }
    else
        tcRho(rule.rhs, exp_type);
}

void typechecker_state::tcRho(Hs::Match& m, const Expected& exp_type)
{
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


