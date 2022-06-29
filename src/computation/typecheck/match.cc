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

void typechecker_state::tcRho(Hs::MRule& rule, const Expected& exp_type)
{
    auto state2 = copy_clear_lie();

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
        state2.tcRho(rule.rhs, Infer(type));
    else
        state2.tcRho(rule.rhs, Check(type));

    current_lie() += state2.current_lie();

    if (exp_type.infer())
        exp_type.infer_type( function_type( pat_types, type ) );
}

void typechecker_state::tcRho(Hs::Match& m, const Expected& exp_type)
{
    // Idea: Change tcRho(MRule, exp_type) to take a list of
    //       expected pattern types, and an expected rhs type.
    //
    //       Then we could pass Infer(NOTYPE) in for the patterns...
    //       we need to handle passing an Infer( ) object to
    //       tcPat( ) twice?

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


