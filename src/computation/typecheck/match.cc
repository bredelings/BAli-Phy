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

void typechecker_state::tcMatch(Hs::MRule& m, const vector<Expected>& pat_types, const Expected& result_type)
{
    assert(m.patterns.size() == pat_types.size());

    auto state2 = copy_clear_wanteds();

    for(int i=0; i<m.patterns.size(); i++)
    {
        local_value_env pat_binders;
        state2.tcPat(pat_binders, m.patterns[i], pat_types[i]);
        state2.add_binders( pat_binders );
    }

    state2.tcRho(m.rhs, result_type);

    current_wanteds() += state2.current_wanteds();
}

void typechecker_state::tcMatches(Hs::Matches& ms, const vector<Expected>& pat_types, const Expected& result_type)
{
    if (ms.empty())
    {
        for(auto& pat_type: pat_types)
            expTypeToType(pat_type);
        expTypeToType(result_type);

        // record types on Match object?
    }
    else
    {
        for(auto& m: ms)
            tcMatch(m, pat_types, result_type);

        // record types on Match object?
    }
}

int getArity(const Hs::Matches& matches)
{
    int arity = getArity(matches[0]);
    for(int i=0;i<matches.size();i++)
        if (matches[i].patterns.size() != arity)
            throw myexception()<<"Got "<<matches[i].patterns.size()<<" patterns but expected "<<arity<<":\n   "<<matches[i].print();
    return arity;
}

int getArity(const Hs::MRule& m)
{
    return m.patterns.size();
}
