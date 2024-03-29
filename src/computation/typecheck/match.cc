#include "typecheck.H"
#include "kindcheck.H"
#include "util/text.H"

using std::string;
using std::vector;
using std::map;
using std::set;
using std::pair;
using std::optional;
using std::tuple;

// Figure 25. Rules for match, mrule, and grhs
void TypeChecker::tcRho(Hs::GuardedRHS& rhs, const Expected& exp_type, int i)
{
    if (i < rhs.guards.size())
    {
        // Fig 25. GUARD
        push_source_span(*rhs.guards[i].loc);
        auto state2 = copy_clear_wanteds();
        state2.infer_guard_type(rhs.guards[i]);
        pop_source_span();

        state2.tcRho(rhs, exp_type, i+1);

        current_wanteds() += state2.current_wanteds();
    }
    else
    {
        if (rhs.body.loc) push_source_span(*rhs.body.loc);
        tcRho(rhs.body, exp_type);
        if (rhs.body.loc) pop_source_span();
    }
}

// Fig 25. GUARD-OR
void TypeChecker::tcRho(Hs::MultiGuardedRHS& rhs, const Expected& exp_type)
{
    auto state2 = copy_clear_wanteds();
    if (rhs.decls)
        state2.infer_type_for_binds(*rhs.decls);

    auto etype = expTypeToType(exp_type);
    for(auto& guarded_rhs: rhs.guarded_rhss)
        state2.tcRho(guarded_rhs, Check(etype));

    current_wanteds() += state2.current_wanteds();
}

void TypeChecker::tcMatch(const Hs::MatchContext& ctx, Hs::MRule& m, const vector<Expected>& pat_types, const Expected& result_type)
{
    push_note(Note()<<"In equation `"<<ctx.print()<<get_lines(m.print())[0]<<"`");
    assert(m.patterns.size() == pat_types.size());

    auto state2 = copy_clear_wanteds();

    local_value_env penv;
    state2.tcPats(penv, m.patterns, pat_types, {}, [&](auto& /*penv2*/, auto& tc) {tc.tcRho(m.rhs,result_type);});

    current_wanteds() += state2.current_wanteds();
    pop_note();
}

void TypeChecker::tcMatches(const Hs::MatchContext& ctx, Hs::Matches& ms, const vector<Expected>& pat_types, const Expected& result_type)
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
            tcMatch(ctx, m, pat_types, result_type);

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
