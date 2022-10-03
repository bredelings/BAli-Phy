#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;

Hs::MultiGuardedRHS renamer_state::rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, const bound_var_info& binders, set<string>& free_vars)
{
    set<string> rhs_free_vars;
    auto R2 = rename(R, plus(bound, binders), rhs_free_vars);
    add(free_vars, minus(rhs_free_vars, binders));
    return R2;
}

Hs::MultiGuardedRHS renamer_state::rename(Hs::MultiGuardedRHS R, const bound_var_info& bound, set<string>& free_vars)
{
    bound_var_info binders;

    if (R.decls)
        binders = rename_decls(unloc(*R.decls), bound, binders, free_vars);

    for(auto& guarded_rhs: R.guarded_rhss)
    {
        for(auto& guard: guarded_rhs.guards)
            add(binders, rename_stmt(guard, bound, binders, free_vars));

        guarded_rhs.body = rename(guarded_rhs.body, bound, binders, free_vars);
    }

    return R;
}

Hs::MRule renamer_state::rename(Hs::MRule mrule, const bound_var_info& bound, set<string>& free_vars)
{
    bound_var_info binders;

    for(auto& arg: mrule.patterns)
    {
        auto new_binders = rename_pattern(arg);
        auto overlap = intersection(binders, new_binders);
        if (not overlap.empty())
        {
            string bad = *overlap.begin();
            throw myexception()<<"Function declaration uses variable '"<<bad<<"' twice:\n"<<" "<<mrule.print();
        }
        add(binders, new_binders);
    }

    mrule.rhs = rename(mrule.rhs, bound, binders, free_vars);

    return mrule;
}

Hs::Matches renamer_state::rename(Hs::Matches matches, const bound_var_info& bound, set<string>& free_vars)
{
    for(auto& mrule: matches)
        mrule = rename(mrule, bound, free_vars);

    return matches;
}

