#include <string>
#include <vector>
#include <set>
#include <map>
#include <optional>

#include "rename.H"
#include "computation/module.H"
#include "computation/expression/apply.H"
#include "computation/expression/tuple.H"
#include "util/set.H"

using std::string;
using std::vector;
using std::pair;
using std::set;
using std::optional;
using std::map;

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

Hs::Match renamer_state::rename(Hs::Match match, const bound_var_info& bound, set<string>& free_vars)
{
    for(auto& mrule: match.rules)
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
    }

    return match;
}

