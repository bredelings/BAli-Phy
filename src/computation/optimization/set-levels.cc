#include <vector>
#include <set>
#include "set-levels.H"
#include "computation/expression/expression.H" // for is_reglike( )
#include "computation/expression/substitute.H"
#include "computation/expression/var.H"
#include "computation/expression/lambda.H"
#include "computation/expression/let.H"
#include "computation/expression/apply.H"
#include "computation/expression/constructor.H"
#include "computation/expression/case.H"
#include "computation/operation.H"
#include "computation/module.H"
#include "util/set.H"

#include "free-vars.H"
#include "immer/map.hpp" // for immer::map

#include "range/v3/all.hpp"
namespace views = ranges::views;
namespace actions = ranges::actions;

using std::vector;
using std::set;
using std::pair;
using std::string;

const expression_ref un_fv(const expression_ref& AE)
{
    return AE.as_<annot_expression_ref<FreeVarSet>>().exp;
}

// This maps the in-name to (i) the out-name and (ii) the level, which is stored on the out-name.
typedef immer::map<var,var> level_env_t;

int max_level(const level_env_t& env, const FreeVarSet& free_vars)
{
    // Global variables that are free will not be in the env, so just ignore them.
    int level = 0;
    for(auto& x: free_vars)
        if (auto x_out = env.find(x))
        {
            assert(x_out->level);
            level = std::max(level, *x_out->level);
        }
    return level;
}

struct let_floater_state: public FreshVarSource
{
    var new_unique_var(const var& x, int level);
    var new_unique_var(const string& name, int level);

    expression_ref set_level(const expression_ref& AE, int level, const level_env_t& env);
    expression_ref set_level_maybe_MFE(const expression_ref& AE, int level, const level_env_t& env);

    level_env_t set_level_decl_group(CDecls& decls, const level_env_t& env);

    let_floater_state(FreshVarState& s):FreshVarSource(s) {}
};

var let_floater_state::new_unique_var(const var& x, int level)
{
    auto x2 = get_fresh_var(x);
    x2.level = level;
    return x2;
}


var let_floater_state::new_unique_var(const string& name, int level)
{
    auto x = get_fresh_var(name);
    x.level = level;
    return x;
}

var strip_level(var x)
{
    x.level.reset();
    return x;
}

expression_ref strip_level_from_pattern(const expression_ref& pattern)
{
    if (is_var(pattern))
        return strip_level(pattern.as_<var>());
    else if (not pattern.size())
        return pattern;
    else
    {
        object_ptr<expression> pattern2 = pattern.as_expression().clone();

        for(auto& Ex: pattern2->sub)
        {
            assert(is_var(Ex));

            Ex = strip_level_from_pattern(Ex);
        }
        return pattern2;
    }
}

var subst_var(var x, const level_env_t& env)
{
    // We should handle this in ... desugar?
    if (is_wildcard(x)) return x; 

    auto record = env.find(x);
    assert(record);

    x.level = record->level;
    x.name = record->name;
    x.index = record->index;

    return x;
}

var subst_var(const expression_ref& E, const level_env_t& env)
{
    const auto& x = E.as_<var>();
    return subst_var(x, env);
}

expression_ref subst_pattern(const expression_ref& pattern, const level_env_t& env)
{
    // I THINK that these should never be VARs in the current paradigm... but we should fix that.

    if (is_var(pattern))
        return strip_level(subst_var(pattern, env));
    else if (not pattern.size())
        return pattern;
    else
    {
        object_ptr<expression> pattern2 = pattern.as_expression().clone();

        for(auto& Ex: pattern2->sub)
        {
            assert(is_var(Ex));

            Ex = subst_pattern(Ex, env);
        }
        return pattern2;
    }
}

level_env_t let_floater_state::set_level_decl_group(CDecls& decls, const level_env_t& env)
{
    FreeVarSet free_vars;
    vector<var> binders;
    for(auto& [x,rhs]: decls)
    {
        free_vars = get_union(free_vars, get_free_vars(rhs));
        binders.push_back(x);
    }
    free_vars = erase(free_vars, binders);

    int level2 = max_level(env, free_vars);

    auto env2 = env;
    for(auto& [x,rhs]: decls)
    {
        if (not x.is_exported)
        {
            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});
            x = x2;
        }
        else
            x.level = 0;
    }

    for(auto& [var,rhs]: decls)
        rhs = set_level(rhs, level2, env2);

    return env2;
}

expression_ref let_floater_state::set_level(const expression_ref& AE, int level, const level_env_t& env)
{
    const auto& E = AE.as_<annot_expression_ref<FreeVarSet>>().exp;

    // 1. Var
    if (is_var(E))
    {
        const auto& x = E.as_<var>();
        // Top-level symbols from this module and other modules won't be in the env.
        if (auto x_out = env.find(x))
            return strip_level(*x_out);
        else
            return strip_level(x);
    }

    // 4. Lambda
    else if (is_lambda_exp(E))
    {
        int level2 = level + 1;
        auto env2 = env;

        vector<var> args;
        auto AE2 = AE;
        while(is_lambda_exp(un_fv(AE2)))
        {
            auto& E2 = un_fv(AE2);

            auto x = E2.sub()[0].as_<var>();

            // assert that none of the other args have the same name!
            // we should check this in the renamer, I think.

            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});

            args.push_back(x2);
            AE2 = E2.sub()[1];
        }

        auto E2 = set_level_maybe_MFE(AE2, level2, env2);

        for(auto x2 : args | views::reverse)
            E2 = lambda_quantify(strip_level(x2),E2);

        return E2;
    }

    // 4. Case
    else if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;

        object = set_level_maybe_MFE(object, level, env);

        int level2 = level+1; // Increment level, since we're going to float out of case alternatives.

        // Don't float out the entire case alternative if this isn't changeable.
        bool non_changeable = alts.size() == 1 and alts[0].pattern.is_a<var>();

        for(auto& [pattern, body]: alts)
        {
            // Extend environment with pattern vars at level2
            auto env2 = env;
            for(auto binder: get_vars(pattern))
            {
                auto binder2 = new_unique_var(binder, level2);
                env2 = env2.insert({binder,binder2});
            }

            pattern = subst_pattern(pattern, env2);

            body = non_changeable?
                set_level(body, level2, env2):
                set_level_maybe_MFE(body, level2, env2);
        }

        return make_case_expression(object, alts);
    }

    // 5. Let
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        auto env2 = set_level_decl_group(L.binds, env);

        L.body = set_level_maybe_MFE(L.body, level, env2);

        return L;
    }

    // 2. Constant
    else if (not E.size())
        return E;

    // 3. Apply or constructor or Operation
    else if (is_apply_exp(E) or is_constructor_exp(E) or is_non_apply_op_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();

        // All the arguments except the first one are supposed to be vars .... I think?
        for(int i=0;i<E.size();i++)
            V2->sub[i] = set_level(V2->sub[i], level, env);

        return V2;
    }

    std::abort();
}

expression_ref let_floater_state::set_level_maybe_MFE(const expression_ref& AE, int level, const level_env_t& env)
{
    int level2 = max_level(env, get_free_vars(AE));
    const auto& E = un_fv(AE);
    if (level2 < level and not is_var(E)) // and not is_WHNF(E))
    {
        auto E = set_level(AE, level2, env);
        var v = new_unique_var("$v", level2);
        return let_expression({{v,E}},v);
    }
    else
        return set_level(AE, level, env);
}

vector<CDecls> set_level_for_module(FreshVarState& fresh_var_state, const vector<Core2::Decls<>>& decl_groups_in)
{
    auto decl_groups = decl_groups_to_expression_ref(decl_groups_in);

    let_floater_state state(fresh_var_state);
    level_env_t env;
    for(auto& decls: decl_groups)
    {
        for(auto& [x,rhs]: decls)
            rhs = add_free_variable_annotations(rhs);
        env = state.set_level_decl_group(decls, env);
    }

    return decl_groups;
}
