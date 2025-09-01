#include <vector>
#include <set>
#include "set-levels.H"
#include "computation/fresh_vars.H"
#include "computation/module.H"
#include "util/set.H"
#include "core/func.H" // for lambda_quantify( )

#include "free-vars.H"
#include "immer/map.hpp" // for immer::map

#include "range/v3/all.hpp"
namespace views = ranges::views;
namespace actions = ranges::actions;

using std::vector;
using std::set;
using std::pair;
using std::string;

// This maps the in-name to (i) the out-name and (ii) the level, which is stored on the out-name.
typedef immer::map<FV::Var,Levels::Var> level_env_t;

int max_level(const level_env_t& env, const FreeVars& free_vars)
{
    // Global variables that are free will not be in the env, so just ignore them.
    int level = 0;
    for(auto& x: free_vars)
        if (auto x_out = env.find(x))
        {
            level = std::max(level, x_out->info);
        }
    return level;
}

struct let_floater_state: public FreshVarSource
{
    Levels::Var new_unique_var(const FV::Var& x, int level);
    Levels::Var new_unique_var(const string& name, int level);

    Levels::Exp set_level(const FV::Exp& AE, int level, const level_env_t& env);
    Levels::Exp set_level_maybe_MFE(const FV::Exp& AE, int level, const level_env_t& env);

    pair<Levels::Decls,level_env_t> set_level_decl_group(const FV::Decls& decls, const level_env_t& env);

    let_floater_state(FreshVarState& s):FreshVarSource(s) {}
};

Levels::Var let_floater_state::new_unique_var(const FV::Var& x, int level)
{
    // I guess we are assuming that the name is sufficient?
    return new_unique_var(x.name, level);
}


Levels::Var let_floater_state::new_unique_var(const string& name, int level)
{
    auto x = get_fresh_levels_var(name);
    x.info = level;
    return x;
}

Core2::Var<> strip_level(const Levels::Var& x)
{
    return Core2::Var<>(x.name, x.index, {}, x.is_exported);
}

vector<Core2::Var<>> strip_levels(const vector<Levels::Var>& xs)
{
    return xs | ranges::views::transform( [&](auto& x) {return strip_level(x);} ) | ranges::to<vector>();
}

Core2::Pattern<> strip_levels_from_pattern(const Levels::Pattern& pattern)
{
    return {pattern.head, strip_levels(pattern.args)};
}

Levels::Var add_level(const FV::Var& x, const level_env_t& env)
{
    // Top-level symbols from this module and other modules won't be in the env.
    if (auto x_out = env.find(x))
        return *x_out;
    else
        return Levels::Var(x.name, x.index, 0, x.is_exported);
}

vector<Levels::Var> add_levels_no_missing(const Vector<FV::Var>& xs, const level_env_t& env)
{
    return xs | ranges::views::transform( [&](auto& x) {return env.at(x);} ) | ranges::to<vector>();
}

vector<Levels::Var> add_levels(const Vector<FV::Var>& xs, const level_env_t& env)
{
    return xs | ranges::views::transform( [&](auto& x) {return add_level(x, env);} ) | ranges::to<vector>();
}

Levels::Pattern subst_pattern(const FV::Pattern& pattern, const level_env_t& env)
{
    return {pattern.head, add_levels_no_missing(pattern.args, env)};
}

pair<Levels::Decls,level_env_t> let_floater_state::set_level_decl_group(const FV::Decls& decls_in, const level_env_t& env)
{
    // 1. Get the free variables in the decl group
    FreeVars free_vars;
    vector<FV::Var> binders;
    for(auto& [x,rhs]: decls_in)
    {
        free_vars = get_union(free_vars, get_free_vars(rhs));
        binders.push_back(x);
    }
    free_vars = erase(free_vars, binders);

    // 2. Compute the level of the decl group
    int level2 = max_level(env, free_vars);

    // 3. Set the variable level to the decl-group level for all non-exported binders.
    auto env2 = env;
    for(auto& [x,rhs]: decls_in)
    {
        if (not x.is_exported)
        {
            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});
        }
    }

    // 4. Set the level on the let-binders and recurse into the bodies.
    Levels::Decls decls_out;
    for(auto& [x,rhs]: decls_in)
    {
        Levels::Var x2;
        if (x.is_exported)
        {
            x2 = Levels::Var(x.name, x.index, 0, x.is_exported);
        }
        else
            x2 = env2.at(x);

        auto rhs2 = set_level(rhs, level2, env2);
        decls_out.push_back({x2, rhs2});
    }

    return {decls_out, env2};
}

Levels::Exp let_floater_state::set_level(const FV::Exp& E, int level, const level_env_t& env)
{
    // 1. Var
    if (auto V = E.to_var())
        return add_level(*V, env);

    // 3. Lambda
    else if (E.to_lambda())
    {
        int level2 = level + 1;
        auto env2 = env;

        vector<Levels::Var> args;
        auto AE2 = E;
        while(auto L2 = AE2.to_lambda())
        {
            auto x = L2->x;

            // assert that none of the other args have the same name!
            // we should check this in the renamer, I think.

            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});

            args.push_back(x2);
            AE2 = L2->body;
        }

        auto E2 = set_level_maybe_MFE(AE2, level2, env2);

        return lambda_quantify(args, E2);
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        auto head2 = set_level_maybe_MFE(A->head, level, env);

        auto arg2 = add_level(A->arg, env);

        return Levels::Apply{head2, arg2};
    }
    // 4. Case
    else if (auto C = E.to_case())
    {
        auto object2 = set_level_maybe_MFE(C->object, level, env);

        int level2 = level+1; // Increment level, since we're going to float out of case alternatives.

        // Is it possible that we could choose a different alternative later?
        // If so, don't float out the entire case alternative if this isn't changeable.
        bool non_changeable = C->alts.size() == 1 and C->alts[0].pat.is_wildcard_pat();

        Levels::Alts alts2;
        for(auto& [pattern, body]: C->alts)
        {
            // Extend environment with pattern vars at level2
            auto env2 = env;
            for(auto binder: pattern.args)
            {
                auto binder2 = new_unique_var(binder, level2);
                env2 = env2.insert({binder,binder2});
            }

            auto pattern2 = subst_pattern(pattern, env2);

            /* We want to lift constant results out of case-alternatives so that our machinery
               can recognize that we got the "same result" when re-evaluating the case and taking
               the same branch */
            auto body2 = non_changeable?
                set_level(body, level2, env2):
                set_level_maybe_MFE(body, level2, env2);

            alts2.push_back({pattern2, body2});
        }

        return Levels::Case{object2, alts2};
    }

    // 5. Let
    else if (auto L = E.to_let())
    {
        auto [binds2, env2] = set_level_decl_group(L->decls, env);

        auto body2 = set_level_maybe_MFE(L->body, level, env2);

        return Levels::Let{binds2, body2};
    }

    // 2. Constant
    else if (auto C = E.to_constant())
        return *C;
    else if (auto C = E.to_conApp())
        return Levels::ConApp{C->head, add_levels(C->args, env)};
    else if (auto B = E.to_builtinOp())
        return Levels::BuiltinOp{B->lib_name, B->func_name, add_levels(B->args,env), B->op};

    std::abort();
}

Levels::Exp let_floater_state::set_level_maybe_MFE(const FV::Exp& E, int level, const level_env_t& env)
{
    int level2 = max_level(env, get_free_vars(E));
    if (level2 < level and not E.to_var()) // and not is_WHNF(E))
    {
        // See note in set_level on why we want to lift constants out of case alternatives.
        // If we have a 0-arg Constructure, then there's already a unique variable for it.
        if (auto con = E.to_conApp(); con and con->args.empty())
        {
            assert(level2 == 0);
            return Levels::Var(con->head, /*index*/ 0, /*level*/ 0);
        }

        auto E2 = set_level(E, level2, env);
        Levels::Var v = new_unique_var("$v", level2);
        return Levels::Let({{v,E2}}, v);
    }
    else
        return set_level(E, level, env);
}

vector<Levels::Decls> set_level_for_module(FreshVarState& fresh_var_state, const vector<Core2::Decls<>>& decl_groups)
{
    let_floater_state state(fresh_var_state);
    level_env_t env;

    vector<Levels::Decls> decl_groups_out;
    for(auto& decls: decl_groups)
    {
        FV::Decls fv_decls;
        for(auto& [x,rhs]: decls)
            fv_decls.push_back({x, add_free_variable_annotations(rhs)});

        auto [level_decls,env2] = state.set_level_decl_group(fv_decls, env);
        env = env2;

        decl_groups_out.push_back(level_decls);
    }

    return decl_groups_out;
}
