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
#include "computation/expression/convert.H"
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

// This maps the in-name to (i) the out-name and (ii) the level, which is stored on the out-name.
typedef immer::map<FV::Var,var> level_env_t;

int max_level(const level_env_t& env, const FreeVars& free_vars)
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
    var new_unique_var(const FV::Var& x, int level);
    var new_unique_var(const string& name, int level);

    expression_ref set_level(const FV::Exp& AE, int level, const level_env_t& env);
    expression_ref set_level_maybe_MFE(const FV::Exp& AE, int level, const level_env_t& env);

    pair<CDecls,level_env_t> set_level_decl_group(const FV::Decls& decls, const level_env_t& env);

    let_floater_state(FreshVarState& s):FreshVarSource(s) {}
};

var let_floater_state::new_unique_var(const FV::Var& x, int level)
{
    // I guess we are assuming that the name is sufficient?
    return new_unique_var(x.name, level);
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
    if (pattern.is_wildcard_pat())
        return Core2::WildcardPat();

    auto CP = pattern.to_con_pat();

    return Core2::ConPat<>{CP->head, strip_levels(CP->args)};
}

var subst_var(FV::Var x, const level_env_t& env)
{
    auto record = env.find(x);
    assert(record);

    var x2;
    x2.level = record->level;
    x2.name = record->name;
    x2.index = record->index;

    return x2;
}

expression_ref subst_pattern(const FV::Pattern& pattern, const level_env_t& env)
{
    // I THINK that these should never be VARs in the current paradigm... but we should fix that.
    if (pattern.is_wildcard_pat())
        return var(-1);
    else if (auto c = pattern.to_con_pat())
    {
        auto args = c->args | ranges::views::transform( [&](auto& x) {return expression_ref(subst_var(x,env));} ) | ranges::to<vector>();
        return expression_ref(constructor(c->head, args.size()), args);
    }
    else
        std::abort();
}

pair<CDecls,level_env_t> let_floater_state::set_level_decl_group(const FV::Decls& decls_in, const level_env_t& env)
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
    CDecls decls_out;
    for(auto& [x,rhs]: decls_in)
    {
        var x2;
        if (x.is_exported)
        {
            x2 = var(x.name, x.index, x.is_exported);
            x2.level = 0;
        }
        else
            x2 = env2.at(x);

        auto rhs2 = set_level(rhs, level2, env2);
        decls_out.push_back({x2, rhs2});
    }

    return {decls_out, env2};
}

expression_ref let_floater_state::set_level(const FV::Exp& E, int level, const level_env_t& env)
{
    // 1. Var
    if (auto V = E.to_var())
    {
        // Top-level symbols from this module and other modules won't be in the env.
        if (auto x_out = env.find(*V))
            return *x_out;
        else
        {
            auto x = var(V->name, V->index, V->is_exported);
            x.level = 0;
            return x;
        }
    }

    // 3. Lambda
    else if (E.to_lambda())
    {
        int level2 = level + 1;
        auto env2 = env;

        vector<var> args;
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

        for(auto x2 : args | views::reverse)
            E2 = lambda_quantify(x2,E2);

        return E2;
    }
    // 3. Apply
    else if (auto A = E.to_apply())
    {
        auto head2 = set_level_maybe_MFE(A->head, level, env);

        vector<expression_ref> args2;
        for(auto& arg: A->args)
            args2.push_back(set_level(arg, level, env));

        return apply_expression(head2, args2);
    }
    // 4. Case
    else if (auto C = E.to_case())
    {
        auto object2 = set_level_maybe_MFE(C->object, level, env);

        int level2 = level+1; // Increment level, since we're going to float out of case alternatives.

        // Don't float out the entire case alternative if this isn't changeable.
        bool non_changeable = C->alts.size() == 1 and C->alts[0].pat.is_wildcard_pat();

        Core::Alts alts2;
        for(auto& [pattern, body]: C->alts)
        {
            // Extend environment with pattern vars at level2
            auto env2 = env;
            for(auto binder: get_vars(pattern))
            {
                auto binder2 = new_unique_var(binder, level2);
                env2 = env2.insert({binder,binder2});
            }

            auto pattern2 = subst_pattern(pattern, env2);

            auto body2 = non_changeable?
                set_level(body, level2, env2):
                set_level_maybe_MFE(body, level2, env2);

            alts2.push_back({pattern2, body2});
        }

        return make_case_expression(object2, alts2);
    }

    // 5. Let
    else if (auto L = E.to_let())
    {
        auto [binds2, env2] = set_level_decl_group(L->decls, env);

        auto body2 = set_level_maybe_MFE(L->body, level, env2);

        return let_expression(binds2, body2);
    }

    // 2. Constant
    else if (auto C = E.to_constant())
        return to_expression_ref(*C);
    else if (auto C = E.to_conApp())
    {
        vector<expression_ref> args2;
        for(auto& arg: C->args)
            args2.push_back(set_level(arg, level, env));

        return expression_ref(constructor(C->head, C->args.size()), args2);

    }
    else if (auto B = E.to_builtinOp())
    {
        Operation O( (operation_fn)B->op, B->lib_name+":"+B->func_name);

        vector<expression_ref> args2;
        for(auto& arg: B->args)
            args2.push_back(set_level(arg, level, env));

        return expression_ref(O, args2);
    }

    std::abort();
}

expression_ref let_floater_state::set_level_maybe_MFE(const FV::Exp& E, int level, const level_env_t& env)
{
    int level2 = max_level(env, get_free_vars(E));
    if (level2 < level and not E.to_var()) // and not is_WHNF(E))
    {
        auto E2 = set_level(E, level2, env);
        var v = new_unique_var("$v", level2);
        return let_expression({{v,E2}},v);
    }
    else
        return set_level(E, level, env);
}

vector<CDecls> set_level_for_module(FreshVarState& fresh_var_state, const vector<Core2::Decls<>>& decl_groups)
{
    let_floater_state state(fresh_var_state);
    level_env_t env;

    vector<CDecls> decl_groups_out;
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
