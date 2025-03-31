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

    pair<CDecls,level_env_t> set_level_decl_group(const CDecls& decls, const level_env_t& env);

    let_floater_state(FreshVarState& s):FreshVarSource(s) {}
};

var let_floater_state::new_unique_var(const var& x, int level)
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

    var x2;
    x2.level = record->level;
    x2.name = record->name;
    x2.index = record->index;

    return x2;
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

pair<CDecls,level_env_t> let_floater_state::set_level_decl_group(const CDecls& decls_in, const level_env_t& env)
{
    FreeVarSet free_vars;
    vector<var> binders;
    for(auto& [x,rhs]: decls_in)
    {
        free_vars = get_union(free_vars, get_free_vars(rhs));
        binders.push_back(x);
    }
    free_vars = erase(free_vars, binders);

    int level2 = max_level(env, free_vars);

    auto env2 = env;
    for(auto& [x,rhs]: decls_in)
    {
        if (not x.is_exported)
        {
            auto x2 = new_unique_var(x, level2);
            env2 = env2.insert({x,x2});
        }
    }

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
        decls_out.push_back({x2,rhs2});
    }

    return {decls_out, env2};
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

    else if (is_apply_exp(E))
    {
        auto head2 = set_level_maybe_MFE(E.sub()[0], level, env);

        vector<expression_ref> args2;
        for(int i=1;i<E.sub().size();i++)
            args2.push_back(set_level(E.sub()[i], level, env));

        return apply_expression(head2, args2);
    }

    // 4. Case
    else if (auto C = parse_case_expression(E))
    {
        auto& [object, alts] = *C;

        auto object2 = set_level_maybe_MFE(object, level, env);

        int level2 = level+1; // Increment level, since we're going to float out of case alternatives.

        // Don't float out the entire case alternative if this isn't changeable.
        bool non_changeable = alts.size() == 1 and alts[0].pattern.is_a<var>();

        Core::Alts alts2;
        for(auto& [pattern, body]: alts)
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
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        auto [binds2, env2] = set_level_decl_group(L.binds, env);

        auto body2 = set_level_maybe_MFE(L.body, level, env2);

        return let_expression(binds2, body2);
    }

    // 2. Constant
    else if (not E.size())
        return E;

    // 3. Apply or constructor or Operation
    else if (is_constructor_exp(E) or is_non_apply_op_exp(E))
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

vector<CDecls> set_level_for_module(FreshVarState& fresh_var_state, const vector<Core2::Decls<>>& decl_groups)
{
    let_floater_state state(fresh_var_state);
    level_env_t env;

    vector<CDecls> decl_groups_out;
    for(auto& decls: decl_groups)
    {
        CDecls fv_decls;
        for(auto& [x,rhs]: to_expression_ref(decls))
            fv_decls.push_back({x, add_free_variable_annotations(rhs)});

        auto [level_decls,env2] = state.set_level_decl_group(fv_decls, env);
        env = env2;

        decl_groups_out.push_back(level_decls);
    }

    return decl_groups_out;
}
