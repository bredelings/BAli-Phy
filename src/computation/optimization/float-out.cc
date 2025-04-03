#include <vector>
#include <set>
#include "set-levels.H"
#include "float-out.H"
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
#include "computation/core/func.H"  // for lambda_quantify( )
#include "util/set.H"

#include "free-vars.H"
#include "immer/map.hpp" // for immer::map

#include "range/v3/all.hpp"
namespace views = ranges::views;

using std::vector;
using std::set;
using std::pair;
using std::string;
using std::tuple;



int get_level(const CDecls& decl_group)
{
    assert( not decl_group.empty() );
    assert( decl_group[0].first.level );

    auto level = *decl_group[0].first.level;

#ifndef NDEBUG
    for(int i=1; i<decl_group.size(); i++)
    {
        assert( decl_group[i].first.level );
        assert( *decl_group[i].first.level == level );
    }
#endif

    return level;
}

struct float_binds_t
{
    Core2::Decls<> top_binds;
    std::map<int,vector<Core2::Decls<>>> level_binds;

    vector<Core2::Decls<>> get_decl_groups_at_level(int level);

    void append_top(Core2::Decls<>&);

    void append_level(int level, Core2::Decls<>&);

    void append_level(int level, vector<Core2::Decls<>>&);

    void append(float_binds_t& float_binds2);

    float_binds_t() = default;
    float_binds_t(const float_binds_t&) = default;
    float_binds_t(float_binds_t&&) = default;
    float_binds_t& operator=(const float_binds_t&) = default;
    float_binds_t& operator=(float_binds_t&&) = default;
};

vector<Core2::Decls<>> float_binds_t::get_decl_groups_at_level(int level)
{
    auto iter = level_binds.find(level);
    if (iter == level_binds.end())
        return {};

    vector<Core2::Decls<>> decl_groups;
    std::swap(decl_groups, iter->second);
    level_binds.erase(iter);

    return decl_groups;
}

pair<vector<var>,expression_ref> get_lambda_binders(expression_ref E)
{
    assert(is_lambda_exp(E));
    vector<var> binders;
    while(is_lambda_exp(E))
    {
        auto x = E.sub()[0].as_<var>();
        binders.push_back(x);
        E = E.sub()[1];
    }
    return {std::move(binders), E};
}

tuple<vector<Levels::Var>,Levels::Exp> get_lambda_binders(Levels::Exp E)
{
    assert(E.to_lambda());
    vector<Levels::Var> binders;
    while(auto L = E.to_lambda())
    {
        binders.push_back(L->x);
        E = L->body;
    }
    return {std::move(binders), E};
}

expression_ref make_lambda(const vector<var>& args, expression_ref E)
{
    for(auto x : args | views::reverse)
        E = lambda_quantify(x,E);
    return E;
}

tuple<Core2::Exp<>, float_binds_t>
float_lets(const expression_ref& E, int level);

Core2::Exp<> install_current_level(float_binds_t& float_binds, int level, Core2::Exp<> E)
{
    auto decl_groups_here = float_binds.get_decl_groups_at_level(level);
    for(auto& decls: decl_groups_here | views::reverse)
        E = Core2::Let<>(decls, E);
    return E;
}

tuple<Core2::Exp<>, float_binds_t>
float_lets_install_current_level(const expression_ref& E, int level)
{
    auto [E2, float_binds] = float_lets(E,level);
    auto E3 = install_current_level(float_binds, level, E2);
    return {E3, float_binds};
}

void append(vector<Core2::Decls<>>& decl_groups1, vector<Core2::Decls<>>& decl_groups2)
{
    assert(&decl_groups1 != &decl_groups2);
    for(auto& decls: decl_groups2)
        decl_groups1.push_back(std::move(decls));
}

void float_binds_t::append_top(Core2::Decls<>& decls)
{
    if (not top_binds.empty())
    {
        for(auto& decl: decls)
            top_binds.push_back( std::move(decl) );

        decls.clear();
    }
    else
        std::swap(top_binds,decls);
}

void float_binds_t::append_level(int level, vector<Core2::Decls<>>& decl_groups)
{
    if (auto iter = level_binds.find(level); iter != level_binds.end())
    {
        ::append(level_binds[level], decl_groups);

        decl_groups.clear();
    }
    else
        std::swap(level_binds[level], decl_groups);
}

void float_binds_t::append_level(int level, Core2::Decls<>& decls)
{
    vector<Core2::Decls<>> decl_groups;
    decl_groups.push_back( std::move(decls) );
    append_level( level, decl_groups );
}

void float_binds_t::append(float_binds_t& float_binds2)
{
    assert(this != &float_binds2);

    append_top(float_binds2.top_binds);

    for(auto& [level,decl_groups]: float_binds2.level_binds)
        append_level(level, decl_groups);
}

tuple<Core2::Decls<>,float_binds_t,int> float_out_from_decl_group(const CDecls& decls_in)
{
    Core2::Decls<> decls_out;
    int level2 = get_level(decls_in);

    float_binds_t float_binds;
    for(auto& [x,rhs]: decls_in)
    {
        auto [rhs2, float_binds_x] = float_lets_install_current_level(rhs, level2);

        float_binds.append(float_binds_x);

        decls_out.push_back({to_core(x), rhs2});
    }

    // We need to move any level-0 bindings to the top level.
    // Otherwise (i) some of their components and/or (ii) components from the let body
    //  could get floated above their binders.
    if (level2 == 0)
    {
        for(auto& decl: decls_out)
            float_binds.top_binds.push_back( decl );
        decls_out.clear();
    }

    return {std::move(decls_out), std::move(float_binds), level2};
}

tuple<Core2::Exp<>,float_binds_t>
float_lets(const Levels::Exp& E, int level)
{
    return float_lets(levels_to_expression_ref(E), level);
}

tuple<Core2::Exp<>,float_binds_t>
float_lets(const expression_ref& E_, int level)
{
    auto E = E_;
    auto EE = to_levels_exp(E);
    
    // 1. Var
    if (auto V = EE.to_var())
    {
        return {strip_level(*V), {}};
    }

    // 4. Apply @ E x1 x2 x3 ... x[n-1];
    else if (auto A = EE.to_apply())
    {
        auto [head2, float_binds] = float_lets(A->head, level);

        return {Core2::Apply{head2, strip_levels(A->args)}, float_binds};
    }

    // 5. Lambda
    else if (auto L = EE.to_lambda())
    {
        auto [binders,body] = get_lambda_binders(*L);
        auto binders2 = strip_levels(binders);

        int level2 = level + 1;

        auto [body2, float_binds] = float_lets_install_current_level(levels_to_expression_ref(body), level2);

        return {lambda_quantify(binders2, body2), float_binds};
    }

    // 6. Case
    else if (auto C = EE.to_case())
    {
        auto [object2, float_binds] = float_lets(C->object, level);
        int level2 = level + 1;

        Core2::Alts<> alts2;
        for(auto& [pattern, body]: C->alts)
        {
            auto pattern2 = strip_levels_from_pattern(pattern);
            auto [body2, float_binds_alt] = float_lets_install_current_level(levels_to_expression_ref(body),level2);

            alts2.push_back({pattern2, body2});
            float_binds.append(float_binds_alt);
        }

        return {Core2::Case{object2, alts2}, float_binds};
    }

    // 7. Let
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        auto [decls, float_binds, level2] = float_out_from_decl_group(L.binds);
        auto Lbinds = decls;
        assert(level2 <= level);

        auto [body, float_binds_from_body] = float_lets(L.body, level);

        float_binds.append(float_binds_from_body);

        Core2::Exp<> E2;
        if (level2 < level)
        {
            // The decls here have to go BEFORE the decls from the (i) the body and (ii) the decl rhs's.
            float_binds_t float_binds_first;
            if (level2 == 0)
                float_binds_first.append_top( Lbinds );
            else
                float_binds_first.append_level( level2, Lbinds );
            float_binds_first.append(float_binds);
            std::swap(float_binds_first, float_binds);
            E2 = body;
        }
        // Prevents floated bindings at the same level from getting installed HIGHER than
        // bindings that they reference.
        // Does this place floated bindings as deep as possible at the correct level?
        else
        {
            E2 = install_current_level(float_binds, level, body);

            if (not Lbinds.empty())
                E2 = Core2::Let<>(Lbinds, E2);
        }

        return {E2, float_binds};
    }

    // 2. Constant
    else if (auto C = EE.to_constant())
        return {*C, {}};


    else if (auto C = EE.to_conApp())
    {
        return {Core2::ConApp<>{C->head, strip_levels(C->args)}, {}};
    }
    else if (auto B = EE.to_builtinOp())
    {
        return {Core2::BuiltinOp<>{B->lib_name, B->func_name, strip_levels(B->args), B->op}, {}};
    }

    std::abort();
}

void float_out_from_module(FreshVarState& fresh_var_state, vector<Core2::Decls<>>& core_decl_groups)
{
    auto decl_groups = set_level_for_module(fresh_var_state, core_decl_groups);

    vector<Core2::Decls<>> core_decl_groups2;

    for(auto& decl_group: decl_groups)
    {
        // FIXME - should we remove empty groups before we get here?
        if (decl_group.empty()) continue;

        auto [decls, float_binds, level2] = float_out_from_decl_group(decl_group);

        assert(float_binds.level_binds.empty());

        // Why put these at the END?
        for(auto& decl: float_binds.top_binds)
            decls.push_back( decl );

        core_decl_groups2.push_back(decls); //decl_group = decls;
    }

    std::swap(core_decl_groups, core_decl_groups2);
}
