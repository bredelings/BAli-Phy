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
#include "computation/operation.H"
#include "computation/module.H"
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
    std::map<int,vector<CDecls>> level_binds;

    vector<Core2::Decls<>> get_decl_groups_at_level(int level);

    void append_top(Core2::Decls<>&);

    void append_level(int level, CDecls&);

    void append_level(int level, const Core2::Decls<>&);

    void append_level(int level, vector<CDecls>&);

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

    vector<CDecls> decl_groups;
    std::swap(decl_groups, iter->second);
    level_binds.erase(iter);

    vector<Core2::Decls<>> decl_groups_core;
    for(auto& decls: decl_groups)
        decl_groups_core.push_back( to_core(decls) );

    return decl_groups_core;
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

void append(vector<CDecls>& decl_groups1, vector<CDecls>& decl_groups2)
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

void float_binds_t::append_level(int level, vector<CDecls>& decl_groups)
{
    if (auto iter = level_binds.find(level); iter != level_binds.end())
    {
        ::append(level_binds[level], decl_groups);

        decl_groups.clear();
    }
    else
        std::swap(level_binds[level], decl_groups);
}

void float_binds_t::append_level(int level, CDecls& decls)
{
    vector<CDecls> decl_groups;
    decl_groups.push_back( std::move(decls) );
    append_level( level, decl_groups );
}

void float_binds_t::append_level(int level, const Core2::Decls<>& decls)
{
    vector<CDecls> decl_groups;
    decl_groups.push_back( to_expression_ref(decls) );
    append_level( level, decl_groups );
}

void float_binds_t::append(float_binds_t& float_binds2)
{
    assert(this != &float_binds2);

    append_top(float_binds2.top_binds);

    for(auto& [level,decl_groups]: float_binds2.level_binds)
        append_level(level, decl_groups);
}

tuple<CDecls,float_binds_t,int> float_out_from_decl_group(const CDecls& decls_in)
{
    auto decls = decls_in;
    int level2 = get_level(decls);

    float_binds_t float_binds;
    for(auto& [x,rhs]: decls)
    {
        x = strip_level(x);
        auto [rhs2, float_binds_x] = float_lets_install_current_level(rhs, level2);
        rhs = to_expression_ref(rhs2);

        float_binds.append(float_binds_x);
    }

    // We need to move any level-0 bindings to the top level.
    // Otherwise (i) some of their components and/or (ii) components from the let body
    //  could get floated above their binders.
    if (level2 == 0)
    {
        for(auto& [x,E]: decls)
            float_binds.top_binds.push_back( {to_core(x),to_core_exp(E)} );
        decls.clear();
    }

    return tuple<CDecls, float_binds_t,int>(std::move(decls), std::move(float_binds), level2);
}

tuple<Core2::Exp<>,float_binds_t>
float_lets(const expression_ref& E_, int level)
{
    auto E = E_;

    // 1. Var
    if (is_var(E))
    {
        auto x = E.as_<var>();
        x = strip_level(x);
        E = x;
        return {to_core_exp(E), {}};
    }

    // 4. Apply @ E x1 x2 x3 ... x[n-1];
    else if (is_apply_exp(E))
    {
        object_ptr<expression> V2 = E.as_expression().clone();
        auto [B_,float_binds] = float_lets(V2->sub[0], level);
        auto B = to_expression_ref(B_);
        V2->sub[0] = B;
#ifndef NDEBUG
        for(int i=1;i<V2->sub.size();i++)
                assert(is_var(V2->sub[i]));
#endif
        E = V2;
        return {to_core_exp(E), float_binds};
    }

    // 5. Lambda
    else if (is_lambda_exp(E))
    {
        auto [binders,body] = get_lambda_binders(E);
        for(auto& x: binders)
            x = strip_level(x);

        int level2 = level + 1;

        auto [body2, float_binds] = float_lets_install_current_level(body, level2);
        body = to_expression_ref(body2);

        E = make_lambda(binders,body);

        return {to_core_exp(E), float_binds};
    }

    // 6. Case
    else if (auto C = parse_case_expression(E))
    {
        auto& [object,alts] = *C;

        auto [object2, float_binds] = float_lets(object, level);
        object = to_expression_ref(object2);
        int level2 = level + 1;
        for(auto& [pattern, body]: alts)
        {
            pattern = strip_level_from_pattern(pattern);
            auto [body2, float_binds_alt] = float_lets_install_current_level(body,level2);
            body = to_expression_ref(body2);

            float_binds.append(float_binds_alt);
        }

        E = make_case_expression(object,alts);
        return {to_core_exp(E), float_binds};
    }

    // 7. Let
    else if (is_let_expression(E))
    {
        auto L = E.as_<let_exp>();

        auto [decls, float_binds, level2] = float_out_from_decl_group(L.binds);
        auto Lbinds = to_core(decls);
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
    else if (not E.size())
        return {to_core_exp(E), {}};


    // 3. Constructor or Operation
    else if (is_constructor_exp(E) or is_non_apply_op_exp(E))
        return {to_core_exp(E), {}};

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
        auto decls2 = to_core(decls);

        assert(float_binds.level_binds.empty());

        // Why put these at the END?
        for(auto& decl: float_binds.top_binds)
            decls2.push_back( decl );

        core_decl_groups2.push_back(decls2); //decl_group = decls;
    }

    std::swap(core_decl_groups, core_decl_groups2);
}
