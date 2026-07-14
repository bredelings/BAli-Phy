#include <vector>
#include <set>
#include "set-levels.H"
#include "float-out.H"
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



int get_level(const Levels::Decls& decl_group)
{
    assert( not decl_group.empty() );

    auto level = decl_group[0].x.info;

#ifndef NDEBUG
    for(int i=1; i<decl_group.size(); i++)
    {
        assert( decl_group[i].x.info == level );
    }
#endif

    return level;
}

struct float_binds_t
{
    Core::Binds<> top_binds;
    std::map<int,Core::Binds<>> level_binds;

    Core::Binds<> get_binds_at_level(int level);

    void append_top(Core::Binds<>&);

    void append_level(int level, Core::Binds<>&);

    void append(float_binds_t& float_binds2);

    float_binds_t() = default;
    float_binds_t(const float_binds_t&) = default;
    float_binds_t(float_binds_t&&) = default;
    float_binds_t& operator=(const float_binds_t&) = default;
    float_binds_t& operator=(float_binds_t&&) = default;
};

Core::Binds<> float_binds_t::get_binds_at_level(int level)
{
    auto iter = level_binds.find(level);
    if (iter == level_binds.end())
        return {};

    Core::Binds<> binds;
    std::swap(binds, iter->second);
    level_binds.erase(iter);

    return binds;
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

tuple<Core::Exp<>, float_binds_t>
float_lets(const Levels::Exp& E, int level);

Core::Exp<> install_current_level(float_binds_t& float_binds, int level, Core::Exp<> E)
{
    auto binds_here = float_binds.get_binds_at_level(level);
    return make_lets(std::move(binds_here), std::move(E));
}

tuple<Core::Exp<>, float_binds_t>
float_lets_install_current_level(const Levels::Exp& E, int level)
{
    auto [E2, float_binds] = float_lets(E,level);
    auto E3 = install_current_level(float_binds, level, E2);
    return {E3, float_binds};
}

void float_binds_t::append_top(Core::Binds<>& binds)
{
    top_binds.insert(top_binds.end(), std::make_move_iterator(binds.begin()),
                     std::make_move_iterator(binds.end()));
    binds.clear();
}

void float_binds_t::append_level(int level, Core::Binds<>& binds)
{
    auto& destination = level_binds[level];
    destination.insert(destination.end(), std::make_move_iterator(binds.begin()),
                       std::make_move_iterator(binds.end()));
    binds.clear();
}

void float_binds_t::append(float_binds_t& float_binds2)
{
    assert(this != &float_binds2);

    append_top(float_binds2.top_binds);

    for(auto& [level,binds]: float_binds2.level_binds)
        append_level(level, binds);
}

tuple<Core::Decls<>,float_binds_t,int> float_out_from_decl_group(const Levels::Decls& decls_in)
{
    Core::Decls<> decls_out;
    int level2 = get_level(decls_in);

    float_binds_t float_binds;
    for(auto& [x,rhs]: decls_in)
    {
        auto [rhs2, float_binds_x] = float_lets_install_current_level(rhs, level2);

        float_binds.append(float_binds_x);

        decls_out.push_back({strip_level(x), rhs2});
    }

    // A top-level float extracted from a recursive RHS may refer to a binder in
    // decls_out, while the rewritten RHS refers to the float. Neither bind can
    // then precede the other, so all top-level RHS floats must join the Rec.
    // Widening their scope cannot capture distinct Core variable identities.
    // This mirrors GHC's addTopFloatPairs; a later occurrence pass may split SCCs.
    if (level2 == 0)
    {
        Core::Decls<> recursive_decls;
        for(auto& bind: float_binds.top_binds)
        {
            if (auto nonrec = std::get_if<Core::NonRec<>>(&bind))
                recursive_decls.push_back(std::move(nonrec->decl));
            else
            {
                auto& decls = std::get<Core::Rec<>>(bind).decls;
                recursive_decls.insert(recursive_decls.end(),
                                       std::make_move_iterator(decls.begin()),
                                       std::make_move_iterator(decls.end()));
            }
        }
        float_binds.top_binds.clear();
        recursive_decls.insert(recursive_decls.end(),
                               std::make_move_iterator(decls_out.begin()),
                               std::make_move_iterator(decls_out.end()));
        decls_out.clear();
        if (not recursive_decls.empty())
            float_binds.top_binds.push_back(Core::Rec<>{std::move(recursive_decls)});
    }

    return {std::move(decls_out), std::move(float_binds), level2};
}

// Float one explicit bind while preserving NonRec ordering and recursive scope.
tuple<std::optional<Core::Bind<>>,float_binds_t,int>
float_out_from_bind(const Levels::Bind& bind)
{
    if (auto nonrec = std::get_if<Levels::NonRec>(&bind))
    {
        int level = nonrec->decl.x.info;
        auto [rhs, float_binds] = float_lets_install_current_level(nonrec->decl.body, level);
        Core::Bind<> bind2 = Core::NonRec<>{{strip_level(nonrec->decl.x), std::move(rhs)}};
        if (level == 0)
        {
            // The NonRec binder was not in scope in its RHS, so its top-level
            // RHS floats can remain dependency-ordered before the binder.
            float_binds.top_binds.push_back(std::move(bind2));
            return {{}, std::move(float_binds), level};
        }
        return {std::move(bind2), std::move(float_binds), level};
    }

    auto [decls, float_binds, level] = float_out_from_decl_group(std::get<Levels::Rec>(bind).decls);
    if (decls.empty())
        return {{}, std::move(float_binds), level};
    return {Core::Rec<>{std::move(decls)}, std::move(float_binds), level};
}

tuple<Core::Exp<>,float_binds_t>
float_lets(const Levels::Exp& E, int level)
{
    // 1. Var
    if (auto V = E.to_var())
    {
        return {strip_level(*V), {}};
    }

    // 4. Apply @ E x1 x2 x3 ... x[n-1];
    else if (auto A = E.to_apply())
    {
        auto [head2, float_binds] = float_lets(A->head, level);

        auto [arg2, arg_float_binds] = float_lets(A->arg, level);

        float_binds.append(arg_float_binds);

        return {Core::Apply{head2, arg2}, float_binds};
    }

    // 5. Lambda
    else if (auto L = E.to_lambda())
    {
        auto [binders,body] = get_lambda_binders(*L);
        auto binders2 = strip_levels(binders);

        int level2 = level + 1;

        auto [body2, float_binds] = float_lets_install_current_level(body, level2);

        return {lambda_quantify(binders2, body2), float_binds};
    }

    // 6. Case
    else if (auto C = E.to_case())
    {
        auto [object2, float_binds] = float_lets(C->object, level);
        int level2 = level + 1;

        vector<Core::Alt<>> alts2;
        for(auto& [pattern, body]: C->alts)
        {
            auto pattern2 = strip_levels_from_pattern(pattern);
            auto [body2, float_binds_alt] = float_lets_install_current_level(body,level2);

            alts2.push_back({pattern2, body2});
            float_binds.append(float_binds_alt);
        }

        return {Core::Case<>{object2, alts2}, float_binds};
    }

    // 7. Let
    else if (auto L = E.to_let())
    {
        bool is_nonrec = L->to_nonrec();
        auto [bind, rhs_float_binds, level2] = float_out_from_bind(L->bind);
        assert(level2 <= level);

        auto [body, body_float_binds] = float_lets(L->body, level);

        Core::Exp<> E2;
        if (level2 < level)
        {
            float_binds_t float_binds;
            // NonRec RHS floats precede its binder. A top-level Rec has already
            // merged its mutually scoped RHS floats and returns no separate bind.
            if (is_nonrec or not bind)
                float_binds.append(rhs_float_binds);

            if (bind)
            {
                if (level2 == 0)
                    float_binds.top_binds.push_back(std::move(*bind));
                else
                    float_binds.level_binds[level2].push_back(std::move(*bind));
            }

            if (not is_nonrec and bind)
                float_binds.append(rhs_float_binds);
            float_binds.append(body_float_binds);
            E2 = body;
            return {E2, std::move(float_binds)};
        }
        // Prevents floated bindings at the same level from getting installed HIGHER than
        // bindings that they reference.
        // Does this place floated bindings as deep as possible at the correct level?
        else
        {
            rhs_float_binds.append(body_float_binds);
            E2 = install_current_level(rhs_float_binds, level, body);

            if (bind)
                E2 = Core::Let<>(std::move(*bind), E2);
        }

        return {E2, std::move(rhs_float_binds)};
    }

    // 2. Constant
    else if (auto C = E.to_constant())
        return {*C, {}};


    else if (auto C = E.to_conApp())
    {
        float_binds_t float_binds;
        std::vector<Core::Exp<>> args;
        for(auto& arg: C->args)
        {
            auto [arg2, arg_float_binds] = float_lets(arg,level);
            float_binds.append(arg_float_binds);
            args.push_back(arg2);
        }
        return {Core::ConApp<>{C->head, args}, float_binds};
    }
    else if (auto B = E.to_builtinOp())
    {
        float_binds_t float_binds;
        std::vector<Core::Exp<>> args;
        for(auto& arg: B->args)
        {
            auto [arg2, arg_float_binds] = float_lets(arg,level);
            float_binds.append(arg_float_binds);
            args.push_back(arg2);
        }
        return {Core::BuiltinOp<>(B->lib_name, B->func_name, B->call_conv, args, B->op), float_binds};
    }

    std::abort();
}

void float_out_from_module(FreshVarState& fresh_var_state, Core::Binds<>& core_binds)
{
    auto level_binds = set_level_for_module(fresh_var_state, core_binds);

    Core::Binds<> core_binds2;

    for(auto& bind: level_binds)
    {
        auto [bind2, float_binds, level] = float_out_from_bind(bind);

        assert(float_binds.level_binds.empty());
        assert(level == 0);

        if (bind2)
            core_binds2.push_back(std::move(*bind2));
        core_binds2.insert(core_binds2.end(),
                           std::make_move_iterator(float_binds.top_binds.begin()),
                           std::make_move_iterator(float_binds.top_binds.end()));
    }

    core_binds = std::move(core_binds2);
}
