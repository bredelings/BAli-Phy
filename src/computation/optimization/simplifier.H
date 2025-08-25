#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>

#include "computation/optimization/occurrence.H"
#include "computation/fresh_vars.H"
#include "immer/map.hpp"
#include "simplifier_options.H"
#include "substitution.H"
#include "inliner.H"

class module_loader;

typedef std::pair<std::optional<Occ::Exp>,occurrence_info> bound_variable_info;

typedef immer::map<Occ::Var, bound_variable_info> in_scope_set;

int get_n_lambdas1(Occ::Exp E);

Occ::Exp peel_n_lambdas1(Occ::Exp E, int n);

class Module;

std::vector<Core2::Decls<>> simplify_module_gently(const simplifier_options&, FreshVarState&, Module&, const std::vector<Core2::Decls<>>& decl_groups);
std::vector<Core2::Decls<>> simplify_module(const simplifier_options&, FreshVarState&, Module&, const std::vector<Core2::Decls<>>& decl_groups);

class SimplifierState: FreshVarSource
{
    simplifier_options options;

    Module& this_mod;

public:

    Occ::Var get_new_name(Occ::Var x, const in_scope_set& bound_vars);

    Occ::Var rename_var(const Occ::Var& x, simplifier::substitution& S, const in_scope_set& bound_vars);
    Occ::Var rename_and_bind_var(const Occ::Var& Evar, simplifier::substitution& S, in_scope_set& bound_vars);

    bool small_enough(const Occ::Exp& rhs, const inline_context& context);

    bool do_inline(const Occ::Exp& rhs, const occurrence_info& occur, const inline_context& context);

    bool do_inline_multi(const Occ::Exp& rhs, const inline_context& context);

    Occ::Exp
    consider_inline(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    simplify(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<simplifier::substitution,in_scope_set> rename_and_bind_pattern_vars(Occ::Pattern& pattern, const simplifier::substitution& S, const in_scope_set& bound_vars);

    Occ::Exp
    rebuild_case(Occ::Exp object, const Occ::Alts& alts, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp
    rebuild_case_inner(Occ::Exp object, Occ::Alts alts, const simplifier::substitution& S, const in_scope_set& bound_vars);

    Occ::Exp
    rebuild_let(const Occ::Decls& decls, Occ::Exp E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    Occ::Exp rebuild_apply(Occ::Exp E, const Occ::Var& arg, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<Occ::Decls, simplifier::substitution>
    simplify_decls(const Occ::Decls& orig_decls, const simplifier::substitution& S, in_scope_set bound_vars, bool is_top_level);

    std::vector<Core2::Decls<>>
    simplify_module_one(const std::vector<Core2::Decls<>>& decl_groups);

    SimplifierState(const simplifier_options& opts, FreshVarState& s, Module& m);
};



#endif
