#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/convert.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

#include "computation/fresh_vars.H"
#include "immer/map.hpp"
#include "simplifier_options.H"
#include "substitution.H"
#include "inliner.H"

class module_loader;

typedef std::pair<expression_ref,occurrence_info> bound_variable_info;

typedef immer::map<Occ::Var, bound_variable_info> in_scope_set;

int get_n_lambdas1(Occ::Exp E);

Occ::Exp peel_n_lambdas1(Occ::Exp E, int n);

class Module;

std::vector<CDecls> simplify_module_gently(const simplifier_options&, FreshVarState&, Module&, const std::vector<CDecls>& decl_groups);
std::vector<CDecls> simplify_module(const simplifier_options&, FreshVarState&, Module&, const std::vector<CDecls>& decl_groups);

class SimplifierState: FreshVarSource
{
    simplifier_options options;

    Module& this_mod;

public:

    Occ::Var get_new_name(Occ::Var x, const in_scope_set& bound_vars);

    Occ::Var rename_var(const Occ::Var& x, simplifier::substitution& S, const in_scope_set& bound_vars);
    Occ::Var rename_and_bind_var(const Occ::Var& Evar, simplifier::substitution& S, in_scope_set& bound_vars);

    bool small_enough(const expression_ref& rhs, const inline_context& context);

    bool do_inline(const expression_ref& rhs, const occurrence_info& occur, const inline_context& context);

    bool do_inline_multi(const expression_ref& rhs, const inline_context& context);

    expression_ref
    consider_inline(const Occ::Var& x, const in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    simplify(const expression_ref& E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    simplify(const Occ::Exp& E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    rebuild(const expression_ref& E, const in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    rebuild(const Occ::Exp& E, const in_scope_set& bound_vars, const inline_context& context);

    std::tuple<CDecls,simplifier::substitution,in_scope_set> rename_and_bind_pattern_vars(expression_ref& pattern, const simplifier::substitution& S, const in_scope_set& bound_vars);

    expression_ref
    rebuild_case(Occ::Exp object, const Occ::Alts& alts, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    rebuild_case_inner(expression_ref object, Core::Alts alts, const simplifier::substitution& S, const in_scope_set& bound_vars);

    expression_ref
    rebuild_let(const Occ::Decls& decls, Occ::Exp E, const simplifier::substitution& S, const in_scope_set& bound_vars, const inline_context& context);

    simplifier::substitution
    simplify_decls(Occ::Decls& orig_decls, const simplifier::substitution& S, in_scope_set bound_vars, bool is_top_level);
    
    std::vector<CDecls>
    simplify_module_one(const std::vector<CDecls>& decl_groups);

    SimplifierState(const simplifier_options& opts, FreshVarState& s, Module& m);
};



#endif
