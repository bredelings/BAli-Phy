#ifndef SIMPLIFIER_STATE_H
#define SIMPLIFIER_STATE_H

#include <vector>
#include "simplifier.H"
#include "inliner.H"
#include "computation/fresh_vars.H"


class SimplifierState: FreshVarSource
{
    simplifier_options options;
public:

    var get_new_name(var x, const in_scope_set& bound_vars);

    var rename_var(const expression_ref& Evar, simplifier::substitution& S, const in_scope_set& bound_vars);

    var rename_and_bind_var(const expression_ref& Evar, simplifier::substitution& S, in_scope_set& bound_vars);

    bool small_enough(const expression_ref& rhs, const inline_context& context);

    bool do_inline(const expression_ref& rhs, const occurrence_info& occur, const inline_context& context);

    bool do_inline_multi(const expression_ref& rhs, const inline_context& context);

    expression_ref
    consider_inline(const expression_ref& E, in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    simplify(const expression_ref& E, const simplifier::substitution& S, in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    rebuild(const expression_ref& E, in_scope_set& bound_vars, const inline_context& context);

    std::tuple<CDecls,simplifier::substitution> rename_and_bind_pattern_vars(expression_ref& pattern, const simplifier::substitution& S, in_scope_set& bound_vars);

    expression_ref
    rebuild_case(expression_ref object, const Core::Alts& alts, const simplifier::substitution& S, in_scope_set& bound_vars, const inline_context& context);

    expression_ref
    rebuild_case_inner(expression_ref object, Core::Alts alts, const simplifier::substitution& S, in_scope_set& bound_vars);

    expression_ref
    rebuild_let(const CDecls& decls, expression_ref E, const simplifier::substitution& S, in_scope_set& bound_vars, const inline_context& context);

    simplifier::substitution
    simplify_decls(CDecls& orig_decls, const simplifier::substitution& S, in_scope_set& bound_vars, bool is_top_level);
    
    std::vector<CDecls>
    simplify_module_one(const std::map<var, expression_ref>&, const std::set<var>&, const std::vector<CDecls>& decl_groups);

    SimplifierState(const simplifier_options& opts, FreshVarState& s);
};

#endif
