#ifndef SET_LEVELS_H
#define SET_LEVELS_H

#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/fresh_vars.H"
#include "computation/core/ast.H"

var strip_level(var x);
expression_ref strip_level_from_pattern(const expression_ref& pattern);
std::vector<CDecls> set_level_for_module(FreshVarState& fresh_var_state, const std::vector<Core2::Decls<>>& decl_groups);

#endif
