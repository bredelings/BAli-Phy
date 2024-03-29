#ifndef SET_LEVELS_H
#define SET_LEVELS_H

#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/fresh_vars.H"

var strip_level(var x);
expression_ref strip_level_from_pattern(const expression_ref& pattern);
void set_level_for_module(FreshVarState& fresh_var_state, std::vector<CDecls>& module);

#endif
