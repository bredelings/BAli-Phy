#ifndef FLOAT_OUT_H
#define FLOAT_OUT_H

#include "computation/fresh_vars.H"
#include "computation/core/ast.H"

void float_out_from_module(FreshVarState& fresh_var_state, std::vector<Core::Decls<>>& decl_groups);

#endif
