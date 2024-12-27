#ifndef FLOAT_OUT_H
#define FLOAT_OUT_H

#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/fresh_vars.H"
#include "computation/core/ast.H"

void float_out_from_module(FreshVarState& fresh_var_state, std::vector<Core2::Decls<>>& decl_groups);

#endif
