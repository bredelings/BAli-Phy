#ifndef FLOAT_OUT_H
#define FLOAT_OUT_H

#include "computation/fresh_vars.hh"
#include "computation/core/ast.hh"

void float_out_from_module(FreshVarState& fresh_var_state, Core::Binds<>& binds);

#endif
