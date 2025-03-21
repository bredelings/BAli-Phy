#ifndef CSE_H
#define CSE_H

#include "simplifier_options.H"
#include "computation/fresh_vars.H"
class Module;
#include "computation/core/ast.H"

std::vector<Core2::Decls<>> cse_module(const simplifier_options& opts, FreshVarState& fvstate, Module&, const std::vector<Core2::Decls<>>& decl_groups);

#endif
