#ifndef SIMPLIFIER_H
#define SIMPLIFIER_H

#include <set>
#include <map>
#include <utility>
#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/var.H"

#include "computation/fresh_vars.H"
#include "immer/map.hpp"
#include "simplifier_options.H"
#include "substitution.H"

class module_loader;

typedef std::pair<expression_ref,occurrence_info> bound_variable_info;

typedef immer::map<var, bound_variable_info> in_scope_set;

int get_n_lambdas1(const expression_ref& E);

expression_ref peel_n_lambdas1(const expression_ref& E, int n);

bool is_trivial(const expression_ref& E);

bool special_prelude_symbol(const std::string& name);

class Module;

std::vector<CDecls> simplify_module_gently(const simplifier_options&, FreshVarState&, Module&, const std::vector<CDecls>& decl_groups);
std::vector<CDecls> simplify_module(const simplifier_options&, FreshVarState&, Module&, const std::vector<CDecls>& decl_groups);

#endif
