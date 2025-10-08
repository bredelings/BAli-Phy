#ifndef SIMPLIFIER_ENV_H
#define SIMPLIFIER_ENV_H

#include "computation/unfolding.H"
#include "computation/optimization/occurrence.H" // for Occ::Var

typedef std::pair<Unfolding,occurrence_info> bound_variable_info;

typedef immer::map<Occ::Var, bound_variable_info> in_scope_set;

[[nodiscard]] in_scope_set bind_var(const in_scope_set& bound_vars, const Occ::Var& x, const Unfolding& unfolding);

[[nodiscard]] in_scope_set rebind_var(in_scope_set bound_vars, const Occ::Var& x, const Unfolding& U);
#endif
