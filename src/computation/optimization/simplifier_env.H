#ifndef SIMPLIFIER_ENV_H
#define SIMPLIFIER_ENV_H

#include "computation/unfolding.H"
#include "computation/optimization/occurrence.H" // for Occ::Var

struct bound_variable_info
{
    Unfolding unfolding;
    occurrence_info occurrence;
    Core::id_info id;
};

typedef immer::map<Occ::Var, bound_variable_info> in_scope_set;

[[nodiscard]] in_scope_set bind_var(const in_scope_set& bound_vars, const Occ::Var& x, const Unfolding& unfolding);

[[nodiscard]] in_scope_set rebind_var(in_scope_set bound_vars, const Occ::Var& x, const Unfolding& U);

enum class arg_info
{
    trivial,
    non_trivial,
    value
};

#endif
