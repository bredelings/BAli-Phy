#ifndef ARITY_H
#define ARITY_H

#include <functional>
#include <vector>

#include "computation/fresh_vars.H"
#include "computation/optimization/occurrence_info.H"

enum class arity_cost
{
    cheap,
    expensive,
};

struct arity_lambda
{
    arity_cost cost = arity_cost::cheap;
    Core::one_shot_info one_shot = Core::one_shot_info::unknown;
};

struct arity_type
{
    std::vector<arity_lambda> lambdas;

    int arity() const { return lambdas.size(); }
};

using id_info_lookup = std::function<Core::id_info(const Occ::Var&)>;

// Infer the safe arity of a non-recursive RHS, including transient call arity.
arity_type find_rhs_arity(const Occ::Exp& rhs, int call_arity, const id_info_lookup& lookup);

// Count lambdas already visible at the head of an expression.
int manifest_arity(const Occ::Exp& expression);

// Add only the missing lambdas justified by a safe RHS arity.
Occ::Exp eta_expand(FreshVarSource& fresh, Occ::Exp expression, const arity_type& arity);

// Report whether resolved id arities prove that evaluating an expression performs no work.
bool is_work_free(const Occ::Exp& expression, const id_info_lookup& lookup);

#endif
