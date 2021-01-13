#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/machine/effects.H"
#include "computation/operations.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/expression/bool.H"
#include "computation/expression/index_var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"

using boost::dynamic_pointer_cast;

using std::optional;
using std::vector;

extern "C" closure builtin_function_maybe_modifiable_structure(OperationArgs& Args)
{
    Args.evaluate_slot_use(0);

    int R1 = Args.reg_for_slot(0);

    return Args.memory().maybe_modifiable_structure(R1);
}

extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    // We are supposed to evaluate the random_variable before we register

    // Force the raw_x so that we get a unique location for it.
    Args.evaluate_slot_force(0);

    int r_var = Args.current_closure().reg_for_slot(0);

    r_var = Args.memory().follow_index_var(r_var);

    auto pdf = Args.evaluate(1).as_log_double();

    auto effect = new register_prior(r_var, pdf);

    Args.set_effect(*effect);

    return effect;
}

extern "C" closure builtin_function_register_likelihood(OperationArgs& Args)
{
    // We are supposed to evaluate the likelihood before we register
    auto likelihood = Args.evaluate(0).as_log_double();

    int r_likelihood = Args.current_closure().reg_for_slot(0);

    r_likelihood = Args.memory().follow_index_var_no_force(r_likelihood);

    auto effect = new register_likelihood(r_likelihood, likelihood);

    Args.set_effect(*effect);

    return effect;
}

extern "C" closure builtin_function_modifiable(OperationArgs& Args)
{
    int r_value = Args.reg_for_slot(0);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( modifiable(),{index_var(0)} );

    return {mod_exp, {r_value}};
}
