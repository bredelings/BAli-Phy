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

int force_slot_to_safe_reg(OperationArgs& Args, int slot)
{
    // Force the slot so that we get a unique location for it.
    Args.evaluate_slot_force(slot);

    int r = Args.current_closure().reg_for_slot(slot);

    return Args.memory().follow_index_var(r);
}

extern "C" closure builtin_function_maybe_modifiable_structure(OperationArgs& Args)
{
    Args.evaluate_slot_use(0);

    int R1 = Args.reg_for_slot(0);

    return Args.memory().maybe_modifiable_structure(R1);
}

extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    // We are supposed to evaluate the random_variable before we register
    int r_from_dist = Args.evaluate_slot_use(0);

    auto pdf = Args.evaluate(1).as_log_double();

    auto effect = new register_prior(r_from_dist, pdf);

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

// Q. How do we ensure that each call is unique?
//    We don't have a state thread...

extern "C" closure builtin_function_register_in_edge(OperationArgs& Args)
{
    int r_from_var = force_slot_to_safe_reg(Args,0);
    int r_to_dist  = Args.evaluate_slot_use(1);
    std::string role = Args.evaluate(2).as_<String>();

    object_ptr<effect> e(new in_edge(r_from_var, r_to_dist, role));

    Args.set_effect(*e);

    return e;
}

extern "C" closure builtin_function_register_out_edge(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);
    int r_to_var    = force_slot_to_safe_reg(Args,1);

    object_ptr<effect> e(new out_edge(r_from_dist, r_to_var));

    Args.set_effect(*e);

    return e;
}

extern "C" closure builtin_function_register_dist(OperationArgs& Args)
{
    std::string name = Args.evaluate(0).as_<String>();

    auto& M = Args.memory();

    int r = Args.allocate_reg();

    object_ptr<effect> e(new register_dist(name, r));

    M.set_C(r, e);

    Args.set_effect(*e);

    return {index_var(0),{r}};
}

extern "C" closure builtin_function_register_dist_property(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);
    int r_to_prop   = force_slot_to_safe_reg(Args,1);
    std::string property = Args.evaluate(2).as_<String>();

    auto effect = new dist_property(r_from_dist, r_to_prop, property);

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
