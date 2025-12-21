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
#include "computation/expression/interchangeable.H"
#include "computation/expression/list.H"

using boost::dynamic_pointer_cast;

using std::optional;
using std::vector;

int force_slot_to_safe_reg(OperationArgs& Args, int slot)
{
    // Force the slot so that we get a unique location for it.
    Args.evaluate_slot_force(slot);

    int r = Args.reg_for_slot(slot);

    return Args.memory().follow_index_var(r);
}

extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    // We are supposed to evaluate the random_variable before we register
    int r_from_dist = Args.evaluate_slot_use(0);

    auto prob = Args.evaluate(1).as_log_double();

    int r_prob = Args.reg_for_slot(1);

    r_prob = Args.memory().follow_index_var_no_force(r_prob);

    object_ptr<effect> e(new register_prior(r_from_dist, r_prob, prob));

    int r_effect = Args.allocate(closure(e));

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

extern "C" closure builtin_function_register_likelihood(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);

    // We are supposed to evaluate the likelihood before we register
    auto prob = Args.evaluate(1).as_log_double();

    int r_prob = Args.reg_for_slot(1);

    r_prob = Args.memory().follow_index_var_no_force(r_prob);

    object_ptr<effect> e(new register_likelihood(r_from_dist, r_prob, prob));

    int r_effect = Args.allocate(closure(e));

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

// Q. How do we ensure that each call is unique?
//    We don't have a state thread...

extern "C" closure builtin_function_register_in_edge(OperationArgs& Args)
{
    int r_from_var = Args.reg_for_slot(0);
    int r_to_dist  = Args.evaluate_slot_use(1);
    String role = Args.evaluate(2).as_<String>();

    expression_ref E(constructor("Effect.InEdge",3),{index_var(0), r_to_dist, role});

    int r_effect = Args.allocate(closure{E,{r_from_var}});

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

extern "C" closure builtin_function_register_out_edge(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);
    int r_to_var    = force_slot_to_safe_reg(Args,1);

    expression_ref E(constructor("Effect.OutEdge",2), {index_var(1), index_var(0)});

    int r_effect = Args.allocate(closure{E,{r_from_dist, r_to_var}});

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

extern "C" closure builtin_function_register_dist(OperationArgs& Args)
{
    String name = Args.evaluate(0).as_<String>();

    int observation = Args.evaluate(1).as_int();

    // The effect to register the sampling event is self-referential,
    // so we need to allocate the location BEFORE we construct the object.

    int r_effect = Args.allocate_reg();

    expression_ref E(constructor("Effect.Dist",3), {index_var(0), observation, name});

    auto& M = Args.memory();
    M.set_C(r_effect, closure{E,{r_effect}});

    Args.set_effect(r_effect);

    return {index_var(0),{r_effect}};
}

extern "C" closure builtin_function_register_dist_property(OperationArgs& Args)
{
    int r_from_dist = Args.evaluate_slot_use(0);
    int r_to_prop = Args.reg_for_slot(1);
    String property = Args.evaluate(2).as_<String>();

    expression_ref E(constructor("Effect.DistProperty",3), {r_from_dist, property, index_var(0)});

    int r_effect = Args.allocate(closure{E,{r_to_prop}});

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

extern "C" closure builtin_function_getProperties(OperationArgs& Args)
{
    auto& M = Args.memory();

    // 1. Get the reg for the observed/sampled variable

    // NOTE: If we are getting properties for e.g. (data !! 2), then we might need to perform
    //       some computation to find the reg that is annotated as being observed.

    // QUESTION: If this is a SAMPLED (not observed) variable, will this find the modifiable or the value?
    //           We want the modifiable.

    int r_var = Args.evaluate_slot_force(0);

    // 2. Find the distribution from which it was sampled / observed
    auto it1 = M.out_edges_to_var.find(r_var);
    if (it1 == M.out_edges_to_var.end())
        throw myexception()<<"This reg is not a random variable!";
    auto& to_var = it1->second;
    if (to_var.size() > 1)
        throw myexception()<<"This data has been observed more than once!";

    int s_dist = *to_var.begin();

    // 3. Get the properties from the distribution
    auto& dist_properties = M.dist_properties;

    auto it = dist_properties.find(s_dist);
    if (it == dist_properties.end())
        throw myexception()<<"No properties for distribution!";

    // 4. Get the "properties" property.
    int r_properties_effect = it->second.at("properties");
    int r_properties = M.closure_at(r_properties_effect).Env[0];

    return {index_var(0), {r_properties}};
}

extern "C" closure builtin_function_modifiable(OperationArgs& Args)
{
    int r_value = Args.reg_for_slot(0);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( modifiable(),{index_var(0)} );

    return {mod_exp, {r_value}};
}


extern "C" closure builtin_function_changeable_apply(OperationArgs& Args)
{
    Args.make_changeable();

    int f_reg = Args.reg_for_slot(0);
    int x_reg = Args.reg_for_slot(1);

    expression_ref apply = {index_var(1), index_var(0)};
    return {apply, {f_reg, x_reg}};
}


extern "C" closure builtin_function_modifiable_apply(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);
    int x_reg = Args.reg_for_slot(1);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( modifiable(),{index_var(1),index_var(0)} );

    return {mod_exp, {f_reg, x_reg}};

}


extern "C" closure builtin_function_interchangeable(OperationArgs& Args)
{
    int f_reg = Args.reg_for_slot(0);
    int x_reg = Args.reg_for_slot(1);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( interchangeable(),{index_var(1), index_var(0)} );

    return {mod_exp, {f_reg, x_reg}};
}


// See: builtins/Prelude.cc - builtin_function_seq( )
// See: computation/operations.{H,cc} - struct Seq, seq_op(OperationArgs&)
extern "C" closure builtin_function_withEffect(OperationArgs& Args)
{
    Args.evaluate_slot_force(0);

    int R = Args.reg_for_slot(1);

    return {index_var(0),{R}};
}
