#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/operations.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/expression/bool.H"
#include "computation/expression/index_var.H"
#include "computation/expression/random_variable.H"
#include "computation/expression/modifiable.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_random_variable(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    int r_var     = Args.reg_for_slot(0);
    int r_pdf     = Args.reg_for_slot(1);
    int r_range   = Args.reg_for_slot(2);
    int r_c_range = Args.reg_for_slot(3);
    int r_rate    = Args.reg_for_slot(4);

    // Allocate a reg so that we get its address, and fill it with a modifiable of the correct index
    expression_ref E(random_variable(),{index_var(4), index_var(3), index_var(2), index_var(1), index_var(0)});
    closure C{E,{r_var, r_pdf, r_range, r_c_range, r_rate}};
    int r_random_var = Args.allocate(std::move(C));

    M.register_random_variable(r_random_var);

    // Return a reference to the new modifiable.
    return {index_var(0),{r_random_var}};
}

extern "C" closure builtin_function_modifiable(OperationArgs& Args)
{
    int r_value = Args.reg_for_slot(0);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( modifiable(),{index_var(0)} );

    return {mod_exp, {r_value}};
}

extern "C" closure builtin_function_is_changeable(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int R1 = Args.evaluate_slot_to_reg(0);

    const reg_heap& M = Args.memory();
    if (M.reg_is_changeable(R1))
	return bool_true;
    else
	return bool_false;
}

extern "C" closure builtin_function_is_modifiable(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int R1 = Args.evaluate_slot_to_reg(0);

    const reg_heap& M = Args.memory();

    if (is_modifiable(M[R1].exp))
	return bool_true;
    else
	return bool_false;
}

extern "C" closure builtin_function_get_modifiable_for_index(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int R1 = Args.evaluate(0).as_int();

    return {index_var(0),{R1}};
}

extern "C" closure builtin_function_get_modifiable_index(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int R1 = Args.evaluate_slot_to_reg(0);

    const reg_heap& M = Args.memory();

    assert(is_modifiable(M[R1].exp));

    return {R1};
}

extern "C" closure builtin_function_set_modifiable_value(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int c = Args.evaluate(0).as_int();

    int R1 = Args.evaluate_slot_to_reg(1);
    int R2 = Args.evaluate_slot_to_reg(2);

    Args.memory().set_reg_value_in_context(R1, {index_var(0),{R2}}, c);

    return constructor("()",0);
}

extern "C" closure builtin_function_get_modifiable_value(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    int c = Args.evaluate(0).as_int();

    int R1 = Args.evaluate_slot_to_reg(1);

    int R2 = Args.memory().get_modifiable_value_in_context(R1, c);

    assert( R2 );

    return {index_var(0),{R2}};
}

extern "C" closure builtin_function_add_parameter(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    const std::string name = Args.evaluate(0).as_<String>();

    int R = Args.evaluate_slot_to_reg(1);

    auto& M = Args.memory();

    M.add_parameter(name, R);

    return constructor("()",0);
}

extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    int R = Args.reg_for_slot(0);

    auto& M = Args.memory();

    M.register_prior(R);

    return constructor("()",0);
}

extern "C" closure builtin_function_register_likelihood(OperationArgs& Args)
{
    int R = Args.reg_for_slot(0);

    auto& M = Args.memory();

    M.register_likelihood(R);

    return constructor("()",0);
}

extern "C" closure builtin_function_evaluate(OperationArgs& Args)
{
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();

#ifndef NDEBUG
    if (Args.evaluate_changeables() and c >= 0)
	throw myexception()<<"Calling builtin_function_evaluate( ) when evaluate_changeables=true and c >= 0";
#endif

    int R1 = Args.reg_for_slot(1);

    int R2 = 0;

    if (c < 0)
	R2 = M.incremental_evaluate_unchangeable(R1);
    else
	R2 = M.incremental_evaluate_in_context(R1, c).first;

    assert( R2 );

    return {index_var(0),{R2}};
}
