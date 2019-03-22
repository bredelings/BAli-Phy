#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/operations.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/expression/bool.H"
#include "computation/expression/index_var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/random_variable.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/list.H"
#include "computation/maybe_modifiable.H"

using boost::dynamic_pointer_cast;

using std::optional;
using std::vector;

bool is_seq(const expression_ref& E)
{
    bool result = E.head().type() == seq_type;
    assert(result == E.head().is_a<Seq>());
    return result;
}

bool is_join(const expression_ref& E)
{
    bool result = E.head().type() == join_type;
    assert(result == E.head().is_a<Join>());
    return result;
}

// Recursively walk through constant structures:It seems like we want a kind of deep_eval_translate_list, except that
// (i)   walk through constant structures, translating their fields.
// (ii)  translate modifiables -> modifiable + reg_var(r)
// (iii) translate other non-constant fields to reg_var(r)

expression_ref maybe_modifiable_structure(OperationArgs& Args, int r1)
{
    // FIXME - we might need to handle random variables when generating here, as we do in reg_head::find_update_modifiable_reg( ).

    // 0. We aren't going to record any uses or forces.  We just want to extract information.
    reg_heap& M = Args.memory();

    // 1. First evaluate the reg.  This will yield a non-index_var.
    auto [r2, value] = M.incremental_evaluate(r1);

    // 2. If this is a structure then translate the parts.
    if (M.reg_is_constant(r2))
    {
        // (i) The closure M[r2] can be moved, so a reference to it may become invalid.
        // (ii) Fields r can be updated to point through an index_var.

        // Can the r2 location be garbage-collected?
        // (a) The top-level one can't, because its referenced from
        //     (maybe_modifiable_structure r1), which is on the stack.
        //     How I think that r1 could be garbage-collected if it is an index-var.
        // (b) The r2 values of the children will then be referenced from the
        //     parent structure.
        //     Again the r1 values of the children might go away.

        // 2a. Atomic constants are already done.
        if (M[r2].exp.size() == 0)
            return M[r2].exp;

        // 2b. Constants with fields need their fields translated.
        vector<expression_ref> sub;
        for(int i=0; i< M[r2].exp.size(); i++)
            sub.push_back(maybe_modifiable_structure(Args, M[r2].reg_for_slot(i)));

        return expression_ref(M[r2].exp.head(), sub);
    }

    // r2 can only be constant or changeable, not unknown or index_var.
    assert(M.reg_is_changeable(r2));

    // 3. If this is a modifiable, stop there and return that.
    if (is_modifiable(M[r2].exp))
    {
        // We are going to encode the "modifiable" outcome this way.
        expression_ref m = modifiable();
        m = m + reg_var(r2);
        return m;
    }
    else if (is_random_variable(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(0);
        return maybe_modifiable_structure(Args,r3);
    }
    else if (is_seq(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(1);
        return maybe_modifiable_structure(Args,r3);
    }
    else if (is_join(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(1);
        return maybe_modifiable_structure(Args,r3);
    }
    // 4. Handle changeable computations
    return reg_var(r2);
}


extern "C" closure builtin_function_maybe_modifiable_structure(OperationArgs& Args)
{
    int R1 = Args.evaluate_slot_to_reg(0);

    return maybe_modifiable_structure(Args, R1);
}

extern "C" closure builtin_function_random_variable(OperationArgs& Args)
{
    int r_var     = Args.reg_for_slot(0);
    int r_pdf     = Args.reg_for_slot(1);
    int r_range   = Args.reg_for_slot(2);
    int r_c_range = Args.reg_for_slot(3);
    int r_rate    = Args.reg_for_slot(4);

    // Allocate a reg so that we get its address, and fill it with a modifiable of the correct index
    expression_ref E(random_variable(),{index_var(4), index_var(3), index_var(2), index_var(1), index_var(0)});

    return closure{E,{r_var, r_pdf, r_range, r_c_range, r_rate}};
}

extern "C" closure builtin_function_register_random_variable(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    int r_random_var = Args.current_closure().reg_for_slot(0);

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
