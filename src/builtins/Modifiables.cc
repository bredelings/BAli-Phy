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

// Recursively walk through constant structures:It seems like we want a kind of deep_eval_translate_list, except that
// (i)   walk through constant structures, translating their fields.
// (ii)  translate modifiables -> modifiable + reg_var(r)
// (iii) translate other non-constant fields to reg_var(r)

expression_ref maybe_modifiable_structure(reg_heap& M, int r1)
{
    // FIXME - we might need to handle random variables when generating here, as we do in reg_head::find_update_modifiable_reg( ).

    // 1. First evaluate the reg.  This will yield a non-index_var.
    int r2 = M.incremental_evaluate_unchangeable(r1);

    // 2. If this is a structure then translate the parts.
    if (M.reg_is_constant_no_force(r2) or M.reg_is_constant_with_force(r2))
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
            sub.push_back(maybe_modifiable_structure(M, M[r2].reg_for_slot(i)));

        return expression_ref(M[r2].exp.head(), sub);
    }

    // We can actually get unevaluated seq ops here.
    assert(M.reg_is_changeable_or_forcing(r2) or M.reg_is_unevaluated(r2));

    // 3. If this is a modifiable, stop there and return that.
    if (is_modifiable(M[r2].exp))
    {
        // We are going to encode the "modifiable" outcome this way.
        expression_ref m = constructor("Modifiable",1);
        m = m + reg_var(r2);
        return m;
    }
    else if (is_seq(M[r2].exp))
    {
        int r3 = M[r2].reg_for_slot(1);
        return maybe_modifiable_structure(M,r3);
    }
    else if (M.reg_is_index_var_with_force_to_changeable(r2) or
             M.reg_is_index_var_with_force_to_nonchangeable(r2))
    {
        int r3 = M[r2].reg_for_index_var();
        return maybe_modifiable_structure(M,r3);
    }
    else if (M.reg_is_changeable(r2) and M.reg_has_call(r2))
    {
        int r3 = M.call_for_reg(r2);
        return maybe_modifiable_structure(M,r3);
    }
    assert(M.reg_is_changeable(r2));

    // 4. Handle changeable computations with no call
    return reg_var(r2);
}

extern "C" closure builtin_function_maybe_modifiable_structure(OperationArgs& Args)
{
    Args.evaluate_slot_to_reg(0);

    int R1 = Args.reg_for_slot(0);

    return maybe_modifiable_structure(Args.memory(), R1);
}

extern "C" closure builtin_function_register_prior(OperationArgs& Args)
{
    // We are supposed to evaluate the random_variable before we register

    // Force the raw_x so that we get a unique location for it.
    Args.evaluate_slot_force(0);

    int r_var = Args.current_closure().reg_for_slot(0);

    r_var = Args.memory().follow_index_var(r_var);

    auto pdf = Args.evaluate(1).as_log_double();

    auto effect = new register_random_variable(r_var, pdf);

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
