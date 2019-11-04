#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/machine/effects.H"
#include "computation/operations.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/expression/bool.H"
#include "computation/expression/index_var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/random_variable.H"
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
            sub.push_back(maybe_modifiable_structure(M, M[r2].reg_for_slot(i)));

        return expression_ref(M[r2].exp.head(), sub);
    }

    // r2 can only be constant or changeable, not unknown or index_var.
    assert(M.reg_is_changeable(r2));

    // 3. If this is a modifiable, stop there and return that.
    if (is_modifiable(M[r2].exp))
    {
        // We are going to encode the "modifiable" outcome this way.
        expression_ref m = constructor("Modifiable",1);
        m = m + reg_var(r2);
        return m;
    }
    // 4. If we see a random_variable guarding a modifiable, we want to claim to be modifiable, but reference the random variable.
    else if (is_random_variable(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(0);
        auto E = maybe_modifiable_structure(M,r3);
        if (is_modifiable(E))
        {
            expression_ref m = constructor("Modifiable",1);
            m = m + reg_var(r2);
            return m;
        }
        else
            return E;
    }
    else if (is_seq(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(1);
        return maybe_modifiable_structure(M,r3);
    }
    else if (is_join(M[r2].exp))
    {
	int r3 = M[r2].reg_for_slot(1);
        return maybe_modifiable_structure(M,r3);
    }
    else if (M.reg_has_call(r2))
    {
        int r3 = M.call_for_reg(r2);
        return maybe_modifiable_structure(M,r3);
    }

    // 4. Handle changeable computations with no call
    return reg_var(r2);
}

optional<int> find_random_variable_in_root_token(reg_heap& M, int r)
{
    // Warning: ABOMINATION!
    // FIXME: This should be forced by a `seq` inside the program.
    r = M.incremental_evaluate(r).first;

    // r should not be unknown or an index_var
    assert(M.reg_is_constant(r) or (M.reg_is_changeable(r) and M.reg_has_call(r)));

    while (not M.reg_is_constant(r))
    {
        assert(M.reg_is_changeable(r));
        assert(M.reg_has_call(r));

        if (is_random_variable(M[r].exp))
            return r;
        else if (is_modifiable(M[r].exp))
            return {};
        else
            r = M.call_for_reg(r);
    };

    // r is (now) a constant.
    // There is therefore no modifiable.
    return {};
}


extern "C" closure builtin_function_maybe_modifiable_structure(OperationArgs& Args)
{
    Args.evaluate_slot_to_reg(0);

    int R1 = Args.reg_for_slot(0);

    return maybe_modifiable_structure(Args.memory(), R1);
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
    int r_random_var = Args.current_closure().reg_for_slot(0);

    // We are supposed to evaluate the random_variable before we register
    Args.evaluate_reg_force(r_random_var);

    auto& M = Args.memory();

    if (auto r = find_random_variable_in_root_token(M, r_random_var))
        r_random_var = *r;
    else
	throw myexception()<<"Trying to register `"<<M.expression_at(r_random_var)<<"` as random variable";

//    Currently the pdf is forced when evaluating (random_variable x pdf) => x
//    int r_pdf = M[r_random_var].reg_for_slot(1);
//    Args.evaluate_reg_force(r_pdf);

    int r_effect = Args.allocate(new register_random_variable(r_random_var));

    Args.set_effect(r_effect);

    // Return a reference to the effect.
    return {index_var(0),{r_effect}};
}

extern "C" closure builtin_function_modifiable(OperationArgs& Args)
{
    int r_value = Args.reg_for_slot(0);

    // Allocate a reg, and fill it with a modifiable of the correct index
    expression_ref mod_exp( modifiable(),{index_var(0)} );

    return {mod_exp, {r_value}};
}

extern "C" closure builtin_function_register_likelihood(OperationArgs& Args)
{
    int R = Args.reg_for_slot(0);

    int state = Args.evaluate(1).as_int();

    // We are suppose to evaluate the likelihood before we register.
    auto result_reg = Args.evaluate_slot_force(0);

    auto& M = Args.memory();
    auto likelihood = M[result_reg].exp;
    assert(likelihood.is_log_double());

    M.register_likelihood_(R);

    return {EPair(state+1, constructor("()",0))};
}
