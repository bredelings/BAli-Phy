#include "args.H"
#include "graph_register.H"
#include "effect.H"
#include "computation/expression/lambda.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/random_variable.H"

using std::optional;

int OperationArgs::reg_for_slot(int slot) const
{
    return current_closure().reg_for_slot(slot);
}

int OperationArgs::n_args() const {return current_closure().exp.size();}

const expression_ref& OperationArgs::reference(int slot) const
{
    assert(0 <= slot);
    assert(slot < current_closure().exp.sub().size());
    return current_closure().exp.sub()[slot];
}

const closure& OperationArgs::evaluate_slot_to_closure(int slot)
{
    return evaluate_reg_to_closure(reg_for_slot(slot));
}

const closure& OperationArgs::evaluate_slot_to_closure_(int slot)
{
    return evaluate_reg_to_closure_(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_force(int slot)
{
    return evaluate_reg_force(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_to_reg(int slot)
{
    return evaluate_reg_to_reg(reg_for_slot(slot));
}

const expression_ref& OperationArgs::evaluate_reg_to_object(int R2)
{
    const expression_ref& result = evaluate_reg_to_closure(R2).exp;
#ifndef NDEBUG
    if (result.head().is_a<lambda2>())
	throw myexception()<<"Evaluating lambda as object: "<<result.print();
#endif
    return result;
}

const expression_ref& OperationArgs::evaluate_reg_to_object_(int R2)
{
    const expression_ref& result = evaluate_reg_to_closure_(R2).exp;
#ifndef NDEBUG
    if (result.head().is_a<lambda2>())
	throw myexception()<<"Evaluating lambda as object: "<<result.print();
#endif
    return result;
}

const expression_ref& OperationArgs::evaluate_slot_to_object(int slot)
{
    return evaluate_reg_to_object(reg_for_slot(slot));
}

const expression_ref& OperationArgs::evaluate_slot_to_object_(int slot)
{
    return evaluate_reg_to_object_(reg_for_slot(slot));
}

const expression_ref& OperationArgs::evaluate(int slot)
{
    return evaluate_slot_to_object(slot);
}

const expression_ref& OperationArgs::evaluate_(int slot)
{
    return evaluate_slot_to_object_(slot);
}

int OperationArgs::evaluate_reg_unchangeable(int r)
{
    return M.incremental_evaluate_unchangeable(r);
}

int OperationArgs::evaluate_slot_unchangeable(int slot)
{
    return evaluate_reg_unchangeable(reg_for_slot(slot));
}

void OperationArgs::stack_push(int r)
{
    M.stack_push(r);
}

void OperationArgs::stack_pop(int r)
{
    M.stack_pop(r);
}

void OperationArgs::stack_pop()
{
    M.stack_pop();
}

int OperationArgs::allocate(closure&& C)
{
    int r = allocate_reg();
    M.set_C(r, std::move(C));
    return r;
}

int OperationArgs::allocate_reg()
{
    int r = M.push_temp_head();
    n_allocated++;
    return r;
}

void OperationArgs::set_effect(const effect& e)
{
    auto& M = memory();
    e.register_effect(M);
}

OperationArgs::OperationArgs(reg_heap& m)
    :M(m)
{ }

OperationArgs::~OperationArgs()
{
    for(int i=0;i<n_allocated;i++)
	M.pop_temp_head();
}

optional<int> OperationArgs::find_random_variable_in_root_token(int r)
{
    assert(evaluate_changeables());

    auto& M = memory();

    // Warning: ABOMINATION!
    // FIXME: This should be forced by a `seq` inside the program.
    // But that probably requires force-edges to be working.
    evaluate_reg(r);

    r = M.follow_index_var(r);

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

/* NOTE: We are currently using this to cheat and do *changeable* evaluation
         from MCMC that is being done *unchangeably*.
 */

optional<int> OperationArgs::find_modifiable_in_context(int r, int c)
{
    assert(not evaluate_changeables());

    auto& M = memory();

    return M.find_modifiable_reg_in_context(r,c);
}

