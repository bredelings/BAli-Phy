#include "args.H"
#include "graph_register.H"
#include "effect.H"
#include "computation/expression/lambda.H"
#include "computation/expression/modifiable.H"

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

int OperationArgs::evaluate_slot_use(int slot)
{
    return evaluate_reg_use(reg_for_slot(slot));
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
    n_allocated++;
    int r = M.push_temp_head();
    if (creator_step > 0)
	M.mark_reg_created_by_step(r, creator_step);
    return r;
}

void OperationArgs::set_effect(int /*r*/)
{
    make_changeable();
}

OperationArgs::OperationArgs(reg_heap& m, int r_)
    :M(m), r(r_), creator_step(m.creator_step_for_reg(r))
{ }

OperationArgs::~OperationArgs()
{
    for(int i=0;i<n_allocated;i++)
	M.pop_temp_head();
}
