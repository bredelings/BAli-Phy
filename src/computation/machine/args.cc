#include "args.H"
#include "graph_register.H"
#include "effect.H"
#include "computation/expression/lambda.H"
#include "computation/expression/modifiable.H"

using std::optional;

expression_ref OperationArgs::arg_for_slot(int slot) const
{
    return current_closure().arg_for_slot(slot);
}

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

const closure& OperationArgs::evaluate_reg_to_closure(int r2)
{
    int r3 = evaluate_reg_use(r2);
    assert(evaluate_changeables() or M.reg_is_constant(r3));
    return M[r3];
}

const closure& OperationArgs::evaluate_reg_to_closure_(int r2)
{
    int r3 = evaluate_reg_force(r2);
    assert(evaluate_changeables() or M.reg_is_constant(r3));
    return M[r3];
}

closure OperationArgs::evaluate_slot_to_closure(int slot)
{
    auto S = arg_for_slot(slot);
    if (S.is_reg_var())
        return evaluate_reg_to_closure(S.as_reg_var());
    else
        return S;
}

closure OperationArgs::evaluate_slot_to_closure_(int slot)
{
    auto S = arg_for_slot(slot);
    if (S.is_reg_var())
        return evaluate_reg_to_closure_(S.as_reg_var());
    else
        return S;
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

expression_ref OperationArgs::evaluate_slot_to_object(int slot)
{
    auto S = arg_for_slot(slot);
    if (S.is_reg_var())
        return evaluate_reg_to_object(S.as_reg_var());
    else
        return S;
}

expression_ref OperationArgs::evaluate_slot_to_object_(int slot)
{
    auto S = arg_for_slot(slot);
    if (S.is_reg_var())
        return evaluate_reg_to_object_(S.as_reg_var());
    else
        return S;
}

expression_ref OperationArgs::evaluate(int slot)
{
    return evaluate_slot_to_object(slot);
}

expression_ref OperationArgs::evaluate_(int slot)
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
    if (creator_step)
	M.mark_reg_created_by_step(r, *creator_step);
    return r;
}

int OperationArgs::allocate_non_contingent(closure&& C)
{
    int r = allocate_non_contingent_reg();
    M.set_C(r, std::move(C));
    return r;
}

int OperationArgs::allocate_non_contingent_reg()
{
    n_allocated++;
    return M.push_temp_head();
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
