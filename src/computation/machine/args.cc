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

Runtime::Exp OperationArgs::runtime_slot(int slot) const
{
    return current_closure().runtime_slot(slot);
}

optional<int> OperationArgs::runtime_reg_for_code(const Runtime::Exp& E) const
{
    if (auto reg_ref = E.to<Runtime::RegRef>())
        return reg_ref->target;
    else if (auto index_var = E.to<Runtime::IndexVar>())
        return lookup_in_env(current_closure().Env, index_var->index);
    else
        return {};
}

int OperationArgs::runtime_reg_for_slot(int slot) const
{
    return current_closure().runtime_reg_for_slot(slot);
}

int OperationArgs::n_args() const {return current_closure().runtime_n_slots();}

int OperationArgs::runtime_n_slots() const {return current_closure().runtime_n_slots();}

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
    return evaluate_code_to_closure(runtime_slot(slot));
}

closure OperationArgs::evaluate_slot_to_closure_(int slot)
{
    return evaluate_code_to_closure_(runtime_slot(slot));
}

closure OperationArgs::evaluate_code_to_closure(const Runtime::Exp& E)
{
    if (auto r = runtime_reg_for_code(E))
        return evaluate_reg_to_closure(*r);
    else
        return closure(E);
}

closure OperationArgs::evaluate_code_to_closure_(const Runtime::Exp& E)
{
    if (auto r = runtime_reg_for_code(E))
        return evaluate_reg_to_closure_(*r);
    else
        return closure(E);
}

optional<int> OperationArgs::evaluate_code_force(const Runtime::Exp& E)
{
    if (auto r = runtime_reg_for_code(E))
        return evaluate_reg_force(*r);
    else
        return {};
}

optional<int> OperationArgs::evaluate_code_use(const Runtime::Exp& E)
{
    if (auto r = runtime_reg_for_code(E))
        return evaluate_reg_use(*r);
    else
        return {};
}

int OperationArgs::evaluate_slot_force(int slot)
{
    return evaluate_reg_force(reg_for_slot(slot));
}

int OperationArgs::evaluate_slot_use(int slot)
{
    return evaluate_reg_use(reg_for_slot(slot));
}

Runtime::Exp OperationArgs::evaluate_slot_to_value(int slot)
{
    closure result = evaluate_slot_to_closure(slot);
#ifndef NDEBUG
    if (result.get_code().to<Runtime::Lambda>())
	throw myexception()<<"Evaluating lambda as object: "<<result.legacy_exp().print();
#endif
    return result.get_code();
}

Runtime::Exp OperationArgs::evaluate_slot_to_value_(int slot)
{
    closure result = evaluate_slot_to_closure_(slot);
#ifndef NDEBUG
    if (result.get_code().to<Runtime::Lambda>())
	throw myexception()<<"Evaluating lambda as object: "<<result.legacy_exp().print();
#endif
    return result.get_code();
}

expression_ref OperationArgs::evaluate_slot_to_legacy_object(int slot)
{
    closure result = evaluate_slot_to_closure(slot);
#ifndef NDEBUG
    if (result.get_code().to<Runtime::Lambda>())
	throw myexception()<<"Evaluating lambda as object: "<<result.legacy_exp().print();
#endif
    return result.legacy_exp();
}

expression_ref OperationArgs::evaluate_slot_to_legacy_object_(int slot)
{
    closure result = evaluate_slot_to_closure_(slot);
#ifndef NDEBUG
    if (result.get_code().to<Runtime::Lambda>())
	throw myexception()<<"Evaluating lambda as object: "<<result.legacy_exp().print();
#endif
    return result.legacy_exp();
}

expression_ref OperationArgs::evaluate(int slot)
{
    return evaluate_slot_to_legacy_object(slot);
}

expression_ref OperationArgs::evaluate_(int slot)
{
    return evaluate_slot_to_legacy_object_(slot);
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

expression_ref get_arg(std::vector<expression_ref>& args)
{
    auto arg = std::move(args.back());
    args.pop_back();
    return arg;
}

Runtime::Exp get_arg(std::vector<Runtime::Exp>& args)
{
    auto arg = std::move(args.back());
    args.pop_back();
    return arg;
}
