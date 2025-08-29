#include "interchangeable.H"
#include "index_var.H"
#include "util/assert.hh"

/*
 * NOTE: If the interchangeable op is not unforgettable, then moving steps between regs
 *       might change how far the step sI for the interchangeable is backward shared.
 *
 *       This can create a situation where some of the steps created by sI are shared
 *       further back than sI is, so that sI is destroyed, but they are not.
 *
 *       This can then create situations where some of the non-destroyed steps create
 *       closures that point to destroyed closures.
 */

/*
 * NOTE: Should the interchangeable discard its references to f and x?
 *
 *       These references are no longer needed, so we discard them here.
 *
 *       Is it a problem to hold on to them?  That is less clear.
 *
 *       Evaluating (modifiable x) does discard the reference to x, but
 *         it is clear that holding on to x there prevents the initial value
 *         from ever being discarded.
 *
 *       It is less clear here.
 */

closure interchangeable_op(OperationArgs& Args)
{
    auto& C = Args.current_closure();

    if (C.exp.size() == 2)
    {
	auto& M = Args.memory();

	// 1. Get the function and argument 
	int f = Args.reg_for_slot(0);
	int x = Args.reg_for_slot(1);

	// 2. Allocate a new reg i with closure (interchangeable)
	int i = Args.allocate( {interchangeable(), {}} );

	// 3. Mark i changeable
	M.mark_reg_changeable(i);

	// 4. Mark i unforgettable
	M.mark_reg_unforgettable(i);

	// 5. Allocate step for i
	int s = M.add_shared_step(i);

	// 6. Create a reg with closure (f x)
	expression_ref f_E = index_var(1);
	expression_ref x_E = index_var(0);
	expression_ref fx_E = {f_E, x_E};
	closure fx_C(fx_E, {f,x});
	int fx = M.allocate_reg_from_step(s, std::move(fx_C));

	// 7. Give i a step that calls fx
	M.set_call(s, fx, true);

	// 8. Unchangeable evaluate to i
	return {index_var(0), {i}};
    }
    else
	throw myexception()<<"Evaluating changeable with no call";
}

interchangeable::interchangeable():
    Operation(interchangeable_op, "interchangeable")
{}

bool is_interchangeable(const expression_ref& E)
{
    bool result = E.head().type() == type_constant::interchangeable_type;
    assert(result == E.head().is_a<interchangeable>());
    return result;
}

