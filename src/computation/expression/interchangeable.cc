#include "interchangeable.H"
#include "index_var.H"
#include "computation/runtime/ast.H"
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
    int n_args = C.runtime_n_slots();

    if (n_args == 2)
    {
	auto& M = Args.memory();

	// 1. Get the function and argument 
	int f = Args.reg_for_slot(0);
	int x = Args.reg_for_slot(1);

	// 2. Allocate a new reg i with closure (interchangeable)
        int i = Args.allocate( {R::App{R::OperationApp(std::make_shared<interchangeable>()),{}}} );

	// 3. Mark i changeable
	M.mark_reg_changeable(i);

	// 4. Mark i unforgettable
	M.mark_reg_unforgettable(i);

	// 5. Allocate step for i
	int s = M.add_shared_step(i);

	// 6. Create a reg with closure (f x)
	closure fx_C(R::apply(R::IndexVar(1), {R::IndexVar(0)}), {f,x});
	int fx = M.allocate_reg_from_step(s, std::move(fx_C));

	// 7. Give i a step that calls fx
	M.set_call(s, fx, true);

	// 8. Unchangeable evaluate to i
	return closure(R::IndexVar(0), {i});
    }
    else
	throw myexception()<<"Evaluating changeable with no call";
}

interchangeable::interchangeable():
    Operation(interchangeable_op, "interchangeable")
{}

bool is_interchangeable(const Runtime::Exp& E)
{
    const auto* app = E.to<Runtime::App>();
    if (not app)
        return false;

    const auto* op = std::get_if<Runtime::OperationApp>(&app->head);
    return op and dynamic_cast<const interchangeable*>(op->head.get());
}
