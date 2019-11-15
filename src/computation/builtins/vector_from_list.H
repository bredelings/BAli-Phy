#ifndef VECTOR_FROM_LIST_H
#define VECTOR_FROM_LIST_H
#include <vector>
#include <optional>
#include "computation/machine/args.H"
#include "computation/object.H"
#include "computation/expression/constructor.H"

inline std::optional<std::vector<int>> maybe_get_regs_for_list(OperationArgs& Args, int r)
{
    std::vector<int> regs;
    const closure* top = &Args.evaluate_reg_to_closure(r);

    // If this not a list, then return an empty optional
    if (not has_constructor(top->exp,":") and not has_constructor(top->exp,"[]"))
	return {};

    while(top->exp.size())
    {
	assert(has_constructor(top->exp,":"));
	assert(top->exp.size() == 2);

	int element_reg = top->reg_for_slot(0);

	int next_reg = top->reg_for_slot(1);

	// Add the element to the list.
	regs.push_back(element_reg);

	// Move to the next element or end
	top = &Args.evaluate_reg_to_closure(next_reg);
    }

    assert(has_constructor(top->exp,"[]"));
    return regs;
}

inline std::vector<int> get_regs_for_list(OperationArgs& Args, int r)
{
    auto regs = maybe_get_regs_for_list(Args, r);
    if (not regs)
    {
	const closure* top = &Args.evaluate_reg_to_closure(r);
	throw myexception()<<"get_regs_for_list: '"<<top->exp<<"' is not a list!";
    }
    return *regs;
}

#endif
