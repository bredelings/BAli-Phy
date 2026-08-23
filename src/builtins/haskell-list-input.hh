#ifndef BUILTINS_HASKELL_LIST_INPUT_HH
#define BUILTINS_HASKELL_LIST_INPUT_HH

#include <string_view>

#include "computation/machine/args.hh"
#include "util/myexception.hh"

// Walk a complete Haskell list while carrying the contingency that controls each cell.
// The callback receives the unevaluated head register and chooses whether to retain, USE, or FORCE it.
template<typename Visit>
void for_each_haskell_list_element(OperationArgs& args, UseWithContingency xs,
                                   std::string_view operation_name, Visit&& visit)
{
    while(true)
    {
        const closure& xs_closure = args.memory().closure_at(xs.value_reg);
        auto list_cell = xs_closure.get_code().to<Runtime::ConstructorApp>();
        if (not list_cell)
            throw myexception()<<operation_name<<": expected a list constructor, but got "
                               <<xs_closure.get_code().print();

        const auto& tag = list_cell->head;
        if (tag.name() == "[]" and tag.n_args() == 0 and list_cell->args.empty())
            return;
        if (tag.name() != ":" or tag.n_args() != 2 or list_cell->args.size() != 2)
            throw myexception()<<operation_name<<": expected ':' or '[]', but got "<<tag.print();

        int head_reg = xs_closure.reg_for_constructor_slot(0);
        int tail_reg = xs_closure.reg_for_constructor_slot(1);

        // Capture both registers before the callback evaluates anything, since evaluation may grow the heap
        // and invalidate xs_closure.  Register numbers themselves remain stable.
        visit(head_reg, xs.edge_contingency);
        xs = args.evaluate_reg_use_with_contingency(tail_reg, xs.edge_contingency);
    }
}

#endif
