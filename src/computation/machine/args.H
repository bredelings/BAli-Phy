#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <optional>
#include "computation/closure.H"
#include "computation/machine/graph_register.H"

// Describes edge ownership relative to the current OperationArgs boundary.
enum class EdgeContingency : unsigned char
{
    fixed = 0,
    contingent = 1
};

// Combine controllers; an edge is contingent if either controller can change.
constexpr EdgeContingency operator|(EdgeContingency c1, EdgeContingency c2)
{
    if (c1 == EdgeContingency::fixed and c2 == EdgeContingency::fixed)
        return EdgeContingency::fixed;
    else
        return EdgeContingency::contingent;
}

// Pairs a USE result with the contingency selected by its resolved dependency.
struct UseWithContingency
{
    int value_reg;
    EdgeContingency edge_contingency;
};

// Pairs an evaluated slot value with the contingency of its fixed USE.
struct ValueWithContingency
{
    Runtime::Exp value;
    EdgeContingency edge_contingency;
};


class OperationArgs
{
protected:
    reg_heap& M;

    int n_allocated = 0;

    const int r;

    std::optional<int> creator_step;

    virtual int evaluate_reg_contingent_force(int) = 0;

    virtual UseWithContingency
    evaluate_reg_contingent_use_with_contingency(int) = 0;

public:

    reg_heap& memory() {return M;}

    const reg_heap& memory() const {return M;}

    // This reg should be on the stack
    int current_reg() const { return r; }

    // If the heap is grown, this could end up pointing to bad memory.
    const closure& current_closure() const { return M.closure_at(current_reg()); }

    virtual bool evaluate_changeables() const = 0;

    virtual void make_changeable() = 0;

    int n_args() const;

    Runtime::Exp slot(int slot_index) const;

    const Runtime::Exp& slot_ref(int slot_index) const;

    std::optional<int> reg_for_code(const Runtime::Exp&) const;

    int reg_for_slot(int slot) const;

    // evaluating a changeable-or-forcing reg unchangeably will return the
    // reg instead of throwing a no_context() exception.
    int evaluate_reg_unchangeable(int r);

    int evaluate_slot_unchangeable(int slot);

    // evaluate_reg_*( ) and friends will throw no_context( )
    // when evaluating a modifiable or other changeable reg
    // in an unchangeable context
    virtual int evaluate_reg_force(int) = 0;

    // Route a FORCE to its owning reg or step according to its input contingency.
    int evaluate_reg_force(int r, EdgeContingency c)
    {
        if (c == EdgeContingency::fixed)
            return evaluate_reg_force(r);
        else
            return evaluate_reg_contingent_force(r);
    }

    std::optional<int> evaluate_code_force(const Runtime::Exp&);

    int evaluate_slot_force(int);

    virtual UseWithContingency evaluate_reg_use_with_contingency(int) = 0;

    // Perform a fixed USE while discarding its output contingency.
    int evaluate_reg_use(int r)
    {
        return evaluate_reg_use_with_contingency(r).value_reg;
    }

    // Route a USE by its input contingency and propagate contingency through it.
    UseWithContingency evaluate_reg_use_with_contingency(int r, EdgeContingency c1)
    {
        if (c1 == EdgeContingency::fixed)
            return evaluate_reg_use_with_contingency(r);
        else
        {
            auto [result,c2] = evaluate_reg_contingent_use_with_contingency(r);
            return {result, c1 | c2};
        }
    }

    // Perform a USE with explicit input contingency while discarding its output.
    int evaluate_reg_use(int r, EdgeContingency c)
    {
        return evaluate_reg_use_with_contingency(r,c).value_reg;
    }

    std::optional<int> evaluate_code_use(const Runtime::Exp&);
  
    int evaluate_slot_use(int);

    UseWithContingency evaluate_slot_use_with_contingency(int);

    const closure& evaluate_reg_to_closure(int r2);

    const closure& evaluate_reg_to_closure_(int r2);

    closure evaluate_code_to_closure(const Runtime::Exp&);

    closure evaluate_code_to_closure_(const Runtime::Exp&);

    Runtime::Exp evaluate_slot_to_value(int);

    ValueWithContingency evaluate_slot_to_value_with_contingency(int);

    Runtime::Exp evaluate_slot_to_value_(int);

    void stack_push(int);

    void stack_pop(int);

    void stack_pop();

    // Making this virtual allows the step to be passed by the derived class.
    virtual void set_effect(int r);

    int allocate(closure&&);

    int allocate_reg();

    int allocate_non_contingent(closure&&);

    int allocate_non_contingent_reg();

    OperationArgs(reg_heap& m, int r);

    virtual ~OperationArgs();
};

Runtime::Exp get_arg(std::vector<Runtime::Exp>& args);

#endif
