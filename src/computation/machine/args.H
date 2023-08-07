#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <optional>
#include "computation/object.H"
#include "computation/closure.H"
#include "computation/machine/graph_register.H"

// class expression_ref;
// class reg_heap;

class OperationArgs: public Object
{
protected:
    reg_heap& M;

    int n_allocated = 0;

    const int r;

    const int sp;

public:
    virtual OperationArgs* clone() const = 0;

    reg_heap& memory() {return M;}

    const reg_heap& memory() const {return M;}

    const closure& current_closure() const { return M.closure_at(r); }

    virtual bool evaluate_changeables() const = 0;

    virtual void make_changeable() = 0;

    int n_args() const;

    const expression_ref& reference(int slot) const;

    int reg_for_slot(int slot) const;

    // evaluating a changeable-or-forcing reg unchangeably will return the
    // reg instead of throwing a no_context() exception.
    int evaluate_reg_unchangeable(int r);

    int evaluate_slot_unchangeable(int slot);

    // evaluate_reg_*( ) and friends will throw no_context( )
    // when evaluating a modifiable or other changeable reg
    // in an unchangeable context
    virtual int evaluate_reg_force(int) = 0;

    int evaluate_slot_force(int);

    virtual int evaluate_reg_use(int) = 0;
  
    int evaluate_slot_use(int);

    virtual const closure& evaluate_reg_to_closure(int) = 0;

    virtual const closure& evaluate_reg_to_closure_(int) = 0;

    const closure& evaluate_slot_to_closure(int);

    const closure& evaluate_slot_to_closure_(int);

    const expression_ref& evaluate_reg_to_object(int);

    const expression_ref& evaluate_reg_to_object_(int);

    const expression_ref& evaluate_slot_to_object(int);

    const expression_ref& evaluate_slot_to_object_(int);

    const expression_ref& evaluate(int);

    const expression_ref& evaluate_(int);

    void stack_push(int);

    void stack_pop(int);

    void stack_pop();

    virtual void set_effect(int r);

    int allocate(closure&&);

    virtual int allocate_reg();

    OperationArgs(reg_heap& m, int r);

    virtual ~OperationArgs();
};

#endif
