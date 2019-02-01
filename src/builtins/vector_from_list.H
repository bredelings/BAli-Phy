#ifndef VECTOR_FROM_LIST_H
#define VECTOR_FROM_LIST_H
#include <vector>
#include "computation/machine/args.H"
#include "util/object.H"
#include "computation/expression/constructor.H"

inline EVector get_vector_from_list(OperationArgs& Args, int slot)
{
  EVector z;
  const closure* top = &Args.evaluate_slot_to_closure(slot);
  while(top->exp.size())
  {
    assert(has_constructor(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_reg = top->reg_for_slot(0);

    int next_reg = top->reg_for_slot(1);

    // Add the element to the list.
    z.push_back( Args.evaluate_reg_to_object(element_reg) );

    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(has_constructor(top->exp,"[]"));
  return z;
}

inline EVector get_vector_from_list_(OperationArgs& Args, int slot)
{
  EVector z;
  const closure* top = &Args.evaluate_slot_to_closure_(slot);
  while(top->exp.size())
  {
    assert(has_constructor(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_reg = top->reg_for_slot(0);

    int next_reg = top->reg_for_slot(1);

    // Add the element to the list.
    z.push_back( Args.evaluate_reg_to_object_(element_reg) );

    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure_(next_reg);
  }
  assert(has_constructor(top->exp,"[]"));
  return z;
}

#endif
