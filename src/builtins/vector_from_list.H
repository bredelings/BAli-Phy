#ifndef VECTOR_FROM_LIST_H
#define VECTOR_FROM_LIST_H
#include <vector>
#include "computation/computation.H"
#include "object.H"

template<typename U, typename V>
std::vector<U> get_vector_from_list(OperationArgs& Args, int slot)
{
  std::vector<U> z;
  const closure* top = &Args.evaluate_slot_to_closure(slot);
  while(top->exp.size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_index = as_<index_var>(top->exp.sub()[0]).index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = as_<index_var>(top->exp.sub()[1]).index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    z.push_back( *convert<const V>(Args.evaluate_reg_to_object(element_reg)) );

    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));
  return z;
}

template<typename U, typename V>
std::vector<U> get_vector_from_list_(OperationArgs& Args, int slot)
{
  std::vector<U> z;
  const closure* top = &Args.evaluate_slot_to_closure_(slot);
  while(top->exp.size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_index = as_<index_var>(top->exp.sub()[0]).index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = as_<index_var>(top->exp.sub()[1]).index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    z.push_back( *convert<const V>(Args.evaluate_reg_to_object_(element_reg)) );

    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure_(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));
  return z;
}

#endif
