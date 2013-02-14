#ifndef COMPUTATION_H
#define COMPUTATION_H

#include "object.H"
#include "closure.H"

struct expression_ref;
class reg_heap;

struct OperationArgs: public Object
{
  virtual OperationArgs* clone() const = 0;

  virtual int current_token() const = 0;

  virtual reg_heap& memory() = 0;

  virtual const closure& current_closure() const = 0;

  int n_args() const;

  expression_ref reference(int slot) const;

  int reg_for_slot(int slot) const;

  virtual int evaluate_slot_(int) = 0;

  virtual int evaluate_reg_no_record(int) = 0;

  int evaluate_slot_no_record(int);

  virtual const closure& evaluate_reg_to_closure(int) = 0;

  const closure& evaluate_slot_to_closure(int);

  object_ref evaluate_reg_to_object(int);

  object_ref evaluate_slot_to_object(int);

  object_ref evaluate(int);

  template <typename T>
  object_ptr<const T> evaluate_as(int slot)
  {
    return convert<const T>( evaluate(slot) );
  }

  virtual int allocate(closure&&) 
  { 
    std::abort();
  }

  virtual ~OperationArgs() {}
};

#endif
