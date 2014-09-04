#ifndef COMPUTATION_H
#define COMPUTATION_H

#include "object.H"
#include "closure.H"

struct expression_ref;
class reg_heap;

class OperationArgs: public Object
{
  reg_heap& M;

  int n_allocated = 0;

public:
  virtual OperationArgs* clone() const = 0;

  virtual int current_token() const = 0;

  reg_heap& memory() {return M;}

  const reg_heap& memory() const {return M;}

  virtual const closure& current_closure() const = 0;

  virtual bool evaluate_changeables() const = 0;

  int n_args() const;

  const expression_ref& reference(int slot) const;

  int reg_for_slot(int slot) const;

  virtual int evaluate_reg_no_record(int) = 0;

  int evaluate_slot_no_record(int);

  virtual int evaluate_reg_to_reg(int) = 0;
  
  int evaluate_slot_to_reg(int);

  const closure& evaluate_reg_to_closure(int);

  const closure& evaluate_reg_to_closure_(int);

  const closure& evaluate_slot_to_closure(int);

  const closure& evaluate_slot_to_closure_(int);

  const object_ptr<const Object>& evaluate_reg_to_object(int);

  const object_ptr<const Object>& evaluate_reg_to_object_(int);

  const object_ptr<const Object>& evaluate_slot_to_object(int);

  const object_ptr<const Object>& evaluate_slot_to_object_(int);

  const object_ptr<const Object>& evaluate(int);

  const object_ptr<const Object>& evaluate_(int);

  template <typename T>
  object_ptr<const T> evaluate_as(int slot)
  {
    return convert<const T>( evaluate(slot) );
  }

  template <typename T>
  object_ptr<const T> evaluate_as_(int slot)
  {
    return convert<const T>( evaluate_(slot) );
  }

  int allocate(closure&&);

  OperationArgs(reg_heap& m);

  virtual ~OperationArgs();
};

#endif
