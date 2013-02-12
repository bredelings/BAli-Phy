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

  virtual int n_args() const {return current_closure().exp->sub.size();}

  virtual expression_ref reference(int slot) const {return current_closure().exp->sub[slot];}

  virtual int evaluate_slot_(int) = 0;

  virtual int evaluate_slot_no_record(int) = 0;

  virtual const closure& lazy_evaluate(int) = 0;

  virtual const closure& lazy_evaluate_reg_closure(int) = 0;

  virtual const object_ref evaluate_reg(int) = 0;

  virtual object_ref evaluate(int slot) = 0;

  template <typename T>
  object_ptr<const T> evaluate_as(int slot)
  {
    return convert<const T>( evaluate(slot) );
  }

  virtual int allocate(closure&&) 
  { 
    std::abort();
  }

  virtual object_ref evaluate_expression(const expression_ref&);

  virtual ~OperationArgs() {}
};

#endif
