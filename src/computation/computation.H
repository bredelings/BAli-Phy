#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <vector>

#include "object.H"
#include "myexception.H"
#include "util.H"
#include "closure.H"

struct expression_ref;

struct OperationArgs: public Object
{
  virtual OperationArgs* clone() const = 0;

  virtual object_ref reference(int slot) const = 0;

  virtual object_ref evaluate(int slot) = 0;

  virtual int n_args() const = 0;

  virtual const closure& current_closure() const = 0;

  virtual closure lazy_evaluate(int)
  {
    std::abort();
  }

  template <typename T>
  object_ptr<const T> evaluate_as(int slot)
  {
    return convert<const T>( evaluate(slot) );
  }

  virtual int allocate(const closure&) 
  { 
    std::abort();
  }

  virtual object_ref evaluate_expression(const expression_ref&);

  virtual ~OperationArgs() {}
};

#endif
