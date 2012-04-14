#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <vector>
#include <boost/shared_ptr.hpp>

#include "object.H"
#include "myexception.H"
#include "util.H"

struct expression_ref;

struct OperationArgs: public Object
{
  virtual OperationArgs* clone() const = 0;

  virtual boost::shared_ptr<const Object> reference(int slot) const = 0;

  virtual boost::shared_ptr<const Object> evaluate(int slot) = 0;

  virtual boost::shared_ptr<const Object> lazy_evaluate(int)
  {
    std::abort();
  }

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(int slot)
  {
    return convert<const T>( evaluate(slot) );
  }

  virtual int allocate(const expression_ref&) 
  { 
    std::abort();
  }

  virtual boost::shared_ptr<const Object> evaluate_expression(const expression_ref&);

  virtual ~OperationArgs() {}
};

#endif
