#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <vector>
#include <boost/shared_ptr.hpp>

#include "object.H"
#include "myexception.H"
#include "util.H"

/// Describes a completed computation
struct Computation: virtual public Object
{
  Computation* clone() const {return new Computation(*this);}

  /// What were the values of arguments that were used?
  std::vector<boost::shared_ptr<const Object> > used_values;
  /// Which args/slots were used?  in what order? 
  std::vector<int> slots_used_order;

  Computation(int n_inputs):used_values(n_inputs) { }
};

struct expression_ref;

struct OperationArgs: virtual public Object
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

  virtual boost::shared_ptr<const Object> evaluate_expression(const expression_ref&);
};

class Context;

struct ContextOperationArgs: public OperationArgs
{
  const Context& CTX;
  boost::shared_ptr<Computation> computation;
  int index_of_caller;

  boost::shared_ptr<const Object> reference(int slot) const;

  boost::shared_ptr<const Object> evaluate(int slot);

  ContextOperationArgs* clone() const {return new ContextOperationArgs(*this);}

  ContextOperationArgs(const Context& A, int i);
};

#endif
