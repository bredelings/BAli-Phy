#ifndef COMPUTATION_H
#define COMPUTATION_H

#include <vector>
#include <boost/shared_ptr.hpp>

#include "object.H"
#include "myexception.H"

/// Describes a completed computation
struct Computation: public Object
{
  Computation* clone() const {return new Computation(*this);}

  /// What were the values of arguments that were used?
  std::vector<boost::shared_ptr<const Object> > used_values;
  /// Which args/slots were used?  in what order? 
  std::vector<int> slots_used_order;

  Computation(int n_inputs):used_values(n_inputs) { }
};

class Context;

struct OperationArgs
{
  Context& CTX;
  boost::shared_ptr<Computation> computation;
  int index_of_caller;

  OperationArgs(Context& A, int i);

  boost::shared_ptr<const Object> evaluate(int slot);

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(int slot)
  {
    boost::shared_ptr<const Object> o = evaluate(slot);
    assert(o);
    boost::shared_ptr<const T> converted =  boost::dynamic_pointer_cast<const T>(o);
    if (not converted)
      throw myexception()<<"Cannot convert '"<<typeid(o.get()).name()<<" to "<<typeid(T).name();
    return converted;
  }

};

#endif
