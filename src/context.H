#ifndef CONTEXT_H
#define CONTEXT_H

#include <vector>
#include <iostream>
#include <boost/shared_ptr.hpp>
#include "cow-ptr.H"
#include "object.H"

class Formula;
class Computation;

// Values
struct Context: virtual public Object
{
  // member type
  struct value: virtual public Object
  {
    value* clone() const {return new value(*this);}

    /// Is the value both AVAILABLE and KNOWN to be correct.
    bool computed;

    /// What computation was run?  Which args/slots were used?  In what order?  What were their values?
    boost::shared_ptr<const Computation> computation;

    /// What was the result of the computation?
    boost::shared_ptr<const Object> result;

    value():computed(false) { }
  };

  // data members

  /// Structure that specifies how to compute each index from other indices (context-independent)
  polymorphic_cow_ptr<Formula> F;

  /// The current state of this particular context
  std::vector< boost::shared_ptr<value> > values;

  // member functions

  Context* clone() const {return new Context(*this);}

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate(int index);

  /// Update the value of a non-constant, non-computed index
  void set_value(int index, boost::shared_ptr<const Object> O);

  /// Update the value of a non-constant, non-computed index
  void set_value(int index, const Object& O);

  /// Update the value of a non-constant, non-computed index
  void set_value(const std::string& var, boost::shared_ptr<const Object> O);

  /// Update the value of a non-constant, non-computed index
  void set_value(const std::string& var, const Object& O);

  /// How many indices total do we have?
  int size() const;

  /// Is index1 possibly used in the computation of index2?
  bool index_may_affect_index(int index1, int index2) const;

  Context(const polymorphic_cow_ptr<Formula>&);
};

std::ostream& operator<<(std::ostream&, const Context& C);

#endif
