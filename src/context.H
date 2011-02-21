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
struct Context: public Object
{
  // member type
  struct value: public Object
  {
    value* clone() const {return new value(*this);}

    bool computed;

    boost::shared_ptr<const Computation> computation;
    boost::shared_ptr<const Object> result;

    value():computed(false) { }
  };

  // data members
  polymorphic_cow_ptr<Formula> F;

  std::vector< boost::shared_ptr<value> > values;

  // member functions
  Context* clone() const {return new Context(*this);}

  boost::shared_ptr<const Object> evaluate(int slot_to_eval);

  void set_value(int index, boost::shared_ptr<const Object> O);

  void set_value(int index, const Object& O);

  int size() const;

  bool index_may_affect_index(int, int) const;

  Context(const polymorphic_cow_ptr<Formula>&);
};

std::ostream& operator<<(std::ostream&, const Context& C);

#endif
