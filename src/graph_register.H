#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H
// a dummy variable expression

#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>
#include "object.H"
#include "expression.H"

struct reg
{
  // The expression
  expression_ref E;

  // The name of the variable
  std::string name;

  // Is this variable named?
  bool named;

  // Which expression is this a reduction of
  boost::shared_ptr<const reg> parent;

  // Which input values were used to reduce the parent to this expression
  std::vector<boost::shared_ptr<const reg> > used_inputs;

  // Which reduction results made use of the value of this expression
  std::vector<boost::shared_ptr<const reg> > outputs;

  // For each different context, what does this expression reduce to?
  std::vector<boost::shared_ptr<reg> > results;

  // Which parameter VALUES have been used in computing this redex?
  std::vector<int> used_parameters;

  bool is_valid() const {return E;}

  reg();
  reg(const std::string& s);
};


struct reg_var: public Object
{
  boost::shared_ptr< reg > target;

  reg_var* clone() const {return new reg_var(*this);}

  std::string print() const 
  {
    return "<" + target->name + ">";
  }

  tribool compare(const Object& o) const
  {
    const reg_var* E = dynamic_cast<const reg_var*>(&o);
    if (not E) 
      return false;

    return target == E->target;
  }

  const expression_ref& value() const {return target->E;}
        expression_ref& value()       {return target->E;}

  reg_var():
    target(new reg)
  { }

  reg_var(const std::string& s):
    target(new reg(s))
  { }

  reg_var(const boost::shared_ptr< reg >& r)
    :target(r)
  { }
};

struct context
{
  int token;

  // the list of expressions that we are interested in evaluating.
  std::vector<reg> heads;
  context(): token(-1) { }
};

#endif
