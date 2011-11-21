#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H
// a dummy variable expression

#include <string>
#include <vector>
#include <boost/shared_ptr.hpp>
#include <set>
#include "object.H"
#include "expression.H"
#include "util.H"
#include <list>

struct reg
{
  // The expression
  expression_ref E;

  // The name of the variable
  std::string name;

  // Is this variable named?
  bool named;

  // Is this a parameter value, or dependent on a parameter value?
  bool changeable;

  // Which input values were used to reduce this expression
  std::vector<boost::shared_ptr<reg> > used_inputs;

  // Which reduction results made use of the value of this expression (via an operation)
  std::set<boost::shared_ptr<reg> > outputs;

  // Which reduction results made use of the value of this expression (via call)
  std::set<boost::shared_ptr<reg> > call_outputs;

  // Does this expression reduce to another expression that we need to evaluate?
  boost::shared_ptr<reg> call;

  // What does this expression reduce to eventually? (WHNF)
  boost::shared_ptr< boost::shared_ptr<const Object> > result;

  // Which parameter VALUES have been used in computing the call/result
  std::vector<int> used_parameters;

  bool is_valid() const {return E;}

  reg* prev_reg;
  reg* next_reg;

  reg();
  reg(const std::string& s);
  reg(const expression_ref&);

  ~reg();
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

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?


struct context: virtual public Object
{
  mutable boost::shared_ptr<reg> first_reg;
  mutable boost::shared_ptr<reg> last_reg;

  std::vector<std::string> parameter_names;

  std::vector<boost::shared_ptr<reg> > parameters;

  context* clone() const {return new context(*this);}

  // the list of expressions that we are interested in evaluating.
  mutable std::vector<boost::shared_ptr<reg> > heads;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate(int index) const;

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(int index) const
  {
    boost::shared_ptr<const Object> o = evaluate(index);
    assert(o);
    boost::shared_ptr<const T> converted =  boost::dynamic_pointer_cast<const T>(o);
    if (not converted)
      throw myexception()<<"Cannot convert '"<<o->print()<<"' from type "<<demangle(typeid(*o.get()).name())<<" to type "<<demangle(typeid(T).name());
    return converted;
  }

  /// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
  boost::shared_ptr<const Object> get_parameter_value(int index) const;

  /// Get the value of a non-constant, non-computed index
  boost::shared_ptr<const Object> get_parameter_value(const std::string&) const;

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(int index, const expression_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(const std::string& var, const expression_ref&);

  int n_parameters() const;

  int find_parameter(const std::string&) const;

  int add_parameter(const std::string& s);

  int add_expression(const expression_ref& e);

  int n_regs() const;

  boost::shared_ptr<reg> add_reg(const boost::shared_ptr<reg>&) const;
  boost::shared_ptr<reg> add_reg(reg*) const;
  boost::shared_ptr<reg> allocate_reg() const;
  boost::shared_ptr<reg> allocate_reg(const std::string&) const;
  boost::shared_ptr<reg> allocate_reg(const expression_ref&) const;

  context& operator=(const context&);
  context(const context& C);

  context();

  ~context();
};

expression_ref incremental_evaluate(const context& C, const expression_ref&);

#endif
