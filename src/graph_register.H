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

  // Which expression is this a reduction of
  boost::shared_ptr<const reg> parent;

  // Which input values were used to reduce the parent to this expression
  std::vector<boost::shared_ptr<const reg> > used_inputs;

  // Which reduction results made use of the value of this expression
  std::set<boost::shared_ptr<const reg> > outputs;

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

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?


struct reg_machine
{
  int n_tokens;
  std::vector< std::list <reg> > regs_for_token;
  std::list<reg> all_regs;

  /// is each token in use or not?
  std::vector<bool> is_token_active;

  /// Return an unused token.
  int find_free_token() const;
  /// Create an unused token.
  int add_token();
  /// Acquire a token for use with C columns and B/2 branches.
  int claim_token();
  /// Setup token2 to point to cached likelihoods for token1
  void copy_token(int token1,int token2);
  /// Initialize token
  void init_token(int token);
  /// Release token and mark unused.
  void release_token(int token);
};

struct context: virtual public Object
{
  boost::shared_ptr<reg_machine> machine;

  int token;
  
  context* clone() const {return new context(*this);}

  // the list of expressions that we are interested in evaluating.
  std::vector<boost::shared_ptr<reg> > heads;

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
  void set_parameter_value(int index, const object_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(const std::string& var, const object_ref&);

  int add_expression(const expression_ref& e);

  context& operator=(const context&);
  context(const context& C);

  context();

  ~context();
};

expression_ref incremental_evaluate(const context& C, const expression_ref&);

#endif
