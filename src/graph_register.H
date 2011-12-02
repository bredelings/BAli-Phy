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
  std::vector< int > used_inputs;

  // Which reduction results made use of the value of this expression (via an operation)
  std::set< int > outputs;

  // Which reduction results made use of the value of this expression (via call)
  std::set< int > call_outputs;

  // Does this expression reduce to another expression that we need to evaluate?
  int call;

  // What does this expression reduce to eventually? (WHNF)
  boost::shared_ptr< boost::shared_ptr<const Object> > result;

  // Which parameter VALUES have been used in computing the call/result
  std::vector<int> used_parameters;

  bool is_valid() const {return E;}

  enum States {none=0,free=1,used=2};

  int prev_reg;
  int next_reg;
  States state;

  void clear();
  void init();
  void init(const std::string& s);
  void init(const expression_ref&);

  reg();
};


struct reg_var: public Object
{
  int target;

  reg_var* clone() const {return new reg_var(*this);}

  std::string print() const 
  {
    return "<" + convertToString(target) + ">";
  }

  tribool compare(const Object& o) const
  {
    const reg_var* E = dynamic_cast<const reg_var*>(&o);
    if (not E) 
      return false;

    return target == E->target;
  }

  reg_var():
    target(-1)
  { }

  reg_var(int t):
    target(t)
  { }
};

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

struct reg_heap
{
  std::vector<reg> memory;

  int first_free_reg;

  int first_used_reg;

  const reg& access(int i) const {return memory[i];}
        reg& access(int i)       {return memory[i];}

  const reg& operator[](int i) const {return access(i);}
        reg& operator[](int i)       {return access(i);}

  int n_regs() const;
  int n_free_regs() const;
  int n_used_regs() const;

  int add_reg_to_free_list(int);
  int get_free_reg();
  int add_reg_to_used_list(int);
  int get_used_reg();
  void expand_memory(int);

  int allocate_reg();

  reg_heap();
};

struct context: virtual public Object
{
  mutable reg_heap memory;
  int n_regs() const {return memory.n_regs();}
  int n_used_regs() const {return memory.n_used_regs();}

  std::vector<std::string> parameter_names;
  std::vector<int> parameters;

  reg& access(int i) const {return memory[i];}

  reg& operator[](int i) const {return memory[i];}

  expression_ref& value(int i) const {return memory[i].E;}

  context* clone() const {return new context(*this);}

  // the list of expressions that we are interested in evaluating.
  mutable std::vector<int> heads;

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

  int allocate_reg() const;
  int allocate_reg(const std::string&) const;
  int allocate_reg(const expression_ref&) const;
};

expression_ref incremental_evaluate(const context& C, const expression_ref&);

#endif
