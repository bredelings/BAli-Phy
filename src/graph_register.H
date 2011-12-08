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
#include <utility>

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
  std::set< std::pair<int,int> > outputs;

  // Which reduction results made use of the value of this expression (via call)
  std::set< int > call_outputs;

  // Does this expression reduce to another expression that we need to evaluate?
  int call;

  // What does this expression reduce to eventually? (WHNF)
  boost::shared_ptr< boost::shared_ptr<const Object> > result;

  // Which parameter VALUES have been used in computing the call/result
  std::vector<int> used_parameters;

  bool is_valid() const {return E;}

  enum States {none=0,free=1,used=2,checked=3};

  int prev_reg;
  int next_reg;
  States state;

  void clear();
  void init();

  reg();
};

struct var: public Object
{
  std::string name;

  var* clone() const {return new var(*this);}

  std::string print() const
  {
    return "<" + name + ">";
  }

  tribool compare(const Object& o) const
  {
    const var* V = dynamic_cast<const var*>(&o);
    if (not V)
      return false;

    return name == V->name;
  }

  var(const std::string& s)
    :name(s)
  { }
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

class reg_heap
{
  std::vector<reg> memory;
  std::vector<int> roots;
  std::vector<int> stack_roots;

  /// Start of free list 
  int first_free_reg;

  /// Start of used list 
  int first_used_reg;

  int allocate_reg();
  void expand_memory(int);

  /// Add a reg (not on any list) to the free list
  int add_reg_to_free_list(int);
  /// Take the first free reg off the free list, or return -1 if empty
  int get_free_reg();

  /// Add a reg (not on any list) to the used list
  int add_reg_to_used_list(int);

  void remove_reg_from_used_list(int);

  void reclaim_used_reg(int);

public:
  const reg& access(int i) const {return memory[i];}
        reg& access(int i)       {return memory[i];}

  const reg& operator[](int i) const {return access(i);}
        reg& operator[](int i)       {return access(i);}

  int n_regs() const;
  int n_free_regs() const;
  int n_used_regs() const;

  void push_reg(int);
  void pop_reg(int);

  int allocate_root_reg();
  int allocate_stack_reg();

  void collect_garbage();

  reg_heap();
};

class context: virtual public Object
{
  // FIXME - disallow operator=() as long as there are any regs on the stack?
  //         (that is, which we are in the middle of modifying the context)?

  /// The array of registers that make up our memory.
  mutable reg_heap memory;

  /// The register indices of the named parameters.
  std::vector<int> parameters;

  /// A list of variable names, and the register indices to which they correspond.
  std::map<std::string,int> variables;
  /// Register a variable called 'name' as having location 'R'
  void add_variable(const std::string& name, int R);
  /// Change the name of the i-th named variable
  void rename_variable(const std::string&, const std::string&);

  /// Translate named variables (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R) const;

public:
  void set_used_input(int R1, int slot, int R2) const;
  void clear_used_input(int R, int slot) const;
  void clear_used_inputs(int R) const;
  void clear_used_inputs(int R, int S) const;
  void set_call(int R1, int R2) const;
  void clear_call(int R) const;

  /// Remove a stack-allocated reg from the stack.
  void pop_reg(int R) const {memory.pop_reg(R);}

  /// The total size of the reg memory
  int n_regs() const {return memory.n_regs();}
  /// How many regs are not known to be free?
  int n_used_regs() const {return memory.n_used_regs();}

  reg& access(int i) const {return memory[i];}

  reg& operator[](int i) const {return memory[i];}

  expression_ref& value(int i) const {return memory[i].E;}

  context* clone() const {return new context(*this);}

  // the list of expressions that we are interested in evaluating.
  mutable std::vector<int> heads;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate(const expression_ref&) const;

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(int index) const
  {
    return convert<const T>( evaluate(index) );
  }

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(const expression_ref& E) const
  {
    return convert<const T>( evaluate(E) );
  }

  /// Find the name of the ith named parameter
  std::string parameter_name(int i) const;

  /// Change the name of the i-th named parameter
  void rename_parameter(int i, const std::string& s);

  /// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
  boost::shared_ptr<const Object> get_parameter_value(int index) const;

  /// Get the value of a non-constant, non-computed index
  boost::shared_ptr<const Object> get_parameter_value(const std::string&) const;

  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, const expression_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(int index, const expression_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(const std::string& var, const expression_ref&);

  int n_parameters() const;

  /// Lookup the reg for the named parameter s
  int find_parameter(const std::string& s) const;

  /// Lookup the reg for the named variable s (not a parameter)
  int find_variable(const std::string& s) const;

  /// Create a new parameter named s
  int add_parameter(const std::string& s);

  /// Add a literal expression that may NOT be reduced
  int add_expression(const expression_ref& e);

  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const expression_ref& e);
  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const std::string& s, const expression_ref& e);

  int n_expressions() const;
  expression_ref get_expression(int i) const;

  /// Allocate a reg which will be a root for garbage-collection
  int allocate_root_reg() const;
  /// Allocate a reg which will be on the stack until removed
  int allocate_stack_reg() const;

  void collect_garbage() const;

  int find_match_expression(const expression_ref& e, std::vector<expression_ref>&) const;
};

expression_ref incremental_evaluate(const context& C, const expression_ref&);

boost::shared_ptr<context> prefix_formula(const std::string&, const boost::shared_ptr<const context>&);

std::ostream& operator<<(std::ostream&, const context& C);
#endif
