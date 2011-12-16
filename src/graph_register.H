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
  /// The expression
  expression_ref E;

  /// Is E a parameter, or is the result dependent on a parameter value?
  bool changeable;

  /// Which reg's were used to reduce this expression?
  std::vector< int > used_inputs;

  /// Which pairs (reg,slot) USED the reduction result of E (via an operation)?
  std::set< std::pair<int,int> > outputs;

  /// Which reduction results made use of the value of this expression (via call)
  std::set< int > call_outputs;

  /// Does E reduce to another reg that we need to evaluate to get the true result?
  int call;

  /// What does E reduce to eventually? (WHNF, but not a heap variable.)
  boost::shared_ptr< boost::shared_ptr<const Object> > result;

  /// Which arguments have we EVALUATED the call/result of E?
  std::vector<int> used_parameters;

  bool is_valid() const {return E;}

  enum States {none=0,free=1,used=2,checked=3};

  int prev_reg;
  int next_reg;
  States state;

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
  std::list<int> roots;

  /// Start of free list 
  int first_free_reg;

  /// Start of used list 
  int first_used_reg;

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
  typedef std::list<int>::iterator root_t;
  typedef std::list<int>::const_iterator const_root_t;

  const reg& access(int i) const {return memory[i];}
        reg& access(int i)       {return memory[i];}

  const reg& operator[](int i) const {return access(i);}
        reg& operator[](int i)       {return access(i);}

  int n_regs() const;
  int n_free_regs() const;
  int n_used_regs() const;

  root_t push_root(int);
  void pop_root(root_t);

  root_t allocate_reg();

  void collect_garbage();

  void set_used_input(int R1, int slot, int R2);
  void clear_used_input(int R, int slot);
  void clear_used_inputs(int R);
  void clear_used_inputs(int R, int S);
  void set_call(int R1, int R2);
  void clear_call(int R);
  void clear(int);

  reg_heap();
};

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context: virtual public Object
{
  // FIXME - disallow operator=() as long as there are any regs on the stack?
  //         (that is, which we are in the middle of modifying the context)?

  /// The array of registers that make up our memory.
  mutable reg_heap memory;

  // the list of expressions that we are interested in evaluating.
  std::vector<reg_heap::root_t> heads;

  /// The register indices of the named parameters.
  std::vector<reg_heap::root_t> parameters;

  /// A list of variable names, and the register indices to which they correspond.
  std::vector<std::pair<std::string,int> > variables;

  /// Register a variable called 'name' as having location 'R'
  void add_variable(const std::string& name, int R);

  /// Translate named variables (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R) const;

  std::vector<expression_ref> notes;

public:
  typedef reg_heap::root_t root_t;

  int n_notes() const {return notes.size();}

  const std::vector<expression_ref>& get_notes() const {return notes;}
  std::vector<expression_ref>& get_notes() {return notes;}

  const expression_ref get_note(int i) const {return notes[i];}
  expression_ref get_note(int i) {return notes[i];}

  int add_note(const expression_ref&);

  int find_match_notes(const expression_ref& e, std::vector<expression_ref>&,int) const;

  boost::shared_ptr<const Object> default_parameter_value(int i) const;

  /// Remove a stack-allocated reg from the stack.
  root_t push_root(int R) const {return memory.push_root(R);}
  void pop_root(root_t r) const {memory.pop_root(r);}

  /// The total size of the reg memory
  int n_regs() const {return memory.n_regs();}
  /// How many regs are not known to be free?
  int n_used_regs() const {return memory.n_used_regs();}

  reg& access(int i) const {return memory[i];}

  reg& operator[](int i) const {return memory[i];}

  expression_ref& value(int i) const {return memory[i].E;}

  context* clone() const {return new context(*this);}

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> evaluate_expression(const expression_ref&) const;

  template <typename T>
  boost::shared_ptr<const T> evaluate_as(int index) const
  {
    return convert<const T>( evaluate(index) );
  }

  template <typename T>
  boost::shared_ptr<const T> evaluate_expression_as(const expression_ref& E) const
  {
    return convert<const T>( evaluate_expression(E) );
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

  int n_variables() const;

  /// Lookup the reg for the named variable s (not a parameter)
  int find_variable(const std::string& s) const;

  const std::string& variable_name(int i) const;

  /// Change the name of the variable named s 
  void rename_variable(int i, const std::string&);

  /// Create a new parameter named s
  int add_parameter(const std::string& s);

  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const expression_ref& e);
  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const std::string& s, const expression_ref& e);

  int n_expressions() const;
  expression_ref get_expression(int i) const;

  void set_used_input(int R1, int slot, int R2) const {memory.set_used_input(R1, slot, R2);}
  void clear_used_input(int R, int slot) const {memory.clear_used_input(R, slot);}
  void clear_used_inputs(int R) const {memory.clear_used_inputs(R);}
  void clear_used_inputs(int R, int S) const {memory.clear_used_inputs(R,S);}
  void set_call(int R1, int R2) const {memory.set_call(R1, R2);}
  void clear_call(int R) const {memory.clear_call(R);}
  void set_call_if_reg_result(int R) const;

  /// Allocate a reg which will be a root for garbage-collection
  root_t allocate_reg() const {return memory.allocate_reg();}

  void collect_garbage() const;

  context();

  context(const std::vector<expression_ref>&);
};

class reg_ptr
{
  reg_heap::root_t r;
  const context& C;
public:
  typedef reg_heap::root_t root_t;

  const reg& operator*()    const throw()   {return  C[*r];}
  const reg* operator->()   const throw()   {return &C[*r];}
  const reg* get()          const throw()   {return &C[*r];}
    
  reg& operator*()                          {return  C[*r];}
  reg* operator->()                         {return &C[*r];}
  reg* modify()                             {return &C[*r];}

  const context& get_context() const {return C;}

  boost::shared_ptr<const Object> evaluate() const
  {
    return C.evaluate(*r);
  }

  template<typename T>
  boost::shared_ptr<const T> evaluate_as() const
  {
    return C.evaluate_as<T>(*r);
  }

  reg_ptr(int R, const context& c)
    :r(c.push_root(R)), C(c)
  { }

  ~reg_ptr()
  {
    C.pop_root(r);
    r = root_t();
  }
};

std::vector<expression_ref> add_prefix(const std::string&, const std::vector<expression_ref>&);

boost::shared_ptr<context> prefix_formula(const std::string&, const boost::shared_ptr<const context>&);

std::vector<expression_ref> combine(const std::vector<expression_ref>& N1, const std::vector<expression_ref>& N2);

std::ostream& operator<<(std::ostream&, const context& C);
#endif
