#ifndef CONTEXT_H

#include "computation/graph_register.H"

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context: virtual public Object
{
public:
  typedef reg_heap::root_t root_t;

private:
  // FIXME - disallow operator=() as long as there are any regs on the stack?
  //         (that is, which we are in the middle of modifying the context)?

  /// The array of registers that make up our memory.
  mutable boost::shared_ptr<reg_heap> memory;

  cow_ptr<Program> P;

  int token;

  std::vector<expression_ref> notes;

  std::vector<root_t>& temp_heads() const {return memory->get_temp_heads_for_context(token);}

  const std::vector<root_t>& heads() const {return memory->get_heads_for_context(token);}
        std::vector<root_t>& heads()       {return memory->get_heads_for_context(token);}

  const std::vector<root_t>& parameters() const {return memory->get_parameters_for_context(token);}
        std::vector<root_t>& parameters()       {return memory->get_parameters_for_context(token);}

  const std::map<std::string, root_t>& identifiers() const {return memory->get_identifiers_for_context(token);}
        std::map<std::string, root_t>& identifiers()       {return memory->get_identifiers_for_context(token);}

  root_t push_temp_head() const;
  void pop_temp_head() const;

  int get_token() const {return token;}

  /// Register a variable called 'name' as having location 'R'
  void add_identifier(const std::string& name, int R);

  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R) const;

public:

  int n_notes() const {return notes.size();}

  const std::vector<expression_ref>& get_notes() const {return notes;}
  std::vector<expression_ref>& get_notes() {return notes;}

  const expression_ref get_note(int i) const {return notes[i];}
  expression_ref get_note(int i) {return notes[i];}

  int add_note(const expression_ref&);

  int find_match_notes(const expression_ref& e, std::vector<expression_ref>&,int) const;

  boost::shared_ptr<const Object> default_parameter_value(int i) const;

  /// Remove a stack-allocated reg from the stack.
  root_t push_root(int R) const {return memory->push_root(R);}
  void pop_root(root_t r) const {memory->pop_root(r);}

  /// The total size of the reg memory
  int n_regs() const {return memory->n_regs();}
  /// How many regs are not known to be free?
  int n_used_regs() const {return memory->n_used_regs();}

  reg& access(int i) const {return memory->access(i);}

  reg& operator[](int i) const {return memory->access(i);}

  context* clone() const {return new context(*this);}

  /// Check if a parameter has a value.
  bool parameter_is_set(int i) const;

  /// Check if a compute expression is computed.
  bool compute_expression_is_up_to_date(int i) const;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> lazy_evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  boost::shared_ptr<const Object> lazy_evaluate_expression(const expression_ref&) const;

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

  /// Lookup the index for the named parameter s
  int find_parameter(const std::string& s) const;

  /// Create a new parameter named s
  int add_parameter(const std::string& s);

  /// Reserve an identifier with name s
  root_t add_identifier(const std::string& s);

  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const expression_ref& e);
  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const std::string& s, const expression_ref& e);

  int n_expressions() const;
  expression_ref get_expression(int i) const;

  void alphabetize_parameters();

protected:  
  void set_E(int R, const expression_ref& e) const {memory->set_E(R,e);}
  void set_reduction_result(int R, const expression_ref& result) const {memory->set_reduction_result(R, result);}
  int incremental_evaluate(int R) const {return memory->incremental_evaluate(R,token);}
  expression_ref full_evaluate(int& R) const;

  /// Allocate a reg which will be a root for garbage-collection
  root_t allocate_reg() const {
    root_t r = memory->allocate_reg();
    access(*r).owners.insert(token);
    return r;
  }

  void collect_garbage() const;
public:
  context& operator+=(const Def& D);

  context& operator+=(const Program& D);

  context& operator=(const context& c);

  context();

  context(const context&);

  context(const std::vector<expression_ref>&);

  ~context();
};

std::vector<expression_ref> add_prefix(const std::string&, const std::vector<expression_ref>&);

boost::shared_ptr<context> prefix_formula(const std::string&, const boost::shared_ptr<const context>&);

std::vector<expression_ref> combine(const std::vector<expression_ref>& N1, const std::vector<expression_ref>& N2);

std::ostream& operator<<(std::ostream&, const context& C);

int add_probability_expression(context& C);

#endif
