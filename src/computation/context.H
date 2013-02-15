#ifndef CONTEXT_H

#include "computation/graph_register.H"
#include "computation/model_notes.H"
#include "computation/program.H"
#include "cow-ptr.H"

class Module;

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context: public Model_Notes
{
public:
  typedef reg_heap::root_t root_t;

private:
  // FIXME - disallow operator=() as long as there are any regs on the stack?
  //         (that is, which we are in the middle of modifying the context)?

  /// The array of registers that make up our memory.
  mutable boost::shared_ptr<reg_heap> memory;

  // FIXME:speed - This is going to be really slow to copy around all the time!
  // FIXME:speed - Instead, wrap in cow_ptr< >.
  cow_ptr<Program> P;

  int token;

  std::vector<root_t>& temp_heads() const {return memory->get_temp_heads_for_context(token);}

  const std::vector<root_t>& heads() const {return memory->get_heads_for_context(token);}
        std::vector<root_t>& heads()       {return memory->get_heads_for_context(token);}

  const std::vector<std::pair<std::string,root_t>>& parameters() const {return memory->get_parameters_for_context(token);}
        std::vector<std::pair<std::string,root_t>>& parameters()       {return memory->get_parameters_for_context(token);}

  const pool<int>& modifiable_regs() const {return memory->get_modifiable_regs_for_context(token);}
        pool<int>& modifiable_regs()       {return memory->get_modifiable_regs_for_context(token);}

  const std::map<std::string, root_t>& identifiers() const {return memory->get_identifiers_for_context(token);}
        std::map<std::string, root_t>& identifiers()       {return memory->get_identifiers_for_context(token);}

  root_t push_temp_head() const;
  void pop_temp_head() const;

  int get_parameter_reg(int i) const;

  int get_modifiable_reg(int i) const;

  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R, std::vector<int>&) const;
  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  closure translate_refs(closure&& C) const;

  /// Check if a compute expression is computed.
  bool reg_is_fully_up_to_date(int r) const;

  closure preprocess(const closure& C) const;

public:

  int get_token() const {return token;}

  const std::vector<Module>& get_Program() const {return *P;}

  int add_note(const expression_ref&);

  expression_ref default_parameter_value(int i) const;

  /// Remove a stack-allocated reg from the stack.
  root_t push_root(int R) const {return memory->push_root(R);}
  void pop_root(root_t r) const {memory->pop_root(r);}

  /// The total size of the reg memory
  int n_regs() const {return memory->n_regs();}
  /// How many regs are not known to be free?
  int n_used_regs() const {return memory->n_used_regs();}

  reg& access(int i) const {return memory->access(i);}

  const closure& access_result(int i) const {return memory->access_result(i);}

  reg& operator[](int i) const {return memory->access(i);}

  context* clone() const {return new context(*this);}

  /// Check if a parameter has a value.
  bool parameter_is_set(int i) const;

  /// Check if a compute expression is computed.
  bool compute_expression_is_up_to_date(int i) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate_expression_(closure&&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate_expression_(closure&&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate_expression(const expression_ref&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate_expression(const expression_ref&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref perform_expression(const expression_ref&,bool=true) const;

  template <typename T>
  object_ptr<const T> evaluate_as(int index) const
  {
    return convert<const T>( evaluate(index) );
  }

  template <typename T>
  object_ptr<const T> evaluate_expression_as(const expression_ref& E) const
  {
    return convert<const T>( evaluate_expression(E) );
  }

  /// Find the name of the ith named parameter
  std::string parameter_name(int i) const;

  /// Change the name of the i-th named parameter
  void rename_parameter(int i, const std::string& s);

  /// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
  object_ref get_parameter_value(int index) const;

  /// Get the value of a non-constant, non-computed index
  object_ref get_parameter_value(const std::string&) const;

  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, closure&&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(int index, const expression_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value_expression(int index, const expression_ref&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value_(int index, closure&&);

  /// Update the value of a non-constant, non-computed index
  void set_parameter_value(const std::string& var, const expression_ref&);

  int n_parameters() const;

  /// Lookup the index for the named parameter s
  int find_parameter(const std::string& s) const;

  /// Create a new parameter named s
  int add_parameter(const std::string& s);

  /// Reserve an identifier with name s
  root_t add_identifier(const std::string& s) const;

  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const expression_ref& e);
  /// Add a literal expression that MAY be reduced
  int add_compute_expression_(closure&&);
  /// Change the i-th compute expression to e
  void set_compute_expression(int i, const expression_ref& e);
  /// Change the i-th compute expression to e
  void set_compute_expression_(int i, closure&& e);

  int n_expressions() const;
  expression_ref get_expression(int i) const;

  void alphabetize_parameters();

protected:  
  void set_C(int R, closure&& c) const {memory->set_C(R,std::move(c));}
  void set_reduction_result(int R, closure&& result) const {memory->set_reduction_result(R, std::move(result) );}
  int incremental_evaluate(int R, bool ec=true) const {return memory->incremental_evaluate(R,token,ec);}

  /// Allocate a reg which will be a root for garbage-collection
  root_t allocate_reg() const {
    root_t r = memory->allocate_reg();
    memory->reg_add_owner(*r, token);
    return r;
  }

  module_loader loader;

  void collect_garbage() const;
public:
  const module_loader& get_module_loader() const;

  const std::vector<std::string>& get_module_path() const;

  const std::vector<std::string>& get_builtins_path() const;

  void allocate_identifiers_for_modules(const std::vector<std::string>& module_names);

  context& operator+=(const std::vector<Module>& D);

  context& operator+=(const std::vector<std::string>& module_names);

  context& operator=(const context& c);

  context(const module_loader&);

  context(const context&);

  context(const module_loader&, const std::vector<expression_ref>&);

  context(const module_loader&, const std::vector<expression_ref>&, const std::vector<Module>& P);

  context(const module_loader&, const std::vector<expression_ref>&, const std::vector<std::string>& module_names);

  ~context();
};

std::ostream& operator<<(std::ostream&, const context& C);

int add_probability_expression(context& C);

#endif
