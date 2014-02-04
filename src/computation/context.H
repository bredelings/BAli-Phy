#ifndef CONTEXT_H

#include "cow-ptr.H"
#include "computation/program.H"
#include "computation/model_notes.H"
#include "computation/closure.H"
#include "mytypes.H"

class reg;

class reg_heap;

class Module;

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context: public Model_Notes
{
  // FIXME - disallow operator=() as long as there are any regs on the stack?
  //         (that is, which we are in the middle of modifying the context)?

  /// The array of registers that make up our memory.
  mutable object_ptr<reg_heap> memory_;

  object_ptr<reg_heap>& memory() const;

  // FIXME:speed - This is going to be really slow to copy around all the time!
  // FIXME:speed - Instead, wrap in cow_ptr< >.
  cow_ptr<Program> P;

  int context_index;

  int perform_io_head = -1;

  std::vector<int>& temp_heads() const;

  std::vector<int>& heads() const;

  std::vector<std::pair<std::string,int>>& parameters() const;

  std::map<std::string, int>& identifiers() const;

public:
  const std::vector<int>& triggers() const;
        std::vector<int>& triggers();

private:
  void release_identifiers();

  int push_temp_head() const;
  void pop_temp_head() const;

  int get_parameter_reg(int i) const;

  int get_modifiable_reg(int i) const;

  /// Find the modifiable that parameter_reg points to.
  int find_parameter_modifiable_reg(int i) const;

  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R, std::vector<int>&) const;
  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  closure translate_refs(closure&& C) const;

  /// Check if a compute expression is computed.
  bool reg_is_fully_up_to_date(int r) const;

  closure preprocess(const closure& C) const;

public:

  int get_context_index() const {return context_index;}

  const std::vector<Module>& get_Program() const {return *P;}

  int add_note(const expression_ref&);

  reg& access(int i) const;

  const closure& access_result_for_reg(int i) const;

  reg& operator[](int i) const;

  context* clone() const {return new context(*this);}

  bool parameter_is_modifiable(int p) const;

  /// Check if a compute expression is computed.
  bool compute_expression_is_up_to_date(int i) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate(int index) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref perform(int index) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate_expression_(closure&&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate_expression_(closure&&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  closure lazy_evaluate_expression(const expression_ref&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref evaluate_expression(const expression_ref&,bool=true) const;

  /// Return the value of a particular index, computing it if necessary
  object_ref perform_expression(const expression_ref&,bool=false) const;

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

  /// Get the value of a modifiable - by its location in memory
  object_ref get_reg_value(int R) const;

  /// Get the value of a modifiable
  object_ref get_modifiable_value(int index) const;

  /// Get the value of a parameter
  object_ref get_parameter_value(int index) const;

  /// Get the value of a parameter
  object_ref get_parameter_value(const std::string&) const;

  /// Update the value of a modifiable - by its location in memory
  void set_reg_value(int R, closure&&);

  /// Set the value of a modifiable
  void set_modifiable_value_(int index, closure&&);

  /// Set the value of a modifiable
  void set_modifiable_value(int index, const expression_ref&);

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

  const std::vector<int>& random_modifiables() const;

  object_ref get_range_for_reg(int r) const;

  object_ref get_parameter_range(int p) const;

protected:
  /// Create a new parameter named s
  void add_parameter_(const std::string& s);

public:
  /// Create a new parameter named s
  int add_parameter(const std::string& s);

  /// Create a new parameter named s
  int add_parameter(const std::string& s, const expression_ref& initializer);

  /// Create a new parameter named s
  int add_parameter_with_dist(const std::string& s, const expression_ref& dist);

  /// Reserve an identifier with name s
  int add_identifier(const std::string& s) const;

  /// Add a literal expression that MAY be reduced
  int add_compute_expression(const expression_ref& e);
  /// Add a literal expression that MAY be reduced
  int add_compute_expression_(closure&&);
  /// Change the i-th compute expression to e
  void set_compute_expression(int i, const expression_ref& e);
  /// Change the i-th compute expression to e
  void set_compute_expression_(int i, closure&& e);

  /// Should the ith compute expression be re_evaluated when invalidated?
  void set_re_evaluate(int i, bool b);

  int n_expressions() const;
  expression_ref get_expression(int i) const;

  void alphabetize_parameters();

  void compile();

  efloat_t get_probability() const;

  int add_probability_factor(const expression_ref& E);

protected:  
  void set_C(int R, closure&& c) const;
  int incremental_evaluate(int R) const;
  int incremental_evaluate_unchangeable(int R) const;
  int allocate() const;

  module_loader loader;

  void collect_garbage() const;
public:
  const module_loader& get_module_loader() const;

  const std::vector<std::string>& get_module_path() const;

  const std::vector<std::string>& get_builtins_path() const;

  void allocate_identifiers_for_modules(const std::vector<std::string>& module_names);

  void initialize_parameter_structures_for_modules(const std::vector<std::string>& module_names);

  context& operator+=(const Module& M);

  context& operator+=(const std::string& module_name);

  context& operator+=(const std::pair<std::string,std::string>& module_name);

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

void add_probability_expression(context& C);

expression_ref dist_for_parameter(const std::string& name, const std::vector<expression_ref>& notes);
#endif
