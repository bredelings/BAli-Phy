#ifndef CONTEXT_H

#include <boost/optional.hpp>
#include "computation/closure.H"
#include "computation/program.H"
#include "math/log-double.H"

class reg;

class reg_heap;

class Module;

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context
{
    // FIXME - disallow operator=() as long as there are any regs on the stack?
    //         (that is, which we are in the middle of modifying the context)?

    /// The array of registers that make up our memory.
    mutable object_ptr<reg_heap> memory_;

    object_ptr<reg_heap>& memory() const;

    // FIXME:speed - This is going to be really slow to copy around all the time!
    // FIXME:speed - Instead, wrap in cow_ptr< >.

    int context_index;

    int perform_io_head = -1;

    const std::vector<int>& heads() const;

    std::vector<std::pair<std::string,int>>& parameters() const;

    std::map<std::string, int>& identifiers() const;

private:
    int push_temp_head() const;
    void pop_temp_head() const;

    int get_parameter_reg(int i) const;

    int get_modifiable_reg(int i) const;

    closure preprocess(const closure& C) const;

    Program& get_Program();

public:

    const reg_heap& get_memory() const {return *memory();}

    const std::vector<std::string>& get_args() const;
    void set_args(const std::vector<std::string>& args);

    int get_context_index() const {return context_index;}

    const Program& get_Program() const;

    void clear_program();

    void clear_identifiers();

    reg& access(int i) const;

    const closure& access_result_for_reg(int i) const;

    reg& operator[](int i) const;

    context* clone() const {return new context(*this);}

    boost::optional<int> parameter_is_modifiable_reg(int p) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& perform(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate_expression_(closure&&,bool=true) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate_expression_(closure&&,bool=true) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate_expression(const expression_ref&,bool=true) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate_expression(const expression_ref&,bool=true) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& perform_expression(const expression_ref&,bool=false) const;

    /// Find the name of the ith named parameter
    std::string parameter_name(int i) const;

    /// Change the name of the i-th named parameter
    void rename_parameter(int i, const std::string& s);

    /// Get the value of a modifiable - by its location in memory
    const expression_ref& get_reg_value(int R) const;

    /// Get the value of a modifiable
    const expression_ref& get_modifiable_value(int index) const;

    /// Get the value of a parameter
    const expression_ref& get_parameter_value(int index) const;

    /// Get the value of a parameter
    const expression_ref& get_parameter_value(const std::string&) const;

    /// Update the value of a modifiable - by its location in memory
    void set_reg_value(int R, closure&&);

    /// Set the value of a modifiable
    void set_modifiable_value_(int index, closure&&);

    /// Set the value of a modifiable
    void set_modifiable_value(int index, const expression_ref&);

    /// Update the value of a non-constant, non-computed index
    void set_parameter_value(int index, const expression_ref&);

    /// Update the value of a non-constant, non-computed index
    void set_parameter_value_(int index, closure&&);

    /// Update the value of a non-constant, non-computed index
    void set_parameter_value(const std::string& var, const expression_ref&);

    int n_parameters() const;

    /// Lookup the index for the named parameter s
    boost::optional<int> maybe_find_parameter(const std::string& s) const;

    /// Lookup the index for the named parameter s
    int find_parameter(const std::string& s) const;

    const std::vector<int>& random_modifiables() const;

    const expression_ref get_range_for_reg(int r) const;

    const expression_ref get_parameter_range(int p) const;

    double get_rate_for_reg(int r) const;

public:
    /// Create a new modifiable parameter named s with initial value 'value'
    int add_modifiable_parameter_with_value(const std::string& s, const expression_ref& value);

    /// Create a new parameter named s for expression E
    int add_parameter(const std::string& s, const expression_ref& E);

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

    int n_expressions() const;
    expression_ref get_expression(int i) const;

    void compile();

    log_double_t get_probability() const;

    int add_probability_factor(const expression_ref& E);

protected:  
    void set_C(int R, closure&& c) const;
    int allocate() const;

    void collect_garbage() const;
public:
    void show_graph() const;
  
    void allocate_identifiers_for_modules(const std::vector<std::string>& module_names);

    void perform_transition_kernel(int i);

    int n_transition_kernels() const;

    context& operator+=(const Module& M);

    context& operator+=(const std::string& module_name);

    context& operator+=(const std::vector<std::string>& module_names);

    context& operator=(const context& c);

    context(const context&);

    context(const Program& P);

    ~context();
};

std::ostream& operator<<(std::ostream&, const context& C);
#endif
