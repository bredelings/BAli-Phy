#ifndef CONTEXT_H
#define CONTEXT_H

#include "computation/closure.H"
#include "computation/param.H"
#include "computation/program.H"
#include "util/math/log-double.H"
#include "util/bounds.H"
#include "mcmc/prob_ratios.H"
#include "util/json.hh"

class reg_heap;

class context;

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context_ref
{
    friend class context;

protected:
    // FIXME - disallow operator=() as long as there are any regs on the stack?
    //         (that is, which we are in the middle of modifying the context)?

    /// The array of registers that make up our memory.
    mutable object_ptr<reg_heap> memory_;

    object_ptr<reg_heap>& memory() const;

    // FIXME:speed - This is going to be really slow to copy around all the time!
    // FIXME:speed - Instead, wrap in cow_ptr< >.

    int context_index = -1;

    const std::vector<int>& heads() const;

    std::vector<std::pair<std::string,int>>& parameters() const;

    std::map<std::string, int>& identifiers() const;

    closure preprocess(const closure& C) const;

    Program& get_Program();

public:

    int get_parameter_reg(int i) const;

    int get_compute_expression_reg(int i) const;

    std::optional<int> get_modifiable_reg(int i) const;

    bool parameter_has_bounds(int i) const;
    const bounds<double>& get_parameter_bounds(int i) const;

    /// Get SOME model modifiable values
    EVector get_modifiable_values(const std::vector<int>&) const;

    /// Get SOME model parameter values
    EVector get_parameter_values(const std::vector<int>&) const;

    /// Set SOME of the model parameter values
    void set_parameter_values(const std::vector<int>& indices,const std::vector<expression_ref>& p);

    const reg_heap& get_memory() const {return *memory();}

    const std::vector<std::string>& get_args() const;
    void set_args(const std::vector<std::string>& args);

    int get_context_index() const {return context_index;}

    const Program& get_Program() const;

    void clear_program();

    void clear_identifiers();

    const closure& access_result_for_reg(int i) const;

    const closure& operator[](int i) const;

    std::optional<int> parameter_is_modifiable_reg(int p) const;

    std::optional<int> compute_expression_is_modifiable_reg(int p) const;

    std::optional<int> compute_expression_is_random_variable(int p) const;

    bool compute_expression_has_bounds(int i) const;
    bounds<double> get_bounds_for_compute_expression(int i) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& perform(int index, bool ec = false) const;

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

    /// Return a structure where all fields are evaluated and substituted.
    expression_ref recursive_evaluate_reg(int r) const;

    /// Return a structure where all fields are evaluated and substituted.
    expression_ref recursive_evaluate(int r) const;

    /// Return a structure where all fields are evaluated and substituted.
    expression_ref recursive_evaluate_parameter(int) const;

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
    std::optional<int> maybe_find_parameter(const std::string& s) const;

    /// Lookup the index for the named parameter s
    int find_parameter(const std::string& s) const;

    const std::vector<int>& random_variables() const;

    const expression_ref get_range_for_random_variable(int r) const;

    const expression_ref get_parameter_range(int p) const;

    double get_rate_for_random_variable(int r) const;

public:
    /// Create a new modifiable head with initial value 'value'
    param new_modifiable(const expression_ref& value);
    /// Create a new modifiable parameter named s with initial value 'value'
    int add_modifiable_parameter(const std::string& s, const expression_ref& value);

    /// Add a literal expression that MAY be reduced
    int add_compute_expression(const expression_ref& e);
    /// Add a literal expression that MAY be reduced
    int add_compute_expression_(closure&&);

    int add_likelihood_factor(const expression_ref& E);

    int n_expressions() const;

    expression_ref get_expression(int i) const;

    void compile();

    virtual log_double_t prior() const;

    virtual log_double_t likelihood() const;

    virtual log_double_t probability() const;

    virtual double get_beta() const {return 1.0;}

    virtual void set_beta(double) {std::abort();}

    virtual log_double_t heated_likelihood() const;
    virtual log_double_t heated_probability() const {return prior() * heated_likelihood();}

    virtual log_double_t heated_probability_ratio(const context&) const;

    prob_ratios_t probability_ratios(const context_ref& C1) const;

    std::vector<double> get_mcmc_move_weights() const;

protected:  
    void collect_garbage() const;

public:

    void show_graph() const;
  
    void run_transition_kernels();

    void perform_transition_kernel(int i);

    int n_transition_kernels() const;

    expression_ref evaluate_program() const;

    virtual json get_logged_parameters() const;

    context_ref& operator=(const context_ref& c) = delete;

    context_ref(const context_ref&) = delete;

    context_ref(reg_heap& M);

    context_ref(reg_heap& M, int c);

    virtual ~context_ref() = default;
};

class Module;

class context: public context_ref
{

public:
//    context* clone() const {return new context(*this);}

    int add_program(const expression_ref&);

    /// Reserve an identifier with name s
    int add_identifier(const std::string& s) const;

    void allocate_identifiers_for_modules(const std::vector<std::string>& module_names);

    context& operator+=(const Module& M);

    context& operator+=(const std::string& module_name);

    context& operator+=(const std::vector<std::string>& module_names);

    context& operator=(const context_ref& c);

    context& operator=(const context& c);

    context(const context_ref&);

    context(const context&);

    context(const Program& P);

    virtual ~context();
};

std::ostream& operator<<(std::ostream&, const context_ref& C);
#endif
