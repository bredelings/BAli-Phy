#ifndef CONTEXT_H
#define CONTEXT_H

#include <unordered_set>
#include <vector>
#include "computation/closure.H"
#include "computation/program.H"
#include "util/math/log-double.H"
#include "util/bounds.H"
#include "mcmc/prob_ratios.H"
#include "util/json.hh"
#include <functional>

class reg_heap;

class context;

/// This class contains only compute expressions, parameters, and names for the compute expressions
class context_ref
{
    friend class context;
    friend class context_ptr;

    class lazy_attribute_map
    {
        const context_ref& C;
        const std::map<std::string,int>* m;
    public:
        std::vector<std::string> arg_names() const;
        std::optional<int> get(const std::string& s) const;
        lazy_attribute_map(const context_ref&, const std::map<std::string,int>*);
    };

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

    closure preprocess(const closure& C) const;

public:

    int get_compute_expression_reg(int i) const;

    std::optional<int> find_modifiable_reg(int r) const;

    /// Get SOME model modifiable values
    EVector get_modifiable_values(const std::vector<int>&) const;

    const reg_heap& get_memory() const {return *memory();}

    const std::vector<std::string>& get_args() const;
    void set_args(const std::vector<std::string>& args);

    int get_context_index() const {return context_index;}

    const closure& access_result_for_reg(int i) const;

    const closure& operator[](int i) const;

    std::optional<int> compute_expression_is_modifiable_reg(int p) const;

    bool reg_is_modifiable(int p) const;

    const closure* precomputed_value_for_reg(int r) const;

    const closure* precomputed_value_for_head(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate_head(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const closure& lazy_evaluate_reg(int& r) const;

    /// Return the value of a particular index, computing it if necessary
    std::pair<int,int> incremental_evaluate(int r) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate_head(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& evaluate_head_unchangeable(int index) const;

    /// Return the value of a particular index, computing it if necessary
    const expression_ref& perform_head(int index, bool ec = false) const;

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

    /// Compute and the return the value of a function application.
    const expression_ref& evaluate_apply(const expression_ref& f, const expression_ref& x, bool=true) const;

    /// Evaluate a particular reg and return the value.
    const expression_ref& evaluate_reg(int r) const;

    /// Return a structure where all fields are evaluated and substituted.
    expression_ref recursive_evaluate_reg(int r) const;

    /// Return a structure where all fields are evaluated and substituted.
    expression_ref recursive_evaluate_head(int r) const;

    /// Get the value of a modifiable - by its location in memory
    const expression_ref& get_reg_value(int R) const;

    /// Get the value of a modifiable
    const expression_ref& get_modifiable_value(int index) const;

    /// Update the value of a modifiable - by its location in memory
    void set_reg_value(int R, closure&&);

    /// Set the value of a modifiable
    void set_modifiable_value_(int r, closure&&);

    /// Set the value of a modifiable
    void set_modifiable_value(int r, const expression_ref&);

    /// Swap the steps for two modifiables.
    void interchange_regs(int r1, int r2);

    std::optional<int> out_edges_from_dist(int r) const;
    const std::set<int>* out_edges_to_var(int r) const;
    std::optional<lazy_attribute_map> in_edges_to_dist(int r)  const;
    std::optional<lazy_attribute_map> dist_properties(int s) const;
    std::optional<std::string> dist_type(int s) const;

public:

    std::set<int> tweak_and_find_affected_sampling_events(const std::function<void(context_ref&)>&);
    std::set<int> find_affected_sampling_events(const std::function<void(context_ref&)>&);

    /// Add a literal expression that MAY be reduced
    int add_compute_expression(const expression_ref& e);
    /// Add a literal expression that MAY be reduced
    int add_compute_expression_(closure&&);

    int n_expressions() const;

    expression_ref get_expression(int i) const;

    log_double_t prior() const;
    log_double_t likelihood() const;
    log_double_t probability() const;

    virtual double get_beta() const;
    virtual void set_beta(double);
    log_double_t heated_likelihood() const;
    log_double_t heated_probability() const;

    prob_ratios_t probability_ratios(const context_ref& C1) const;
    prob_ratios_t heated_probability_ratios(const context_ref& C1) const;
    log_double_t heated_probability_ratio(const context_ref&) const;

protected:  
    void collect_garbage() const;

public:

    void write_factor_graph() const;

    void write_factor_graph(std::ostream&) const;

    void show_graph() const;
  
    void show_graph_for_root_token() const;

    void run_transition_kernels();
    void perform_transition_kernel(int s);
    int n_transition_kernels() const;

    void run_loggers(long iteration);
    void perform_logger(int s, long iteration);
    int n_loggers() const;

    expression_ref evaluate_program() const;

    virtual json::object get_logged_parameters() const;

    context_ref& operator=(const context_ref& c);

    explicit context_ref(const context_ref&) = default;

    context_ref(reg_heap& M);

    context_ref(reg_heap& M, int c);

    virtual ~context_ref() = default;
};

class Module;

class context: public context_ref
{

public:
    virtual context* clone() const {return new context(*this);}

    context& operator=(const context&) = default;

    context& operator=(context&&);

    context(const context_ref&);

    context(const context&);

    context(context&&);

    virtual ~context();
};

void show_parameters(std::ostream& o,const context_ref& C);

std::string show_parameters(const context_ref& C);

bool accept_MH(const context_ref& C1, const context_ref& C2, log_double_t rho);

bool perform_MH(context_ref& C1, const context_ref& C2, log_double_t rho);

std::ostream& operator<<(std::ostream&, const context_ref& C);

typedef std::function<log_double_t(context_ref& P)> Proposal;

void simplify(json::object& j);

json::object flatten_me(const json::object& j);

#endif
