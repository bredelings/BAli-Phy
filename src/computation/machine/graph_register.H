#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H

#include <string>
#include <vector>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include <utility>
#include "computation/closure.H"
#include "pool.H"
#include "computation/program.H"
#include <boost/container/small_vector.hpp>
#include <bitset>
#include "computation/loader.H"
#include "util/assert.hh"
#include "kahan.H"
#include "mcmc/prob_ratios.H"
#include "mapping.H"

struct Step
{
    int source_reg = -1;

    int call = 0;

    // NOTE: There is a space-time trade-off in the number of used_inputs here.
    //       However, small_vector< ,1> takes little or no extra space, and noticeably saves time.

    /// Which reg's were used to reduce this expression?
    boost::container::small_vector< std::pair<int,int>, 2 > used_inputs;
    boost::container::small_vector< int, 2 > forced_regs;
    boost::container::small_vector< int, 2 > created_regs;

    std::bitset<8> flags;

    void clear();

    void check_cleared();

    Step& operator=(const Step&) = delete;
    Step& operator=(Step&&) noexcept;

    Step() = default;

    Step(const Step&) = delete;
    Step(Step&&) noexcept;
};

struct Result
{
    int source_step = -1;

    int source_reg = -1;

    /// What final WHNF does this computation reduce to?
    int value = 0;

    /// Does C reduce to another reg that we need to evaluate to get the true value?
    std::pair<int,int> call_edge;
  
    /// Which pairs (reg,slot) USED the reduction value of C (via an operation)?
    boost::container::small_vector<std::pair<int,int>, 2> used_by;

    /// Which reduction values made use of the value of this expression (via call)
    boost::container::small_vector<int, 5> called_by;

    std::bitset<8> flags;

    void clear();

    void check_cleared();

    Result& operator=(const Result&) = delete;
    Result& operator=(Result&&) noexcept;

    Result() = default;

    Result(const Result&) = delete;
    Result(Result&&) noexcept;
};

constexpr int non_computed_index = -1;

constexpr int non_existant_index = -2;

class reg
{
public:
    /* The closure */
    closure C;

    enum class type_t {unknown=0, index_var=1, constant=2, changeable=3};

    type_t type = type_t::unknown;

    std::pair<int, int> created_by;

    std::bitset<8> flags;

    void clear();

    void check_cleared();

    reg& operator=(const reg&) = delete;
    reg& operator=(reg&&) noexcept;

    reg() = default;

    reg(const reg&) = delete;
    reg(reg&&) noexcept;
};

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

class reg_heap: public Object
{
    pool<reg> regs;

    mutable pool<Step> steps;

    mutable pool<Result> results;

    mutable int n_active_scratch_lists = 0;
    mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

    std::vector<std::function<void(int)>> reroot_handlers;

    void resize(int s);

    void reclaim_used(int);

    bool result_is_called_by(int, int) const;
    bool result_is_used_by(int, int) const;
    bool reg_is_used_by(int, int) const;

public:
    const closure& operator[](int r) const {return regs.access(r).C;}
    bool reg_is_used(int r) const {return regs.is_used(r);}
    reg::type_t reg_type(int r) const {return regs.access(r).type;}
    std::optional<int> creator_of_reg(int r) const;
    bool reg_is_contingent(int r) const;
    bool reg_exists(int r) const;
    bool step_exists_in_root(int s) const;
    size_t size() const;

    reg_heap* clone() const {std::abort();}

    void check_used_regs_in_token(int) const;
    void check_used_regs() const;

    void collect_garbage();
    void trace(std::vector<int>& remap);
    void trace_and_reclaim_unreachable();
    bool reg_is_changeable(int r) const;
    bool reg_is_constant(int r) const;
    void make_reg_changeable(int r);

    void set_used_input(int s1, int R2);
    void set_forced_input(int s1, int R2);
    void set_call(int R1, int R2);
    void destroy_all_computations_in_token(int t);
    void clear_call(int s);
    void clear_call_for_reg(int R);
    void clear_C(int R);
    void set_C(int R, closure&& C);
    int allocate();
    int allocate_reg_from_step(int s);
    int allocate_reg_from_step(int s, closure&& C);
    int allocate_reg_from_step_in_token(int s, int t);
    void mark_reg_created_by_step(int r, int s);

    void get_roots(std::vector<int>&, bool keep_identifiers=true) const;

    // the list of expressions that we are temporarily evaluating
    std::vector<int> temp;
  
    // the list of expressions that we are interested in evaluating.
    std::vector<int> heads;

    std::optional<int> program_result_head;
    std::optional<int> logging_head;

    int add_program(const expression_ref& E);

    // the list of regs that are currently being evaluated
    std::vector<int> stack;

    // a list of closures under evaluation
    std::vector<closure> closure_stack;

    kahan_multiplier prior;

    void register_prior(int r);

    int register_prior(closure&& C);

    log_double_t prior_for_context_full(int c);

    log_double_t prior_for_context(int c);

    std::vector<int> likelihood_heads;

    log_double_t constant_likelihood = 1.0;

    kahan_multiplier likelihood;

    std::vector<int> likelihoods_list;

    void register_likelihood(int r);

    int register_likelihood(closure&& C);

    void dec_likelihood_for_reg(int r);

    bool inc_likelihood_for_reg(int r);

    void dec_likelihood(int rc);

    bool inc_likelihood(int rc);

    log_double_t likelihood_for_context_full(int c);

    log_double_t likelihood_for_context_diff(int c);

    log_double_t likelihood_for_context(int c);

    log_double_t probability_for_context(int c);

    prob_ratios_t probability_ratios(int c1, int c2);

    // the list of random variables
    std::vector<int> random_variables_;

    const std::vector<int>& random_variables() const;

    void register_random_variable(int r);

    // the list of transition kernels
    std::vector<int> transition_kernels_;

    const std::vector<int>& transition_kernels() const;

    int add_transition_kernel(int);

    /// The registered indices of the named parameters.
    std::vector<std::pair<std::string,int>> parameters;
  
    /// Probably this would be non-parameter identifiers
    std::map<std::string, int> identifiers;
  
    std::shared_ptr<Program> P;

    std::vector<std::string> args;

    /// Get the memory location (reg) for a given identifier (string)
    int reg_for_id(const std::string&);
    /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
    expression_ref translate_refs(const expression_ref& R, closure::Env_t&);
    /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
    closure translate_refs(closure&& C);

    closure preprocess(const closure& C);

    /// Lookup the index for the named parameter s
    std::optional<int> maybe_find_parameter(const std::string& s) const;

    /// Lookup the index for the named parameter s
    int find_parameter(const std::string& s) const;

    /*----- Modifiable regs ----*/
private:
    std::optional<int> find_update_modifiable_reg(int& R);
public:
    std::optional<int> find_modifiable_reg(int R);

    std::optional<int> parameter_is_modifiable_reg(int index);

    std::optional<int> compute_expression_is_modifiable_reg(int index);

    /*----- Random variables ----*/
private:
    std::optional<int> find_update_random_variable(int& R);
public:
    std::optional<int> find_random_variable(int R);

    std::optional<int> parameter_is_random_variable(int index);

    std::optional<int> compute_expression_is_random_variable(int index);

    const expression_ref get_parameter_range(int c, int r);

    const expression_ref get_range_for_random_variable(int c, int r);

    double get_rate_for_random_variable(int c, int r);

    /*----- Token manager ------*/

    struct Token
    {
	// The context this context is derived from.
	int parent = -1;
	// Contexts that are derived from this one.
	std::vector<int> children;

	// Contexts that point here
	int n_context_refs = 0;
	bool is_referenced() const {assert(n_context_refs >= 0); return n_context_refs > 0;}
    
	/// Mapping from closures to steps/results
	const auto& delta_result() const { return vm_result.delta(); }
	const auto& delta_step() const {return vm_step.delta(); }

	mapping<int> vm_result;
	mapping<int> vm_step;

	bool used = false;
	long version = 0;
    };
private:
    int root_token = -1;

    /// The roots for each token
    std::vector<Token> tokens;

    /// The list of unused_tokens
    std::vector<int> unused_tokens;

    std::vector<int> prog_steps;
    std::vector<int> prog_results;
    mutable std::vector<std::bitset<8>> prog_temp;
  
public:
    /// Is a particular token unused?
    bool token_is_used(int) const;

    void check_tokens() const;

    /// How many tokens are there, maximum?
    int get_n_tokens() const {return tokens.size();}

    const Token& get_token(int t) const {return tokens[t];}

    /// Acquire an unused token
    int get_unused_token();

    /// Release the token, and also the parent if its an unreferenced knuckle
    int release_unreferenced_tips(int);

    /// Release the tip token
    void release_tip_token(int);

    /// Merge the knuckle token and release it
    int release_knuckle_tokens(int);

    void capture_parent_token(int);

    bool is_terminal_token(int) const;

    bool is_root_token(int) const;

    int parent_token(int) const;

    const std::vector<int>& children_of_token(int) const;

    int degree_of_token(int) const;

    /// Make the context point to a token
    void switch_to_token(int c, int t);

    /// Make the context point to the token of another context
    void switch_to_context(int c1, int c2);

    /// Acquire a copy of a token
    int make_child_token(int);

    /// Make a child of the current token, point to the child, and unreference the current token.
    int switch_to_child_token(int c);

    void merge_split_mappings(const std::vector<int>&);

    /*------------- Stuff for context indices -----------*/
    std::vector<int> unused_contexts;

    std::vector<int> token_for_context_;

    int get_n_contexts() const;

    int unset_token_for_context(int c);

    void set_token_for_context(int c, int t);

    int copy_context(int);

    int get_new_context();

    int get_unused_context();

    void release_context(int);

    int token_for_context(int) const;

    const expression_ref& get_parameter_value_in_context(int R, int c);

    const expression_ref& get_reg_value_in_context(int& R, int c);

    void set_reg_value_in_context(int index, closure&& C, int c);

    int get_modifiable_value_in_context(int R, int c);

    /*------------------------------------------------*/

    const std::vector<int>& get_temp_heads() const {return temp;}

    const std::vector<int>& get_heads() const {return heads;}

    const std::vector<std::pair<std::string,int>>& get_parameters() const {return parameters;}
    std::vector<std::pair<std::string,int>>& get_parameters()       {return parameters;}

    const std::map<std::string,int>& get_identifiers() const {return identifiers;}
    std::map<std::string,int>& get_identifiers()       {return identifiers;}

    /// Make heads[index] point to r
    int set_head(int index, int r);
    int set_head(int index, closure&& C);

    /// Allocate a reg in context t and put it on the top of the head stack.
    int allocate_head(closure&& C);
    int add_compute_expression(const expression_ref&);

    /// Allocate a reg in context t and put it on the top of the temporary-head stack.
    int push_temp_head();
    int push_temp_head(closure&& C);

    /// Deallocate the temporary head on the top of the temporary-head stack.
    void pop_temp_head();

    /// Add a new identifier, pointing to a newly allocated location
    int add_identifier(const std::string&);

    /// Add a new parameter, which is a modifiable.
    int add_modifiable_parameter(const std::string&);

    /// Add a new parameter, which is a modifiable.
    int add_parameter(const std::string&, const expression_ref&);

    /// Add a new parameter pointing to reg r.
    void add_parameter(const std::string&, int r);

    /*----- Graph walking ------*/
    void find_all_regs_in_context(int, bool, std::vector<int>&) const;
    void find_all_used_regs_in_context(int, bool, std::vector<int>&) const;

    void find_all_regs_in_context_no_check(int, bool, std::vector<int>&) const;
    void find_all_regs_in_context_no_check(int, std::vector<int>&,std::vector<int>&) const;

    std::vector<int> find_all_regs_in_context(int, bool) const;
    std::vector<int> find_all_used_regs_in_context(int, bool) const;

    std::vector<int> find_all_regs_in_context_no_check(int, bool) const;

    /*----- Virtual memory ------*/
    int step_index_for_reg(int r) const;
    int result_index_for_reg(int r) const;
    const Step& step_for_reg(int r) const ;
    Step& step_for_reg(int r);
    const Result& result_for_reg(int r) const ;
    Result& result_for_reg(int r);
    bool has_step(int r) const;
    bool has_result(int r) const;
    bool reg_has_value(int r) const;
    bool reg_has_result_value(int r) const;
    int result_value_for_reg(int r) const;
    bool reg_has_call(int r) const;
    int call_for_reg(int r) const;

    const closure& access_value_for_reg(int R1) const;

    int remove_shared_result(int t, int r);

    int get_shared_step(int r);
    int get_shared_result(int r, int step);

    int add_shared_step(int r);
    int add_shared_result(int r, int step);

    void clear_step(int r);
    void clear_result(int r);

    void clear_back_edges_for_reg(int r);
    void clear_back_edges_for_step(int s);
    void clear_back_edges_for_result(int rc);

    void check_back_edges_cleared_for_step(int rc);
    void check_back_edges_cleared_for_result(int rc);

    int value_for_reg(int r) const;

    void set_result_value_for_reg(int r1);

    std::vector<int> used_regs_for_reg(int r) const;
    std::vector<int> forced_regs_for_reg(int r) const;

    void reroot_at(int t);

    void reroot_at_context(int c);

    /*----- Graph splitting -----*/
    /// Update the value of a non-constant, non-computed index
    void set_reg_value(int index, closure&&, int t);
    void set_shared_value(int r, int v);
    bool reg_is_shared(int t, int r) const;
    void unshare_regs(int t1);
    void mark_completely_dirty(int t);
    bool is_completely_dirty(int t) const;
    bool is_dirty(int t) const;

    std::pair<int,int> incremental_evaluate_in_context(int R, int c);

    std::pair<int,int> incremental_evaluate(int R);
private:  
    std::pair<int,int> incremental_evaluate_(int R);
    int incremental_evaluate_unchangeable_(int R);
    void incremental_evaluate_from_call(int S, closure&);
    void incremental_evaluate_from_call_(int S);

public:
  
    int incremental_evaluate_unchangeable(int R);

    const closure& lazy_evaluate(int& R);

    const closure& lazy_evaluate(int& R, int c);

    const closure& lazy_evaluate_head(int h, int c);

    const closure& lazy_evaluate_unchangeable(int& R);

    std::vector<int>& get_scratch_list() const;
    void release_scratch_list() const;

    reg_heap(const std::shared_ptr<module_loader>&);
};

expression_ref graph_normalize(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

void dot_graph_for_token(const reg_heap& C, int t);

void write_token_graph(const reg_heap& C, std::ostream& o);

void write_token_graph(const reg_heap& C);

struct no_context: public std::exception
{
};

extern long total_reductions;
extern long total_reg_allocations;
extern long total_comp_allocations;
extern long total_reroot;
extern long total_tokens;

#endif
