#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H

#include <string>
#include <vector>
#include <unordered_set>
#include <unordered_map>
#include "computation/object.H"
#include "computation/expression/expression_ref.H"
#include <utility>
#include "computation/closure.H"
#include "pool.H"
#include "computation/program.H"
#include <boost/container/small_vector.hpp>
#include "computation/loader.H"
#include "util/assert.hh"
#include "util/bitmask.H"
#include "kahan.H"
#include "mcmc/prob_ratios.H"
#include "mapping.H"
#include "effects.H"

struct prev_prog_token_t
{
    int token;
    std::optional<int> index;
    bool can_revert;
    prev_prog_token_t(int i, std::optional<int> oi, bool b):token(i),index(oi),can_revert(b) {}
};

struct Step
{
    int source_reg = -1;

    int call = 0;

    // NOTE: There is a space-time trade-off in the number of used_regs here.
    //       However, small_vector< ,1> takes little or no extra space, and noticeably saves time.

    /// Does C reduce to another reg that we need to evaluate to get the true value?
    std::pair<int,int> call_edge;

    boost::container::small_vector< int, 2 > created_regs;

    bitmask_8 flags;

    // Bit 6 for being an effect step.

    bool has_effect() const;

    void mark_with_effect();

    // Bit 5 for being on the pending-registration list.

    bool has_pending_effect_registration() const;

    void set_pending_effect_registration();

    void clear_pending_effect_registration();

    // Bit 4 for being on the pending-unregistration list.

    bool has_pending_effect_unregistration() const;

    void set_pending_effect_unregistration();

    void clear_pending_effect_unregistration();

    // End bits

    void clear();

    void check_cleared() const;

    Step& operator=(const Step&) = delete;
    Step& operator=(Step&&) noexcept;

    Step() = default;

    Step(const Step&) = delete;
    Step(Step&&) noexcept;
};

// Bits in on regs[r].flags
constexpr int reg_is_on_stack_bit = 3;
constexpr int reg_is_unforgettable_bit = 4;
constexpr int reg_is_always_evaluated_bit = 6;

constexpr int non_computed_index = -1;

constexpr int non_existant_index = -2;

enum class token_type {none,root,merged,set,set_unshare,execute,execute2,reverse_set,reverse_set_unshare,reverse_execute,unmap,reverse_unmap,reverse_execute2};

token_type reverse(token_type);

std::ostream& operator<<(std::ostream& o, token_type);

class effect;

class reg
{
public:
    /* The closure */
    closure C;

    enum class type_t
    {
        unevaluated=0,
        index_var_no_force=1,
        constant_no_force=2,
        changeable=3,
        index_var_with_force_to_nonchangeable=4,
        index_var_with_force_to_changeable=5,
        constant_with_force=6
    };

    type_t type = type_t::unevaluated;

    /// Which reg's were used to reduce this expression?
    boost::container::small_vector< std::pair<int,int>, 2 > used_regs;

    boost::container::small_vector< std::pair<int,int>, 2 > forced_regs;

    /// Which regs (reg,index) USE this reg?
    boost::container::small_vector<std::pair<int,int>, 2> used_by;

    /// Which regs (reg,index) FORCE this reg?
    boost::container::small_vector<std::pair<int,int>, 2> forced_by;

    /// Which reduction values made use of the value of this expression (via call)
    boost::container::small_vector<int, 5> called_by;

    /// Which reduction values made use of the value of this expression (via call)
    boost::container::small_vector<int, 5> called_by_index_vars;

    std::pair<int, int> created_by;

    std::pair<int, int> index_var_ref;

    bitmask_8 flags;

    void mark_unconditionally_evaluated();
    bool is_unconditionally_evaluated() const;

    void clear();

    void check_cleared() const;

    reg& operator=(const reg&) = delete;
    reg& operator=(reg&&) noexcept;

    reg() = default;

    reg(const reg&) = delete;
    reg(reg&&) noexcept;
};

struct set_op
{
    int reg;
    closure value;
};

struct exchange_op
{
    int r1;
    int r2;
};

typedef std::variant<set_op, exchange_op> set_exchange_op;

// These bits are used in evaluation as well as unsharing.

constexpr int unshare_result_bit = 1;
constexpr int unshare_step_bit   = 2;
constexpr int unshare_count_bit  = 3;
constexpr int call_decremented_bit = 4;
// This bit can only be set if unshare_result_bit was originally set.
// If we don't follow this rule, it could be set on regs not in unshared_regs,
//   and then it might not get cleared.
// It is only tested in one place - force_regs_check_same_inputs( ).
constexpr int different_result_bit = 5;
constexpr int initially_unforced_bit = 6;

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

class reg_heap: public Object
{
    pool<reg> regs;

    mutable pool<Step> steps;

    mutable int n_active_scratch_lists = 0;
    mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

    mutable int n_active_pair_scratch_lists = 0;
    mutable std::vector< object_ptr<Vector<std::pair<int,int>> > > pair_scratch_lists;

    std::vector<std::function<void(int, int)>> register_dist_handlers;
    std::vector<std::function<void(int, int)>> unregister_dist_handlers;

    std::vector<std::function<void(const register_prob&, int)>> register_likelihood_handlers;
    std::vector<std::function<void(const register_prob&, int)>> unregister_likelihood_handlers;

    std::vector<std::function<void(const register_prob&, int)>> register_prior_handlers;
    std::vector<std::function<void(const register_prob&, int)>> unregister_prior_handlers;
public:
    std::vector<std::function<void(int, int)>>   register_tk_handlers;
    std::vector<std::function<void(int, int)>> unregister_tk_handlers;

private:
    void resize(int s);

    void reclaim_used(int);

    bool reg_is_called_by(int, int) const;
    bool reg_is_used_by(int, int) const;
    bool reg_is_forced_by(int, int) const;

    void allocate_identifiers_for_program();

public:
    const closure& closure_at(int r) const {assert(r>0); return regs.access(r).C;}
    const expression_ref& expression_at(int r) const {return closure_at(r).exp;}
    const closure& operator[](int r) const {return closure_at(r);}

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
    bool reg_is_unevaluated(int r) const;
    void mark_reg_unevaluated(int r);
    bool reg_is_constant_no_force(int r) const;
    void mark_reg_constant_no_force(int r);
    bool reg_is_changeable(int r) const;
    void mark_reg_changeable(int r);
    bool unevaluated_reg_is_index_var_no_force(int r) const;
    bool reg_is_index_var_no_force(int r) const;
    void mark_reg_index_var_no_force(int r);
    bool reg_is_index_var_with_force_to_nonchangeable(int r) const;
    void mark_reg_index_var_with_force_to_nonchangeable(int r);
    bool reg_is_index_var_with_force_to_changeable(int r) const;
    void mark_reg_index_var_with_force_to_changeable(int r);
    bool reg_is_index_var_with_force(int r) const;
    bool reg_is_constant_with_force(int r) const;
    void mark_reg_constant_with_force(int r);
    bool reg_is_constant(int r) const;
    bool reg_is_changeable_or_forcing(int r) const;
    bool reg_is_to_changeable(int r) const;
    bool reg_has_forces(int r) const;
    std::optional<int> reg_has_single_force(int r) const;
    bool reg_is_on_stack(int r) const;
    bool regs_maybe_different_value(int r1, int r2) const;
    bool reg_is_unforgettable(int r) const;
    bool reg_is_forgettable(int r) const;
    void mark_reg_unforgettable(int r);

    void set_used_reg(int r1, int r2);
    int set_forced_reg(int r1, int r2);
    void set_call(int s, int r2, bool = false);
    void set_index_var_ref(int r1, int r2);
    void destroy_step_and_created_regs(int s);
    void destroy_all_computations_in_token(int t);
    void clear_call(int s);
    void clear_call_for_reg(int R);
    void clear_C(int R);
    void set_C(int R, closure&& C);
    int allocate();
    int allocate_reg_from_step(int s);
    int allocate_reg_from_step(int s, closure&& C);
    void mark_reg_created_by_step(int r, int s);
    void mark_step_with_effect(int s);

    void get_roots(std::vector<int>&, bool keep_identifiers=true) const;

    // the list of expressions that we are temporarily evaluating
    std::vector<int> temp;
  
    // the list of expressions that we are interested in evaluating.
    std::vector<int> heads;

    std::map<std::string,int> named_heads;

    int add_named_head(const std::string&, int);
    std::optional<int> lookup_named_head(const std::string&);

    std::optional<int> perform_io_head;
    std::optional<int> program_result_head;
    std::optional<int> logging_head;

    int add_perform_io_head();
    int add_program(const expression_ref& E);

    // the list of regs that are currently being evaluated
    std::vector<int> stack;

    void stack_push(int r);
    void stack_pop(int r);
    int stack_pop();

    std::set<int> find_affected_sampling_events(const std::function<void()>& do_changes);

private:
    void do_pending_effect_registrations();

    void do_pending_effect_unregistrations();

    void mark_effect_to_register_at_step(int s);

    void unmark_effect_to_register_at_step(int s);

    void mark_effect_to_unregister_at_step(int s);

    void unmark_effect_to_unregister_at_step(int s);

    void register_effect_at_step(int s);

    void unregister_effect_at_step(int s);

    void note_step_not_in_root(int s);

    void note_step_in_root(int s);

    friend class RegOperationArgs1;
    friend class RegOperationArgs2Changeable;
    friend class RegOperationArgs2Unevaluated;

    void _register_effect_at_reg(int r, int s);

    void _unregister_effect_at_reg(int r, int s);

public:
    bool step_has_effect(int s) const;

    const closure& get_effect(int s) const;

private:
    void compute_initial_force_counts();
    void mark_unconditional_regs();
    void first_evaluate_program(int c);

    bool simple_set_path_to(int child_token) const;

    std::vector<set_exchange_op> find_set_regs_on_path(int child_token) const;

    int force_simple_set_path_to_PPET(int c);

    expression_ref unshare_and_evaluate_program(int c);

    int force_count(int r) const;

public:
    expression_ref evaluate_program(int c);

    /* ---- */

    log_double_t prior_for_context(int c);

    log_double_t likelihood_for_context(int c);

    log_double_t probability_for_context(int c);

    prob_ratios_t probability_ratios(int c1, int c2);

    // the likelihood term
    std::unordered_map<int, register_prob> likelihood_terms;

    void register_likelihood_(const register_prob& E, int s);
    void unregister_likelihood_(const register_prob& E, int s);

    // the prior terms
    std::unordered_map<int, register_prob> prior_terms;

    void   register_prior(const register_prob& e, int s);
    void unregister_prior(const register_prob& e, int s);

    // the list of transition kernels
    std::unordered_set<int> transition_kernels_;

    const std::unordered_set<int>& transition_kernels() const;

    void   register_transition_kernel(int r, int s);
    void unregister_transition_kernel(int r, int s);

    // Since there is only one out-edges, should we remove this?
    // No, if we observe the same data twice, we still need separate sampling events.
    std::map<int, std::string> dist_type;

    // For each distribution, the argument name should be unique.
    std::map<int, std::map<std::string, int>> in_edges_to_dist;
    // NOTE: no indexing by node (->dist) because node is lazily evaluated.

    // FIXME: Use boost::multi-index?
    std::map<int, int> out_edges_from_dist;
    std::map<int, std::set<int>> out_edges_to_var;
    std::map<int, std::map<std::string, int>> dist_properties;

    void register_in_edge(int r, int s);
    void unregister_in_edge(int r, int s);

    void register_out_edge(int r, int s);
    void unregister_out_edge(int r, int s);

    void   register_dist(int r, int s);
    void unregister_dist(int r, int s);

    void register_dist_property(int r, int s);
    void unregister_dist_property(int r, int s);

    /// Probably this would be non-parameter identifiers
    std::map<var, int> identifiers;
  
    std::shared_ptr<Program> program;

    FreshVarState fresh_var_state;

    std::vector<std::string> args;

    // FIXME: maybe this should really take a FastString = (String, Int)
    /// Get the memory location (reg) for a given identifier (string)
    int reg_for_id(const var&);
    /// Translate named identifiers (struct var) into reg_var references
    expression_ref translate_refs(const expression_ref& R, closure::Env_t&);
    /// Translate named identifiers (struct var) into reg_var references
    closure translate_refs(closure&& C);

    closure preprocess(const closure& C);

    /*----- Modifiable regs ----*/
private:
    std::optional<int> find_update_modifiable_reg(int& R);

public:
    std::optional<int> find_modifiable_reg(int R);

    std::optional<int> find_precomputed_const_or_modifiable_reg(int r);

    std::optional<int> find_precomputed_modifiable_reg_in_context(int R, int c);

    std::optional<int> find_precomputed_exchangeable_reg(int R);

    std::optional<int> find_precomputed_exchangeable_reg_in_context(int R, int c);

    std::optional<int> find_modifiable_reg_in_context(int R, int c);

    std::optional<int> compute_expression_is_modifiable_reg(int index);

    /*----- Random variables ----*/
public:

    /*----- Token manager ------*/

    struct Token
    {
        // The context this context is derived from.
        int parent = -1;
        // Contexts that are derived from this one.
        std::vector<int> children;

        // Contexts that point here
        int n_context_refs = 0;
        bool is_referenced() const {assert(n_context_refs >= 0); return n_context_refs > 0 or not prev_prog_active_refs.empty();}
    
        /// Mapping from closures to steps/results
        const auto& delta_force_count() const { return vm_force_count.delta(); }
        const auto& delta_result() const { return vm_result.delta(); }
        const auto& delta_step() const {return vm_step.delta(); }

        mapping vm_result;
        mapping vm_step;
        mapping vm_force_count;

        std::vector<exchange_op> exchanges;

        boost::container::small_vector< int, 2> prev_prog_active_refs;
        boost::container::small_vector< int, 2> prev_prog_inactive_refs;
        std::optional<prev_prog_token_t> prev_prog_token;

        token_type type = token_type::none;
        bool used = false;
        bitmask_8 flags;
    };

    void set_prev_prog_token(int t, std::optional<prev_prog_token_t> prev_prog_token);
    std::optional<int> unset_prev_prog_token(int t);
    std::optional<int> get_prev_prog_token_for_token(int t) const;
    std::optional<int> get_prev_prog_token_for_context(int c) const;
    bool is_program_execution_token(int) const;

private:
    int root_token = -1;

    /// The roots for each token
    std::vector<Token> tokens;

    /// The list of unused_tokens
    std::vector<int> unused_tokens;

    std::vector<int> prog_steps;
    std::vector<int> prog_results;
    std::vector<int> prog_force_counts;
    mutable std::vector<bitmask_8> prog_temp;
    mutable std::vector<bitmask_8> prog_unshare;

    std::unordered_set<int> steps_pending_effect_registration;
    std::unordered_set<int> steps_pending_effect_unregistration;

public:

    /// Is a particular token unused?
    bool token_is_used(int) const;

    void check_tokens() const;

    /// How many tokens are there, maximum?
    int get_n_tokens() const {return tokens.size();}

    const Token& get_token(int t) const {return tokens[t];}

    /// Acquire an unused token
    int get_unused_token(token_type type, std::optional<int> prev);

    /// Report all used tokens
    std::vector<int> get_used_tokens() const;

    /// Releases the token, and also the parent if its an unreferenced knuckle
    int release_unreferenced_tips(int);

    /// Release the tip token
    void release_tip_token(int);

    /// Merge the knuckle token and release it
    int release_knuckle_tokens(int);

    void capture_parent_token(int);

    bool is_terminal_token(int) const;

    bool is_root_token(int) const;

    int get_root_token() const;

    int parent_token(int) const;

    const std::vector<int>& children_of_token(int) const;

    int degree_of_token(int) const;

    /// Make the context point to a token
    void switch_to_token(int c, int t);

    /// Acquire a copy of a token
    int make_child_token(int c, token_type type);

    /// Make a child of the current token, point to the child, and unreference the current token.
    int switch_to_child_token(int c, token_type type);

    void merge_split_mappings(const std::vector<int>&);

    /*------------- Stuff for context indices -----------*/
    std::vector<int> unused_contexts;

    std::vector<int> token_for_context_;

    int get_n_contexts() const;

private:
    std::pair<int,std::optional<int>> unset_token_for_context_no_release_tips_(int c);

    void set_token_for_unset_context_(int c, int t);

    // This is the safe interface that allows resetting things and releases unreferenced tips.
    void set_token_for_context(int c, std::optional<int> t);

    int get_new_context();

public:
    int copy_context(int);

    /// Make the context point to the token of another context
    void switch_to_context(int c1, int c2);

    int get_first_context();

    void release_context(int);

    int token_for_context(int) const;

    const expression_ref& get_reg_value_in_context(int& R, int c);

private:
    void exchange_regs_in_context_(int r1, int r2, int c);

public:
    void exchange_regs_in_context(int r1, int r2, int c);

    void set_reg_value_in_context(int index, closure&& C, int c);

    int get_modifiable_value_in_context(int R, int c);

    /*------------------------------------------------*/

    const std::vector<int>& get_temp_heads() const {return temp;}

    const std::vector<int>& get_heads() const {return heads;}

    const std::map<var,int>& get_identifiers() const {return identifiers;}
    std::map<var,int>& get_identifiers()       {return identifiers;}

    /// Find out which reg the head `index` points to.
    int reg_for_head(int index) const;

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
    int add_identifier(const var&);

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
    const Step& step_for_reg(int r) const ;
    Step& step_for_reg(int r);
    bool has_step1(int r) const;
    bool has_step2(int r) const;
    bool has_result1(int r) const;
    bool has_result2(int r) const;
    bool reg_has_value(int r) const;
    int follow_index_var(int r) const;
    int follow_index_var_no_force(int r) const;
    bool reg_is_forced(int r) const;
    int inc_count(int r);
    int dec_count(int r);
    int result_for_reg(int r) const;
    bool reg_has_call(int r) const;
    int call_for_reg(int r) const;
    bool force_regs_check_same_inputs(int r);
    void force_reg_no_call(int r);
    void force_reg_with_call(int r);

    const closure& access_value_for_reg(int R1) const;

    const closure& value_for_precomputed_reg(int r) const;

    int remove_shared_result(int t, int r);

    int get_shared_step(int r);

    int add_shared_step(int r);

    void clear_step(int r);
    void clear_result(int r);

    void clear_back_edges_for_reg(int r, bool creator_survives=true);
    void clear_back_edges_for_step(int s);

    void check_back_edges_cleared_for_reg(int r) const;
    void check_back_edges_cleared_for_step(int s) const;

    int value_for_reg(int r) const;

    void set_result_for_reg(int r1);

    std::vector<int> used_regs_for_reg(int r) const;
    std::vector<int> forced_regs_for_reg(int r) const;

    void reroot_at(int t);

    void reroot_at_token(int t);
    void reroot_at_context(int c);

    /*----- Graph splitting -----*/
    /// Update the value of a non-constant, non-computed index
    void exchange_regs(int r1, int r2, int t);
    void set_reg_value(int index, closure&&, int t);
    bool reg_is_shared(int t, int r) const;
    void check_created_regs_unshared(int t1);
    void unshare_regs1(int t1);

    void find_unshared_regs(std::vector<int>&, std::vector<int>&, int t);
    void tweak_deltas_and_unshare_bits(int t);
    void reroot_at_token_with_tweaked_deltas_and_bits(int t);
    void increment_counts_from_new_calls();
    void evaluate_unconditional_regs(const std::vector<int>& unshared_regs);
    void decrement_counts_from_invalid_calls(const std::vector<int>&, std::vector<int>&);
    void evaluate_forced_invalid_regs(const std::vector<int>&);
    void cleanup_count_deltas_and_bits();
    void remove_zero_count_regs(const std::vector<int>&, const std::vector<int>&);
    void unregister_effects_for_bumped_steps();
    void mark_as_program_execution_token(int t);
    void check_force_counts();
    expression_ref unshare_regs2(int t1);

    void check_unshare_regs(int t);
    void maybe_unshare_regs(int t1);
    bool execution_allowed() const;

    std::pair<int,int> incremental_evaluate_in_context(int R, int c);

    std::pair<int,int> incremental_evaluate1(int R);

    std::pair<int,int> incremental_evaluate2(int R, bool);

    std::optional<int> precomputed_value_in_context(int R, int c);

private:  
    std::pair<int,int> incremental_evaluate1_(int R);
    std::pair<int,int> incremental_evaluate2_(int R);
    std::pair<int,int> incremental_evaluate2_index_var_with_force_(int R);
    std::pair<int,int> incremental_evaluate2_changeable_(int R);
    std::pair<int,int> incremental_evaluate2_unevaluated_(int R);
    int incremental_evaluate_unchangeable_(int R);

public:
  
    int incremental_evaluate_unchangeable(int R);

    const closure& lazy_evaluate1(int& R);

    const closure& lazy_evaluate2(int& R);

    const closure& lazy_evaluate(int& R, int c);

    const closure& lazy_evaluate_head(int h, int c);

    const closure& lazy_evaluate_unchangeable(int& R);

    expression_ref maybe_modifiable_structure(int r);

    std::vector<int>& get_scratch_list() const;
    void release_scratch_list() const;

    std::vector<std::pair<int,int>>& get_pair_scratch_list() const;
    void release_pair_scratch_list() const;

    reg_heap(const Program& P);
};

expression_ref graph_normalize(FreshVarState&, const expression_ref& R);

void write_dot_graph(const reg_heap& C, std::ostream& o);

void write_dot_graph(const reg_heap& C);


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

/// Inlined functions

inline bool reg_heap::reg_is_unevaluated(int r) const
{
    return regs.access(r).type == reg::type_t::unevaluated;
}

inline bool reg_heap::reg_has_forces(int r) const
{
    return not regs[r].forced_regs.empty();
}

inline bool reg_heap::reg_is_on_stack(int r) const
{
    return regs[r].flags.test(reg_is_on_stack_bit);
}

inline bool reg_heap::reg_is_unforgettable(int r) const
{
    return regs[r].flags.test(reg_is_unforgettable_bit);
}

inline bool reg_heap::reg_is_forgettable(int r) const
{
    return not reg_is_unforgettable(r);
}

inline void reg_heap::mark_reg_unforgettable(int r)
{
    assert(reg_is_changeable(r));
    regs[r].flags.set(reg_is_unforgettable_bit);
}

inline void reg_heap::mark_reg_unevaluated(int r)
{
    regs[r].type = reg::type_t::unevaluated;
}

inline bool reg_heap::reg_is_constant_no_force(int r) const
{
    return regs.access(r).type == reg::type_t::constant_no_force;
}

inline void reg_heap::mark_reg_constant_no_force(int r)
{
    assert( reg_is_constant_no_force(r) or reg_is_unevaluated(r));

    regs[r].type = reg::type_t::constant_no_force;
}

inline bool reg_heap::reg_is_changeable(int r) const
{
    return regs.access(r).type == reg::type_t::changeable;
}

inline void reg_heap::mark_reg_changeable(int r)
{
    assert( reg_is_changeable(r) or reg_is_unevaluated(r));

    regs.access(r).type = reg::type_t::changeable;
}

inline bool reg_heap::unevaluated_reg_is_index_var_no_force(int r) const
{
    return regs.access(r).type == reg::type_t::index_var_no_force;
}

inline bool reg_heap::reg_is_index_var_no_force(int r) const
{
    assert(not reg_is_unevaluated(r));
    return unevaluated_reg_is_index_var_no_force(r);
}

inline void reg_heap::mark_reg_index_var_no_force(int r)
{
    assert( unevaluated_reg_is_index_var_no_force(r) or reg_is_unevaluated(r));

    regs.access(r).type = reg::type_t::index_var_no_force;
}

inline bool reg_heap::reg_is_index_var_with_force_to_nonchangeable(int r) const
{
    return regs.access(r).type == reg::type_t::index_var_with_force_to_nonchangeable;
}

inline void reg_heap::mark_reg_index_var_with_force_to_nonchangeable(int r)
{
    assert( reg_is_index_var_with_force_to_nonchangeable(r) or reg_is_unevaluated(r));

    regs.access(r).type = reg::type_t::index_var_with_force_to_nonchangeable;
}

inline bool reg_heap::reg_is_index_var_with_force_to_changeable(int r) const
{
    return regs.access(r).type == reg::type_t::index_var_with_force_to_changeable;
}

inline void reg_heap::mark_reg_index_var_with_force_to_changeable(int r)
{
    assert( reg_is_index_var_with_force_to_changeable(r) or reg_is_unevaluated(r));

    regs.access(r).type = reg::type_t::index_var_with_force_to_changeable;
}

inline bool reg_heap::reg_is_index_var_with_force(int r) const
{
    return reg_is_index_var_with_force_to_changeable(r)
        or reg_is_index_var_with_force_to_nonchangeable(r);
}

inline bool reg_heap::reg_is_constant_with_force(int r) const
{
    return regs.access(r).type == reg::type_t::constant_with_force;
}

inline void reg_heap::mark_reg_constant_with_force(int r)
{
    assert( reg_is_constant_with_force(r) or reg_is_unevaluated(r));

    regs[r].type = reg::type_t::constant_with_force;
}

inline bool reg_heap::reg_is_constant(int r) const
{
    return reg_is_constant_no_force(r) or reg_is_constant_with_force(r);
}

inline bool reg_heap::reg_is_changeable_or_forcing(int r) const
{
    return regs.access(r).type >= reg::type_t::changeable;
}

inline bool reg_heap::reg_is_to_changeable(int r) const
{
    return reg_is_changeable(r) or reg_is_index_var_with_force_to_changeable(r);
}

inline bool reg_heap::reg_is_forced(int r) const
{
    assert(reg_is_changeable_or_forcing(r) or reg_is_unevaluated(r));
    return prog_force_counts[r] > 0;
}

inline void reg_heap::note_step_not_in_root(int s)
{
    if (steps[s].has_effect())
    {
        if (steps[s].has_pending_effect_registration())
            unmark_effect_to_register_at_step(s);
        else
            mark_effect_to_unregister_at_step(s);
    }
}

inline void reg_heap::note_step_in_root(int s)
{
    if (steps[s].has_effect())
        mark_effect_to_register_at_step(s);
}

#endif
