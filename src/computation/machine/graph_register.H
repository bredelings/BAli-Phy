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

    enum class type_t {unevaluated=0, index_var=1, constant=2, changeable=3};

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

    std::pair<int, int> created_by;

    bitmask_8 flags;

    void clear();

    void check_cleared() const;

    reg& operator=(const reg&) = delete;
    reg& operator=(reg&&) noexcept;

    reg() = default;

    reg(const reg&) = delete;
    reg(reg&&) noexcept;
};

// These bits are used in evaluation as well as unsharing.

constexpr int unshare_force_bit  = 0;
constexpr int unshare_result_bit = 1;
constexpr int unshare_step_bit   = 2;

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

    std::vector<std::function<void(const effect&, int)>> register_likelihood_handlers;
    std::vector<std::function<void(const effect&, int)>> unregister_likelihood_handlers;
    std::vector<std::function<void(const effect&, int)>> register_prior_handlers;
    std::vector<std::function<void(const effect&, int)>> unregister_prior_handlers;

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
    bool reg_is_constant(int r) const;
    void mark_reg_constant(int r);
    bool reg_is_changeable(int r) const;
    void mark_reg_changeable(int r);
    bool reg_is_index_var(int r) const;
    void mark_reg_index_var(int r);

    void set_used_reg(int r1, int r2);
    void set_forced_reg(int r1, int r2);
    void set_call(int s, int r2);
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

    // a list of closures under evaluation
    std::vector<closure> closure_stack;

    /* ---- */
private:
    void do_pending_effect_registrations();

    void do_pending_effect_unregistrations();

    void mark_effect_to_register_at_step(int s);

    void unmark_effect_to_register_at_step(int s);

    void mark_effect_to_unregister_at_step(int s);

    void unmark_effect_to_unregister_at_step(int s);

    void register_effect_at_step(int s);

    void unregister_effect_at_step(int s);

    void first_evaluate_program(int c);

    bool simple_set_path_to(int child_token) const;

    std::vector<std::pair<int,closure>> find_set_regs_on_path(int child_token) const;

    expression_ref unshare_and_evaluate_program(int c);

    int force_count(int r) const;

    int unmap_unforced_steps(int );

public:
    expression_ref evaluate_program(int c);

    /* ---- */

    log_double_t prior_for_context(int c);

    std::unordered_map<int,int> likelihood_heads;

    void register_likelihood_(const effect& e, int s);

    void unregister_likelihood_(const effect& e, int s);

    log_double_t likelihood_for_context(int c);

    log_double_t probability_for_context(int c);

    prob_ratios_t probability_ratios(int c1, int c2);

    // the list of random variables
    std::unordered_map<int,int> random_variables;

    void register_random_variable(const effect& e, int s);
    void unregister_random_variable(const effect& e, int s);

    // the list of transition kernels
    std::vector<std::pair<int,int>> transition_kernels_;

    const std::vector<std::pair<int,int>>& transition_kernels() const;

    void register_transition_kernel(int,int,int);
    void unregister_transition_kernel(int,int);

    void mark_transition_kernel_active(int r);
    bool transition_kernel_is_active(int r);
    void clear_transition_kernel_active(int r);

    /// Probably this would be non-parameter identifiers
    std::map<std::string, int> identifiers;
  
    std::shared_ptr<Program> program;

    std::vector<std::string> args;

    /// Get the memory location (reg) for a given identifier (string)
    int reg_for_id(const std::string&);
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
        const auto& delta_force() const { return vm_force.delta(); }
        const auto& delta_result() const { return vm_result.delta(); }
        const auto& delta_step() const {return vm_step.delta(); }

        mapping<int> vm_result;
        mapping<int> vm_step;
        mapping<char> vm_force;
        mapping<int> vm_force_count;

        boost::container::small_vector< int, 2> prev_prog_active_refs;
        boost::container::small_vector< int, 2> prev_prog_inactive_refs;
        std::optional<prev_prog_token_t> prev_prog_token;

        token_type type = token_type::none;
        bool used = false;
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
    std::vector<char> prog_forces;
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

    void set_reg_value_in_context(int index, closure&& C, int c);

    int get_modifiable_value_in_context(int R, int c);

    /*------------------------------------------------*/

    const std::vector<int>& get_temp_heads() const {return temp;}

    const std::vector<int>& get_heads() const {return heads;}

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
    bool has_force1(int r) const;
    bool has_force2(int r) const;
    int result_for_reg(int r) const;
    bool reg_has_call(int r) const;
    int call_for_reg(int r) const;
    void force_reg1(int r);
    void force_reg2(int r);

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
    void set_reg_value(int index, closure&&, int t);
    bool reg_is_shared(int t, int r) const;
    void check_created_regs_unshared(int t1);
    void unshare_regs1(int t1);
    void unshare_regs2(int t1);
    void check_unshare_regs(int t);
    void maybe_unshare_regs(int t1);
    bool execution_allowed() const;

    std::pair<int,int> incremental_evaluate_in_context(int R, int c, bool reforce = false);

    std::pair<int,int> incremental_evaluate1(int R, bool reforce);

    std::pair<int,int> incremental_evaluate2(int R, bool reforce);

    std::optional<int> precomputed_value_in_context(int R, int c);

private:  
    std::pair<int,int> incremental_evaluate1_(int R, bool reforce);
    std::pair<int,int> incremental_evaluate2_(int R, bool reforce);
    int incremental_evaluate_unchangeable_(int R);

public:
  
    int incremental_evaluate_unchangeable(int R);

    const closure& lazy_evaluate1(int& R, bool reforce = false);

    const closure& lazy_evaluate2(int& R, bool reforce = false);

    const closure& lazy_evaluate(int& R, int c, bool reforce = false);

    const closure& lazy_evaluate_head(int h, int c, bool reforce = false);

    const closure& lazy_evaluate_unchangeable(int& R);

    std::vector<int>& get_scratch_list() const;
    void release_scratch_list() const;

    std::vector<std::pair<int,int>>& get_pair_scratch_list() const;
    void release_pair_scratch_list() const;

    reg_heap(const Program& P);
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

/// Inlined functions

inline bool reg_heap::reg_is_unevaluated(int r) const
{
    return regs.access(r).type == reg::type_t::unevaluated;
}

inline void reg_heap::mark_reg_unevaluated(int r)
{
    regs[r].type = reg::type_t::unevaluated;
}

inline bool reg_heap::reg_is_constant(int r) const
{
    return regs.access(r).type == reg::type_t::constant;
}

inline void reg_heap::mark_reg_constant(int r)
{
    assert( reg_is_constant(r) or reg_is_unevaluated(r));

    regs[r].type = reg::type_t::constant;
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

inline bool reg_heap::reg_is_index_var(int r) const
{
    return regs.access(r).type == reg::type_t::index_var;
}

inline void reg_heap::mark_reg_index_var(int r)
{
    assert( reg_is_index_var(r) or reg_is_unevaluated(r));

    regs.access(r).type = reg::type_t::index_var;
}

inline bool reg_heap::has_force1(int r) const
{
    assert(prog_forces[r] != 0);
    return prog_forces[r] > 0;
}

inline bool reg_heap::has_force2(int r) const
{
    assert(prog_forces[r] != 0);
    return prog_forces[r] > 0;
}

#endif
