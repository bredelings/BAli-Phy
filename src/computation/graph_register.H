#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H

#include <string>
#include <vector>
#include "object.H"
#include "expression.H"
#include "util.H"
#include <utility>
#include "closure.H"
#include "pool.H"
#include "program.H"
#include <boost/shared_ptr.hpp>
#include <boost/container/small_vector.hpp>
#include <bitset>
#include "CacheList.H"

struct Step
{
  int source_reg = -1;

  int call = 0;

  /// Which reg's were used to reduce this expression?
  boost::container::small_vector< std::pair<int,CacheList<int>::iterator>, 10 > used_inputs;
  CacheList<int> created_regs;

  int temp = -1;

  std::bitset<64> flags;

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
  std::pair<int,CacheList<int>::iterator> call_edge;
  
  /// Which pairs (reg,slot) USED the reduction value of C (via an operation)?
  CacheList<int> used_by;

  /// Which reduction values made use of the value of this expression (via call)
  CacheList<int> called_by;

  int temp = -1;

  std::bitset<64> flags;

  void clear();

  void check_cleared();

  Result& operator=(const Result&) = delete;
  Result& operator=(Result&&) noexcept;

  Result() = default;

  Result(const Result&) = delete;
  Result(Result&&) noexcept;
};

class reg
{
public:
  /* The closure */
  closure C;

  /// Is C a parameter, or is the reduction value dependent on a parameter value?
  bool re_evaluate = false;

  enum class type_t {unknown=0, index_var=1, constant=2, changeable=3};

  type_t type = type_t::unknown;

  int n_heads = 0;

  std::pair<int, CacheList<int>::iterator> created_by;
  
  void clear();

  void check_cleared();

  reg& operator=(const reg&) = delete;
  reg& operator=(reg&&) noexcept;

  reg() = default;

  reg(const reg&) = delete;
  reg(reg&&) noexcept;
};

class mapping
{
  struct address
  {
    int value = 0;
    int index = -1;
    address() {}
    address(int i1):value(i1) {}
  };

  std::vector<int> modified_;
  std::vector<address> values;
public:
  mapping& operator=(mapping&) = default;

  mapping& operator=(mapping&& vm) noexcept
  {
    modified_ = std::move(vm.modified_);
    values = std::move(vm.values);
    return *this;
  }

  int operator[](int i) const {return values[i].value;}
  bool has_value(int r) const;
  void add_value(int r, int v);
  int replace_value(int r, int v);
  int erase_value(int r);
  int set_value(int r, int v);
  void clear();
  void resize(int s);
  int size() const;
  bool empty() const;
  const std::vector<int>& modified() const {return modified_;}

  mapping() {}
  mapping(int s) {resize(s);}
  mapping(const mapping&) = default;
  mapping(mapping&& vm) noexcept
  :modified_(std::move(vm.modified_)),
    values(std::move(vm.values))
  { }
};

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

class reg_heap: public Object, public checked_pool<reg>
{
  typedef checked_pool<reg> base_pool_t;

  mutable checked_pool_callback<Step> steps;

  mutable checked_pool_callback<Result> results;

  mutable int n_active_scratch_lists = 0;
  mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

  void get_more_memory();

  void expand_memory(int);

  void reclaim_used(int);

  bool result_is_called_by(int, int) const;
  bool result_is_used_by(int, int) const;
  bool reg_is_used_by(int, int) const;

  int inc_heads(int R);
  int dec_heads(int R);

public:

  reg_heap* clone() const {std::abort();}

  void check_used_reg(int) const;
  void check_used_regs() const;

  void collect_garbage();
  void trace(std::vector<int>& remap);
  void trace_and_reclaim_unreachable();
  bool reg_is_changeable(int r) const;
  bool reg_is_constant(int r) const;
  void make_reg_changeable(int r);

  void set_used_input(int R1, int R2);
  void set_call(int t, int R1, int R2);
  void set_call(int R1, int R2);
  void destroy_all_computations_in_token(int t);
  void clear_call(int s);
  void clear_call_for_reg(int R);
  void clear_C(int R);
  void set_C(int R, closure&& C);
  void set_reduction_value(int t, int R, closure&& C);
  int create_reg_from_step(int s);
  void mark_reg_created_by_step(int r, int s);

  void get_roots(std::vector<int>&, bool keep_identifiers=true) const;

  // the list of expressions that we are temporarily evaluating
  std::vector<int> temp;
  
  // the list of expressions that we are interested in evaluating.
  std::vector<int> heads;

  std::vector<int> probability_heads;

  // the list of regs that are currently being evaluated
  std::vector<int> stack;

  log_double_t constant_pr = 1.0;

  log_double_t variable_pr = 1.0;

  log_double_t unhandled_pr = 1.0;

  log_double_t error_pr = 1.0;

  double total_error = 0.0;

  std::vector<int> prs_list;

  void register_probability(int r);

  int register_probability(closure&& C);

  void dec_probability_for_reg(int r);

  bool inc_probability_for_reg(int r);

  void dec_probability(int rc);

  bool inc_probability(int rc);

  log_double_t probability_for_context_full(int c);

  log_double_t probability_for_context_diff(int c);

  log_double_t probability_for_context(int c);

  // the list of modifiables
  std::vector<int> random_modifiables_;
  
  const std::vector<int>& random_modifiables() const;

  int add_random_modifiable(int);

  std::vector<int> transition_kernels_;

  const std::vector<int>& transition_kernels() const;

  int add_transition_kernel(int);

  /// The register indices of the named parameters.
  std::vector<std::pair<std::string,int>> parameters;
  
  /// Probably this would be non-parameter identifiers
  std::map<std::string, int> identifiers;
  
  boost::shared_ptr<Program> P;

  module_loader loader;

  std::vector<std::string> args;

  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R, closure::Env_t&);
  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  closure translate_refs(closure&& C);

  closure preprocess(const closure& C);

  /// Lookup the index for the named parameter s
  int find_parameter(const std::string& s) const;

  /*----- Modifiable ranges ----*/
  bool parameter_is_modifiable(int index);

  int find_parameter_modifiable_reg(int i);

  const expression_ref get_parameter_range(int c, int r);

  const expression_ref get_range_for_reg(int c, int r);

  double get_rate_for_reg(int r);

  /*----- Token manager ------*/

  struct Token
  {
    // The context this context is derived from.
    int parent = -1;
    // Contexts that are derived from this one.
    std::vector<int> children;
    bool referenced = false;
    
    /// A list of signals that have been sent
    std::vector<int> triggers;

    /// Mapping from closures to steps/results
    mapping vm_result;
    mapping vm_step;

    /// These are the regs that must be evaluated when the context becomes active.
    std::vector<int> regs_to_re_evaluate;

    bool used = false;
    int version = 0;
  };
private:
  int root_token = -1;

  /// The roots for each token
  std::vector<Token> tokens;

  /// The list of unused_tokens
  std::vector<int> unused_tokens;

public:
  /// Is a particular token unused?
  bool token_is_used(int) const;

  void check_tokens() const;

  /// How many tokens are there, maximum?
  int get_n_tokens() const {return tokens.size();}

  /// Acquire an unused token
  int get_unused_token();

  /// Release the token if its unreferenced and has no children.
  void try_release_token(int);

  void release_child_token(int);

  void capture_parent_token(int);

  bool is_terminal_token(int) const;

  bool is_root_token(int) const;

  int parent_token(int) const;

  const std::vector<int>& children_of_token(int) const;

  int degree_of_token(int) const;

  /// Acquire a copy of a token
  int copy_token(int);

  /// Make a child of the current token, and unreference the current token.
  int switch_to_child_token(int);

  void merge_split_mapping(int, int);
  void merge_split_step_mapping(int, int);
  void merge_split_relative_mapping(int, int);

  void pivot_step_mapping(int, int);
  void pivot_relative_mapping(int, int);

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

        std::vector<int>& triggers_for_context(int c);

  bool reg_is_fully_up_to_date_in_context(int c, int r);

  bool reg_is_fully_up_to_date(int r) const;

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

  const std::vector<int>& triggers() const;
        std::vector<int>& triggers();

  /// Make heads[index] point to r
  int set_head(int index, int r);

  /// Allocate a reg in context t and put it on the top of the head stack.
  int allocate_head();

  /// Allocate a reg in context t and put it on the top of the temporary-head stack.
  int push_temp_head();
  int push_temp_head(int r);

  /// Deallocate the temporary head on the top of the temporary-head stack.
  void pop_temp_head();

  /// Add a new identifier, pointing to a newly allocated location
  int add_identifier(const std::string&);

  /// Add a new parameter, which is a modifiable.
  int add_parameter(const std::string&);

  /*----- Graph walking ------*/
  void find_all_regs_in_context(int, bool, std::vector<int>&) const;
  void find_all_used_regs_in_context(int, bool, std::vector<int>&) const;

  void find_all_regs_in_context_no_check(int, bool, std::vector<int>&) const;
  void find_all_regs_in_context_no_check(int, std::vector<int>&,std::vector<int>&) const;

  std::vector<int> find_all_regs_in_context(int, bool) const;
  std::vector<int> find_all_used_regs_in_context(int, bool) const;

  std::vector<int> find_all_regs_in_context_no_check(int, bool) const;

  /*----- Virtual memory ------*/
  int step_index_for_reg_(int t, int r) const;
  int result_index_for_reg_(int t, int r) const;
  const Step& step_for_reg_(int t, int r) const;
        Step& step_for_reg_(int t, int r);
  const Result& result_for_reg_(int t, int r) const;
        Result& result_for_reg_(int t, int r);
  bool has_step_(int t, int r) const;
  bool has_result_(int t, int r) const;
  bool reg_has_value_(int t, int r) const;
  bool reg_has_result_value_(int t, int r) const;
  int result_value_for_reg_(int t, int r) const;
  bool reg_has_call_(int t, int r) const;
  int call_for_reg_(int t, int r) const;

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

  int remove_shared_step(int t, int r);
  int remove_shared_result(int t, int r);

  int add_shared_step(int t, int r);
  int add_shared_result(int t, int r, int step);

  void clear_step(int t, int r);
  void clear_result(int t, int r);

  void clear_back_edges_for_reg(int r);
  void clear_back_edges_for_step(int s);
  void clear_back_edges_for_result(int rc);

  void check_back_edges_cleared_for_step(int rc);
  void check_back_edges_cleared_for_result(int rc);

  int move_step(int t1, int t2, int r);
  int move_result(int t1, int t2, int r);

  bool step_is_referenced(int,int) const;
  bool result_is_referenced(int,int) const;

  int value_for_reg(int r) const;

  void set_result_value_for_reg(int r1);

  std::vector<int> used_regs_for_reg(int r) const;

  void reroot_at(int t);

  void reroot_at_context(int c);

  /*----- Graph splitting -----*/
  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, closure&&, int t);
  void set_shared_value(int r, int v);
  bool reg_is_shared(int t, int r) const;
  void invalidate_shared_regs(int t1, int t2);
  void mark_completely_dirty(int t);
  bool is_completely_dirty(int t) const;
  bool is_dirty(int t) const;
  void find_callers(int t1, int t2, int start, const std::vector<int>& split, std::vector<int>& callers, int mark);
  void find_users(int t1, int t2, int start, const std::vector<int>& split, std::vector<int>& callers, int mark);

  std::pair<int,int> incremental_evaluate_in_context(int R, int c);

  std::pair<int,int> incremental_evaluate(int R);
private:  
  std::pair<int,int> incremental_evaluate_(int R);
  int incremental_evaluate_unchangeable_(int R);
  std::pair<int,int> incremental_evaluate_from_call(int P, int R);
  std::pair<int,int> incremental_evaluate_from_call_(int P, int R);

public:
  
  int incremental_evaluate_unchangeable(int R);

  const closure& lazy_evaluate(int& R);

  const closure& lazy_evaluate(int& R, int c);

  const closure& lazy_evaluate_head(int h, int c);

  const closure& lazy_evaluate_unchangeable(int& R);

  std::vector<int>& get_scratch_list() const;
  void release_scratch_list() const;

  reg_heap(const module_loader&);
};

expression_ref graph_normalize(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

void dot_graph_for_token(const reg_heap& C, int t);

struct no_context: public std::exception
{
};

extern int total_reductions;
extern int total_reg_allocations;
extern int total_comp_allocations;
extern int total_reroot;

#endif
