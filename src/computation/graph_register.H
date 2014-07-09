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

struct computation
{
  int source = -1;

  /// What final WHNF does this computation reduce to?
  int result = -1;

  /// Does C reduce to another reg that we need to evaluate to get the true result?
  int call = -1;

  /// Which reg's were used to reduce this expression?
  std::vector< int > used_inputs;

  /// Which pairs (reg,slot) USED the reduction result of C (via an operation)?
  std::vector< pool<computation>::weak_ref > used_by;

  /// Which reduction results made use of the value of this expression (via call)
  std::vector< pool<computation>::weak_ref > called_by;

  int temp = -1;

  void clear();

  void check_cleared();

  computation& operator=(const computation&) = delete;
  computation& operator=(computation&&) noexcept;

  computation() = default;

  computation(const computation&) = delete;
  computation(computation&&) noexcept;
};

class reg
{
public:
  /* The closure */
  closure C;

  /// Is C a parameter, or is the reduction result dependent on a parameter value?
  bool re_evaluate = false;

  enum class type_t {unknown=0, index_var=1, constant=2, changeable=3};

  type_t type = type_t::unknown;

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

  mutable checked_pool_callback<computation> computations;

  mutable int n_active_scratch_lists = 0;
  mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

  void get_more_memory();

  void expand_memory(int);

  void reclaim_used(int);

  bool computation_is_called_by(int, int) const;
  bool computation_is_used_by(int, int) const;
  bool reg_is_used_by(int, int, int) const;

public:

  reg_heap* clone() const {std::abort();}

  void check_used_reg(int) const;
  void check_used_regs() const;

  void collect_garbage();
  void trace_and_reclaim_unreachable();
  bool reg_is_changeable(int r) const;
  bool reg_is_constant(int r) const;
  void make_reg_changeable(int r);

  void set_used_input(int t, int R1, int R2);
  void set_call(int t, int R1, int R2);
  void clear_call(int rc);
  void clear_call_for_reg(int t, int R);
  void clear_C(int R);
  void set_C(int R, closure&& C);
  void set_reduction_result(int t, int R, closure&& C);

  void get_roots(std::vector<int>&, bool keep_identifiers=true) const;

  // the list of expressions that we are temporarily evaluating
  std::vector<int> temp;
  
  // the list of expressions that we are interested in evaluating.
  std::vector<int> heads;

  std::vector<int> probability_heads;

  void register_probability(int r);

  int register_probability(closure&& C);

  log_double_t probability_for_context(int c);

  // where is the location of the probability expression
  int probability_index = -1;

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

  std::vector<std::string> args;

  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  expression_ref translate_refs(const expression_ref& R, std::vector<int>&);
  /// Translate named identifiers (struct var) and named parameters (struct parameter) into reg_var references
  closure translate_refs(closure&& C);

  closure preprocess(const closure& C);

  /// Lookup the index for the named parameter s
  int find_parameter(const std::string& s) const;

  /*----- Modifiable ranges ----*/
  bool parameter_is_modifiable(int index);

  int find_parameter_modifiable_reg(int i);

  object_ref get_parameter_range(int c, int r);

  object_ref get_range_for_reg(int c, int r);

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

    /// Mapping from closures to computations/results
    mapping vm_relative;

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

  bool is_terminal_token(int) const;

  bool is_root_token(int) const;

  int parent_token(int) const;

  const std::vector<int>& children_of_token(int) const;

  int degree_of_token(int) const;

  /// Acquire a copy of a token
  int copy_token(int);

  /// Make a child of the current token, and unreference the current token.
  int switch_to_child_token(int);

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

  bool reg_is_fully_up_to_date(int t, int r) const;

  int incremental_evaluate_in_context(int R, int c);

  object_ref get_parameter_value_in_context(int R, int c);

  object_ref get_reg_value_in_context(int& R, int c);

  void set_reg_value_in_context(int index, closure&& C, int c);

  int get_modifiable_value_in_context(int R, int c);

  /*------------------------------------------------*/

  const std::vector<int>& get_temp_heads() const {return temp;}
        std::vector<int>& get_temp_heads()       {return temp;}

  const std::vector<int>& get_heads() const {return heads;}
        std::vector<int>& get_heads()       {return heads;}

  const std::vector<std::pair<std::string,int>>& get_parameters() const {return parameters;}
        std::vector<std::pair<std::string,int>>& get_parameters()       {return parameters;}

  const std::map<std::string,int>& get_identifiers() const {return identifiers;}
        std::map<std::string,int>& get_identifiers()       {return identifiers;}

  const std::vector<int>& triggers(int t) const;
        std::vector<int>& triggers(int t);

  /// Allocate a reg in context t and put it on the top of the temporary-head stack.
  int push_temp_head();

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
  int computation_index_for_reg_(int t, int r) const;
  const computation& computation_for_reg_(int t, int r) const;
        computation& computation_for_reg_(int t, int r);
  bool has_computation_(int t, int r) const;
  bool reg_has_result_(int t, int r) const;
  bool reg_has_computation_result_(int t, int r) const;
  int computation_result_for_reg_(int t, int r) const;
  bool reg_has_call_(int t, int r) const;
  int call_for_reg_(int t, int r) const;

  int computation_index_for_reg(int t, int r) const;
  const computation& computation_for_reg(int t, int r) const ;
        computation& computation_for_reg(int t, int r);
  bool has_computation(int t, int r) const;
  bool reg_has_result(int t, int r) const;
  bool reg_has_computation_result(int t, int r) const;
  int computation_result_for_reg(int t, int r) const;
  bool reg_has_call(int t, int r) const;
  int call_for_reg(int t, int r) const;

  const closure& access_result_for_reg(int t, int R1) const;

  int replace_shared_computation(int t, int r);

  int remove_shared_computation(int t, int r);

  int new_computation_for_reg(int) const;

  void duplicate_computation(int rc1, int rc2) const;

  int add_shared_computation(int t, int r);

  int find_computation_for_reg(int t, int r) const;

  bool computation_is_referenced(int,int) const;

  int result_for_reg(int t, int r) const;

  void set_computation_result_for_reg(int t, int r1);

  std::vector<int> used_regs_for_reg(int t, int r) const;

  void reroot_at(int t);

  /*----- Graph splitting -----*/
  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, closure&&, int t);
  void set_shared_value(int r, int v);
  bool reg_is_shared(int t, int r) const;
  int share_and_clear(int t, int r);
  void invalidate_shared_regs(int t1, int t2);
  void mark_completely_dirty(int t);
  bool is_completely_dirty(int t) const;
  bool is_dirty(int t) const;
  void find_callers(int t1, int t2, int start, const std::vector<int>& split, std::vector<int>& callers, int mark);
  void find_users(int t1, int t2, int start, const std::vector<int>& split, std::vector<int>& callers, int mark);
  bool reg_is_shared_with_parent(int t, int r) const;

  int incremental_evaluate(int R, int t);

  const closure& lazy_evaluate(int& R, int c);

  const closure& lazy_evaluate_unchangeable(int& R);

  std::vector<int>& get_scratch_list() const;
  void release_scratch_list() const;

  reg_heap();
};

expression_ref graph_normalize(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

void dot_graph_for_token(const reg_heap& C, int t);

struct no_context: public std::exception
{
};
#endif
