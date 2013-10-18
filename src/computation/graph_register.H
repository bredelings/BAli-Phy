#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H

#include <string>
#include <vector>
#include <set>
#include <bitset>
#include "object.H"
#include "expression.H"
#include "util.H"
#include <list>
#include <utility>
#include "closure.H"
#include "CacheList.H"
#include <unordered_map>
#include "pool.H"
#include "bithash.H"

struct computation
{
  int source = -1;

  typedef CacheList<int>::iterator back_edge_deleter;

  /// Does C reduce to another reg that we need to evaluate to get the true result?
  int call = -1;
  back_edge_deleter call_reverse;

  /* 2. The computations we used. */
  /// Which reg's were used to reduce this expression?
  std::vector< std::pair<int,back_edge_deleter> > used_inputs;

  /* 3. The computations that use or reference us. */

  /// Which pairs (reg,slot) USED the reduction result of C (via an operation)?
  CacheList< int > used_by;

  /// Which reduction results made use of the value of this expression (via call)
  CacheList<int> called_by;

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
  bool changeable = false;

  bool re_evaluate = false;

  enum class type_t {unknown=0, index_var=1, constant=2};

  type_t type = type_t::unknown;

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

class reg_heap: public Object, public checked_pool<reg>
{
  typedef checked_pool<reg> base_pool_t;

  checked_pool_callback<computation> computations;

  mutable int n_active_scratch_lists = 0;
  mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

  void get_more_memory();

  void expand_memory(int);

  void reclaim_used(int);

  void check_used_reg(int) const;
  void check_used_regs() const;

  int root_token = -1;

public:

  reg_heap* clone() const {std::abort();}

  const computation& computation_for_reg(int t, int r) const 
  { 
    int rc = computation_index_for_reg(t,r);
    assert(rc > 0);
    return computations.access_unused(rc);
  }

        computation& computation_for_reg(int t, int r)
  { 
    int rc = computation_index_for_reg(t,r);
    assert(rc > 0);
    return computations.access_unused(rc);
  }

  const closure& access_result_for_reg(int t, int R1) const
  {
    int R2 = result_for_reg(t,R1);
    assert(R2);
    return access(R2).C;
  }

  const closure& access_computation_result_for_reg(int t, int R1) const
  {
    int R2 = computation_result_for_reg(t,R1);
    assert(R2);
    return access(R2).C;
  }

  bool reg_has_call(int t, int r) const
  {
    return computation_for_reg(t,r).call;
  }

  int call_for_reg(int t, int r) const
  {
    return computations[computation_for_reg(t,r).call].source;
  }

  void collect_garbage();
  void trace_and_reclaim_unreachable();
  bool reg_is_changeable(int r) const;

  void set_used_input(int t, int R1, int R2);
  void clear_used_inputs_for_reg(int t, int R);
  void clear_used_inputs(int R);
  void set_call(int t, int R1, int R2);
  void set_call_unsafe(int t, int R1, int R2);
  void clear_call(int rc);
  void clear_call_for_reg(int t, int R);
  void clear_C(int R);
  void set_C(int R, closure&& C);
  void set_reduction_result(int t, int R, closure&& C);

  /*----- Token manager ------*/

  /// The list of unused_tokens
  std::vector<int> unused_tokens;

  void get_roots(std::vector<int>&) const;

  struct address
  {
    int rc = 0;
    int result = 0;
    int index = -1;
    address() {}
    address(int i1, int i2):rc(i1),result(i2),index(-1) {}
    address(int i1, int i2, int i3):rc(i1),result(i2),index(i3) {}
  };

  // the list of expressions that we are temporarily evaluating
  std::vector<int> temp;
  
  // the list of expressions that we are interested in evaluating.
  std::vector<int> heads;
  
  /// The register indices of the named parameters.
  std::vector<std::pair<std::string,int>> parameters;
  
  /// Probably this would be non-parameter identifiers
  std::map<std::string, int> identifiers;

  struct graph_roots
  {
    // The context this context is derived from.
    int parent = -1;
    // Contexts that are derived from this one.
    std::vector<int> children;
    bool referenced = false;
    
    /// A list of signals that have been sent
    std::vector<int> triggers;

    /// Mapping from closures to computations/results
    std::vector<address> virtual_mapping;

    std::vector<int> modified;

    /// These are the regs we need to check for new uses to split.
    std::vector<int> modified_modifiables;

    bool used;
    graph_roots():used(false) {}
  };

  const std::vector<int>& get_temp_heads() const {return temp;}
        std::vector<int>& get_temp_heads()       {return temp;}

  const std::vector<int>& get_heads() const {return heads;}
        std::vector<int>& get_heads()       {return heads;}

  const std::vector<std::pair<std::string,int>>& get_parameters() const {return parameters;}
        std::vector<std::pair<std::string,int>>& get_parameters()       {return parameters;}

  const std::map<std::string,int>& get_identifiers() const {return identifiers;}
        std::map<std::string,int>& get_identifiers()       {return identifiers;}

  const std::vector<int>& triggers(int t) const {return token_roots[t].triggers;}
        std::vector<int>& triggers(int t)       {return token_roots[t].triggers;}

  /// Allocate a reg in context t and put it on the top of the temporary-head stack.
  int push_temp_head();

  /// Deallocate the temporary head on the top of the temporary-head stack.
  void pop_temp_head();

  /// Add a new identifier, pointing to a newly allocated location
  int add_identifier(const std::string&);

  /// The roots for each token
  std::vector<graph_roots> token_roots;

  /// Is a particular token unused?
  bool token_is_used(int) const;

  /// How many tokens are there, maximum?
  int get_n_tokens() const {return token_roots.size();}

  /// Acquire an unused token
  int get_unused_token();

  /// Release the token if its unreferenced and has no children.
  void try_release_token(int);

  /// Mark the token unreferenced and try to release it.
  void release_token(int);

  bool is_terminal_token(int) const;

  bool is_root_token(int) const;

  int parent_token(int) const;

  /// Release just the identifiers for a token
  void release_identifiers();

  /// Acquire a copy of a token
  int copy_token(int);

  /*---- Ownership ------*/
  bool reg_is_shared(int) const;

  /*----- Graph walking ------*/
  void find_all_regs_in_context(int, std::vector<int>&) const;
  void find_all_used_regs_in_context(int, std::vector<int>&) const;

  void find_all_regs_in_context_no_check(int, std::vector<int>&) const;
  void find_all_regs_in_context_no_check(int, std::vector<int>&,std::vector<int>&) const;

  std::vector<int> find_all_regs_in_context(int) const;
  std::vector<int> find_all_used_regs_in_context(int) const;

  std::vector<int> find_all_regs_in_context_no_check(int) const;

  void check_results_in_context(int) const;

  /*----- Virtual memory ------*/
  int add_computation(int t, int r);

  void remove_computation(int t, int r);
  
  bool has_computation(int t, int r) const
  {
    int rc = token_roots[t].virtual_mapping[r].rc;
    return rc;
  }

  int computation_index_for_reg(int t, int r) const 
  {
    int rc = token_roots[t].virtual_mapping[r].rc;
    assert(rc > 0);
    return rc;
  }

  int result_for_reg(int t, int r) const 
  {
    return computation_result_for_reg(t,r);
  }

  int computation_result_for_reg(int t, int r) const 
  {
    return token_roots[t].virtual_mapping[r].result;
  }

  void set_computation_result_for_reg(int t, int r1, int r2)
  {
    token_roots[t].virtual_mapping[r1].result = r2;
  }

  void clear_computation_result(int t, int r)
  {
    set_computation_result_for_reg(t,r,0);
  }

  std::vector<int> used_regs_for_reg(int t, int r) const;

  void reroot_mappings_at(int t);

  /*----- Graph splitting -----*/
  int uniquify_reg(int reg, int t);
  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, closure&&, int t);

  int incremental_evaluate(int R, int t, bool = true);

  std::vector<int>& get_scratch_list() const;
  void release_scratch_list() const;

  reg_heap();
};

expression_ref graph_normalize(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

void dot_graph_for_token(const reg_heap& C, int t);

#endif
