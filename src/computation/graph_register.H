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

// Type for ownership sets
typedef std::bitset<64> owner_set_t;
typedef CacheList<owner_set_t>::iterator ownership_category_t;

struct reg
{
public:
  /* 5. Which sub-graphs are we in for sub-graph sharing? */
  owner_set_t owners;

  /// Pointer to the current ownership set
  ownership_category_t ownership_category;

  typedef CacheList<int>::iterator back_edge_deleter;

  /* The closure */
  closure C;
  std::vector<back_edge_deleter> referenced_by_in_E_reverse;
 
 /// Is C a parameter, or is the reduction result dependent on a parameter value?
  bool changeable = false;

  /// What does C reduce to eventually? (WHNF, but not an index_var.)
  int result;

  /// Does C reduce to another reg that we need to evaluate to get the true result?
  int call = -1;
  back_edge_deleter call_reverse;

  /* 2. The computations we used. */
  /// Which reg's were used to reduce this expression?
  std::vector< std::pair<int,back_edge_deleter> > used_inputs;

  /* 3. The computations that use or reference us. */

  /// Which pairs (reg,slot) USED the reduction result of C (via an operation)?
  CacheList< int > outputs;

  /// Which reduction results made use of the value of this expression (via call)
  CacheList<int> call_outputs;

  /// The nodes that directly reference us (not via a call)
  CacheList<int> referenced_by_in_E;

  /* 4. Fields used by the memory manager */
  enum States {none=0,free=1,used=2,checked=3};

  int prev_reg = -1;
  int next_reg = -1;
  mutable States state = none;

  /* 5. Which sub-graphs are we in for sub-graph sharing? */
  owner_set_t temp_owners;

  int temp = -1;

  bool re_evaluate = false;

  reg& operator=(const reg&) = delete;
  reg& operator=(reg&&) noexcept;

  reg() = default;

  reg(const reg&) = delete;
  reg(reg&&) noexcept;
};

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

class reg_heap
{
  std::vector<reg> memory;
  CacheList<int> roots;

  mutable int n_active_scratch_lists = 0;
  mutable std::vector< object_ptr<Vector<int> > > scratch_lists;

  CacheList<owner_set_t> ownership_categories;
  bithash< owner_set_t, ownership_category_t> canonical_ownership_categories;

  /// Start of free list 
  int first_free_reg;

  /// Start of used list 
  int first_used_reg;

  void expand_memory(int);

  /// Add a reg (not on any list) to the free list
  int add_reg_to_free_list(int);
  /// Take the first free reg off the free list, or return -1 if empty
  int get_free_reg();

  /// Add a reg (not on any list) to the used list
  int add_reg_to_used_list(int);

  void remove_reg_from_used_list(int);

  void reclaim_used_reg(int);

  void remove_unused_ownership_marks();

  void check_used_reg(int) const;
  void check_used_regs() const;

public:
  typedef CacheList<int>::iterator root_t;
  typedef CacheList<int>::const_iterator const_root_t;

  const reg& access(int i) const {return memory[i];}
        reg& access(int i)       {return memory[i];}

  const reg& operator[](int i) const {return access(i);}
        reg& operator[](int i)       {return access(i);}


  const closure& access_result(int R1) const
  {
    int R2 = access(R1).result;
    assert(R2);
    return access(R2).C;
  }

  void clear_result(int R) {access(R).result = 0;}

  int n_regs() const;
  int n_free_regs() const;
  int n_used_regs() const;
  int n_null_regs() const;

  root_t push_root(int);
  void pop_root(root_t);

  root_t allocate_reg();

  void collect_garbage();
  void trace_and_reclaim_unreachable();
  void compute_ownership_categories();

  void set_used_input(int R1, int R2);
  void clear_used_inputs(int R);
  void set_call(int R1, int R2);
  void set_call_unsafe(int R1, int R2);
  void clear_call(int R);
  void clear(int);
  void clear_C(int R);
  void set_C(int R, closure&& C);
  void set_reduction_result(int R, closure&& C);

  /*----- Token manager ------*/

  /// The list of unused_tokens
  std::vector<int> unused_tokens;

  void get_roots(std::vector<int>&) const;
  void get_roots(std::vector<int>&,int) const;

  struct graph_roots
  {
    // the list of expressions that we are temporarily evaluating
    std::vector<int> temp;

    // the list of expressions that we are interested in evaluating.
    std::vector<int> heads;
    
    /// The register indices of the named parameters.
    std::vector<std::pair<std::string,int>> parameters;

    /// Probably this would be non-parameter identifiers
    std::map<std::string, int> identifiers;

    /// The register indices of non-root parameters
    pool<int> modifiable_regs;

    /// A list of signals that have been sent
    std::vector<int> triggers;

    bool used;
    graph_roots():used(false) {}
  };

  const std::vector<int>& get_temp_heads_for_context(int t) const {return token_roots[t].temp;}
        std::vector<int>& get_temp_heads_for_context(int t)       {return token_roots[t].temp;}

  const std::vector<int>& get_heads_for_context(int t) const {return token_roots[t].heads;}
        std::vector<int>& get_heads_for_context(int t)       {return token_roots[t].heads;}

  const std::vector<std::pair<std::string,int>>& get_parameters_for_context(int t) const {return token_roots[t].parameters;}
        std::vector<std::pair<std::string,int>>& get_parameters_for_context(int t)       {return token_roots[t].parameters;}

  const pool<int>& get_modifiable_regs_for_context(int t) const {return token_roots[t].modifiable_regs;}
        pool<int>& get_modifiable_regs_for_context(int t)       {return token_roots[t].modifiable_regs;}

  const std::map<std::string,int>& get_identifiers_for_context(int t) const {return token_roots[t].identifiers;}
        std::map<std::string,int>& get_identifiers_for_context(int t)       {return token_roots[t].identifiers;}

  const std::vector<int>& triggers(int t) const {return token_roots[t].triggers;}
        std::vector<int>& triggers(int t)       {return token_roots[t].triggers;}

  /// Allocate a reg in context t and put it on the top of the temporary-head stack.
  root_t push_temp_head(int t);

  /// Allocate a reg in contexts tokens and put it on the top of the temporary-head stacks.
  root_t push_temp_head(const owner_set_t& tokens);

  /// Deallocate the temporary head on the top of the temporary-head stack.
  void pop_temp_head(int t);

  /// Deallocate the temporary head on the top of the temporary-head stacks for context tokens.
  void pop_temp_head(const owner_set_t& tokens);

  /// Add a new identifier, pointing to a newly allocated location
  int add_identifier_to_context(int, const std::string&);

  /// The roots for each token
  std::vector<graph_roots> token_roots;

  /// Is a particular token unused?
  bool token_is_used(int) const;

  /// How many tokens are there, maximum?
  int get_n_tokens() const {return token_roots.size();}

  /// Acquire an unused token
  int get_unused_token();

  /// Release a used token
  void release_token(int);

  /// Release just the identifiers for a token
  void release_identifiers(int);

  /// Acquire a copy of a token
  int copy_token(int);

  const owner_set_t& get_reg_owners(int) const;
  
  const ownership_category_t& get_reg_ownership_category(int) const;
  
  void set_reg_owners(int, const owner_set_t& o);

  void set_reg_ownership_category(int, const ownership_category_t&);

  void reg_add_owner(int, int);

  void reg_clear_owner(int, int);

  void reg_clear_owners(int);

  bool reg_is_owned_by(int,int) const;

  bool reg_is_owned_by_only(int,int) const;

  bool reg_is_owned_by_all_of(int,const owner_set_t&) const;

  int n_reg_owners(int) const;

  bool reg_is_unowned(int) const;

  bool reg_is_shared(int) const;

  /*----- Graph walking ------*/
  void remove_ownership_mark(int);
  /*----- Graph walking ------*/
  void duplicate_ownership_mark(int,int);
  /*----- Graph walking ------*/
  void find_all_regs_in_context(int, std::vector<int>&) const;
  void find_all_used_regs_in_context(int, std::vector<int>&) const;
  /*----- Graph walking ------*/
  void find_all_regs_in_context_no_check(int, std::vector<int>&) const;
  void find_all_regs_in_context_no_check(int, std::vector<int>&,std::vector<int>&) const;
  /*----- Graph walking ------*/
  std::vector<int> find_all_regs_in_context(int) const;
  std::vector<int> find_all_used_regs_in_context(int) const;
  /*----- Graph walking ------*/
  std::vector<int> find_all_regs_in_context_no_check(int) const;
  /*----- Graph walking ------*/
  void find_shared_ancestor_regs_in_context(int,int, std::vector<int>&) const;
  /*----- Graph walking ------*/
  void find_unsplit_parents(const std::vector<int>&,int, std::vector<int>&) const;
  /*----- Graph walking ------*/
  std::vector<int> find_call_ancestors_in_context(int,int) const;
  /*----- Graph walking ------*/
  void check_results_in_context(int) const;
  /*----- Graph splitting -----*/
  int remap_reg(int) const;
  /*----- Graph splitting -----*/
  closure remap_regs(closure) const;
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

expression_ref Fun_normalize(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

void dot_graph_for_token(const reg_heap& C, int t);

#endif
