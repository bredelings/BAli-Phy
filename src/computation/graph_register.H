#ifndef GRAPH_REGISTER_H
#define GRAPH_REGISTER_H

#include <string>
#include <vector>
#include <set>
#include "object.H"
#include "expression.H"
#include "util.H"
#include <list>
#include <utility>
#include "program.H"

typedef std::set<int> owner_set_t;

struct reg
{
private:
  /* 5. Which sub-graphs are we in for sub-graph sharing? */
  owner_set_t owners;

public:
  /* 1. The computation. */

  /// The expression
  expression_ref E;

  /// Is E a parameter, or is the result dependent on a parameter value?
  bool changeable;

  /// What does E reduce to eventually? (WHNF, but not a heap variable.)
  expression_ref result;

  /// Does E reduce to another reg that we need to evaluate to get the true result?
  int call;

  /* 2. The computations we used */
  /// Which reg's were used to reduce this expression?
  std::set< int > used_inputs;


  /* 3. The computations that use or reference us. */

  /// Which pairs (reg,slot) USED the reduction result of E (via an operation)?
  std::set< int > outputs;

  /// Which reduction results made use of the value of this expression (via call)
  std::set< int > call_outputs;

  /// The nodes that directly reference us (not via a call)
  std::set< int > referenced_by_in_E;

  /// The nodes that we directly reference from E
  std::set< int > references;

  /* 4. Fields used by the memory manager */
  enum States {none=0,free=1,used=2,checked=3};

  int prev_reg;
  int next_reg;
  mutable States state;

  const owner_set_t& get_owners() const;
  void set_owners(const owner_set_t&);
  void add_owner(int);
  void clear_owner(int);
  void clear_owners();
  bool is_owned_by(int) const;
  bool is_owned_by_all_of(const owner_set_t&) const;
  int n_owners() const;
  bool is_unowned() const;
  bool is_shared() const;

  /* 5. Which sub-graphs are we in for sub-graph sharing? */
  owner_set_t temp_owners;

  reg();
};

struct var: public Object
{
  std::string name;

  var* clone() const {return new var(*this);}

  std::string print() const
  {
    return name;
  }

  tribool compare(const Object& o) const
  {
    const var* V = dynamic_cast<const var*>(&o);
    if (not V)
      return false;

    return name == V->name;
  }

  var(const std::string& s)
    :name(s)
  { }
};


struct reg_var: public Object
{
  int target;

  reg_var* clone() const {return new reg_var(*this);}

  std::string print() const 
  {
    return "<" + convertToString(target) + ">";
  }

  tribool compare(const Object& o) const
  {
    const reg_var* E = dynamic_cast<const reg_var*>(&o);
    if (not E) 
      return false;

    return target == E->target;
  }

  reg_var():
    target(-1)
  { }

  reg_var(int t):
    target(t)
  { }
};

// How do we handle parameters?
// * well, parameters could be reg's, so that a separate reg must be a separate parameter.
// * how, then, do we handle reg NAME's, if reg's are not uniquely identified by their names?

class reg_heap
{
  std::vector<reg> memory;
  std::list<int> roots;

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
  typedef std::list<int>::iterator root_t;
  typedef std::list<int>::const_iterator const_root_t;

  const reg& access(int i) const {return memory[i];}
        reg& access(int i)       {return memory[i];}

  const reg& operator[](int i) const {return access(i);}
        reg& operator[](int i)       {return access(i);}

  int n_regs() const;
  int n_free_regs() const;
  int n_used_regs() const;

  root_t push_root(int);
  void pop_root(root_t);

  root_t allocate_reg();

  void collect_garbage();

  void set_used_input(int R1, int R2);
  void clear_used_input(int R1, int R2);
  void clear_used_inputs(int R);
  void set_call(int R1, int R2);
  void set_call_unsafe(int R1, int R2);
  void clear_call(int R);
  void clear(int);
  void clear_E(int R);
  void set_E(int R, const expression_ref& e);
  void set_reduction_result(int R, const expression_ref& E);

  /*----- Token manager ------*/

  /// The list of unused_tokens
  std::vector<int> unused_tokens;

  struct graph_roots
  {
    // the list of expressions that we are temporarily evaluating
    std::vector<root_t> temp;

    // the list of expressions that we are interested in evaluating.
    std::vector<root_t> heads;
    
    /// The register indices of the named parameters.
    std::vector<root_t> parameters;

    /// Probably this would be non-parameter identifiers
    std::map<std::string, root_t> identifiers;

    bool used;
    graph_roots():used(false) {}
  };

  const std::vector<root_t>& get_temp_heads_for_context(int t) const {return token_roots[t].temp;}
        std::vector<root_t>& get_temp_heads_for_context(int t)       {return token_roots[t].temp;}

  const std::vector<root_t>& get_heads_for_context(int t) const {return token_roots[t].heads;}
        std::vector<root_t>& get_heads_for_context(int t)       {return token_roots[t].heads;}

  const std::vector<root_t>& get_parameters_for_context(int t) const {return token_roots[t].parameters;}
        std::vector<root_t>& get_parameters_for_context(int t)       {return token_roots[t].parameters;}

  const std::map<std::string,root_t>& get_identifiers_for_context(int t) const {return token_roots[t].identifiers;}
        std::map<std::string,root_t>& get_identifiers_for_context(int t)       {return token_roots[t].identifiers;}

  /// Allocate a reg in context t and put it on the top of the temporary-head stack.
  root_t push_temp_head(int t);

  /// Allocate a reg in contexts tokens and put it on the top of the temporary-head stacks.
  root_t push_temp_head(const owner_set_t& tokens);

  /// Deallocate the temporary head on the top of the temporary-head stack.
  void pop_temp_head(int t);

  /// Deallocate the temporary head on the top of the temporary-head stacks for context tokens.
  void pop_temp_head(const owner_set_t& tokens);

  /// Add a new identifier, pointing to a newly allocated location
  root_t add_identifier_to_context(int, const std::string&);

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

  /// Acquire a copy of a token
  int copy_token(int);

  bool reg_is_shared(int) const;

  bool reg_is_owned_by(int,int) const;

  reg_heap();

  /*----- Graph walking ------*/
  std::vector<int> find_all_regs_in_context(int) const;
  /*----- Graph walking ------*/
  std::vector<int> find_all_regs_in_context_no_check(int) const;
  /*----- Graph walking ------*/
  std::vector<int> find_shared_ancestor_regs_in_context(int,int) const;
  /*----- Graph walking ------*/
  std::vector<int> find_call_ancestors_in_context(int,int) const;
  /*----- Graph walking ------*/
  void check_results_in_context(int) const;
  /*----- Graph splitting -----*/
  int uniquify_reg(int reg, int t);
  /// Update the value of a non-constant, non-computed index
  void set_reg_value(int index, const expression_ref&, int t);

  int incremental_evaluate(int R, int t);
};

expression_ref graph_normalize(const expression_ref& R);

bool is_reg_var(const expression_ref& R);

bool is_var(const expression_ref& R);

bool is_reg_like(const expression_ref& R);

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o);

#endif
