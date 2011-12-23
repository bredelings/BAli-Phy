#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include <algorithm>

using boost::shared_ptr;
using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;

using std::endl;
/*
 * 1. Q: When can we allow sharing of partially-evaluated expressions between contexts?
 *    A: If the memories for the two contexts are completely separate, then only when
 *       the WHNF reduced result doesn't (directly or indirectly) reference a var newly
 *       allocated in only one of the contexts.
 *
 * 2. Q: Therefore, we seek to put the contexts BACK into a single machine!  How can we handle
 *       nodes that are in only SOME of the graphs?
 *    A:
 *      (i) Give each graph a unique name.
 *     (ii) Allowing each node and edge to list the names of the graphs that contain it.
 *    (iii) When initializing a graph, we need to walk all its nodes and edges in order to name them.
 *     (iv) When destroying a graph, we also need to walk all its nodes and edges in order to un-name them.
 *      (v) When modifying a node in a graph with name N, we first check if that node is in any other graphs.
 *          - If the node is in a unique graph, then just modify it.
 *          - If the node is NOT is a unique graph, then
 *            (a) duplicate the node n1->n2, and move the name N to the new node n2.
 *            (b) update all edges to n1 to point to the new node n2.
 *            (c) modify the new node n2 that is now unique to graph N.
 *     (vi) If the out-edges from a node are part of its VALUE, then modifying any single node n1 entails
 *          walking up the tree toward the heads in order to split the nodes.
 *    (vii) If we split a node pointed to by a head, then we need to modify the head, but only for the graph
 *          that is being modified.
 *
 * 3. How do we succeed in clearing a node if we need to update the nodes that connect to it?
 *    We need to do this clearing from inside the machine!
 *    Move the set_*( ) and clear_*( ) routines into reg_heap!(?)
 *
 * 4. The ultimate goal of the root re-work was ACTUALLY to enable a pointer that also prevents the reg
 *    it points to from being garbage-collected.
 *    
 *    (i) We convert to expression_ref only by COMPLETELY (e.g. not partially) evaluating the object,
 *        including any of its constructor fields, if they exist.  (This is involves creating a copy of the
 *        object, if any fields are unevaluated.)
 *      
 *   (ii) We would like to have a type T that can reference partially-evaluate structures.  Referencing a
 *        field and a head type evaluates the structure, and returns a reference of type T to that field. 
 *        However, to convert to expression_ref, we COMPLETELY evaluate.  If the field is already an
 *        atomic (i.e. non-expression) object, this is quite simple.  If not, then it would trigger more
 *        evaluation.
 *        
 */


/* ------------------------------------ Shared subgraphs ---------------------------------------------
 *
 * Goal: Share computation of WHNF structures between contexts, even when those stuctures are uncomputed
 *       at the time the contexts are split.
 *
 * If a reg is shared, then all of its E-descendants must be shared as well.  (Here, "descendants" means
 * transitively reachable through node.E's.)  Therefore, we have a single version of edges in
 * - E
 * - used_inputs
 * - call
 * - changeable
 * Since these terms describe the computation that a node represents.
 *
 * Instead of annotating each edge with a graph number, we can simply assume that
 * (a) if a node is in graph N, then out-edges are in graph N.
 *
 * 1. Setting ownership
 * We need to set ownership for new heads.
 * When we analyze a let expression, we need to set ownership for the newly created heap vars.
 * - we COULD set this ownership in set_E.  (Does this ownership setting PROPAGATE?)
 * When we reduce an expression
 * (a) the reduced expression cannot reference (transitively) any regs the original expression didn't reference.
 * (b) the exception is when we create a call reg.  Then we need to set ownership on the call reg.
 *
 * 2. Removing ownership
 * If we remove a head, then we would remove ownership for all its descendant.
 * Not all the let vars may end up being substituted into the resulting expression.
 * When reducing an expression:
 * (a) We might replace the original expression, using set_E.  This might reference fewer vars.
 * (b) We might create a call reg.  No dereferencing here.
 *
 * Now, previous when dereferencing, we didn't worry about the cost of invalidating unreachable
 * from roots) regs that reference a parameter.  Let's worry about this later.
 *
 * Also, note that we would invalidate them only if they had ever been computed, and USED the 
 * parameter.  After being invalidated once, they would never be walked again.  How about the cost
 * of "splitting" such regs, if they are not garbage collected first?  Let's worry about that later.
 *
 * See: let a=X+X, b=Y+Y in b.  Here, 
 *
 * 3. Uniquifying a heap variable
 *
 * When splitting a heap variable p:
 *
 *  NOTE: We only need to split E-ancestors of p that are not already unique!
 *
 *  I. We must uniquify any heap variable q which references p through E.
 *  This is because we must alter q.E to update references to p.  
 *   - q: This entails updating q.E, q.references, and q.used_inputs
 *        It also involves updating q.result if its non-NULL.  The result might not change.
 *   - p: This entails updating p.referenced_by and q.outputs
 *
 *  II. We must also uniquify any heap variable q which calls p.
 *   - q: This entails updating q.call.  It also involves updating
 *   - p: This entails updating p.call_outputs
 *
 *  (Note: if we split just a head h, we would NOT need to update h.result, h.E, h.call, etc.)
 *
 *---------------------------------------------------------------------------------------------------
 *  THEOREM: For a node p, the reduction of p.E could reference any node q that is an E-descendant of p.
 *           This is because it can certain reference its E-children, and it may also use their results,
 *            and so it may reference the E-descendants of its children. (rephrase induction.)
 *
 *  THEOREM: For a node p, p.result can reference a node that is not an E-descendant of p only if
 *           E generates a new call reg q such that
 *           (i) q.E is a let expression, or
 *          (ii) q.result contains a node that is not an E-descendant of q.E.
 *
 *  We don't have to consider the case where p.E is a let expression, since in that case p.E
 *  is replaced with the reduced expression, and so there is no result, and p.E references the created
 *  nodes.
 *
 *  CORROLARY: For a node p, p.result can reference nodes that are not E-descendents of p only if
 *             these nodes are let-introduced in call-descendants of p.
 *
 *  Calls may contain let expressions which generate new heap variables and pass them
 *  back to the calling node via constructors or lambdas which contain them.
 *  These heap variables will not even be marked "used" from the calling node!
 *
 *  Example: (take 3 (iota 1)) where iota n = n:(iota (n+1))
 *
 *  [Therefore, we must update all results which can reach any split reg through a sequence of calls.]
 * 
 *---------------------------------------------------------------------------------------------------
 *
 * THEOREM:  For a node p, a node q that is NOT an E-ancestor of p can call a node r that IS an
 *           E-ancestor of p, but only if p is let-introduced.
 *
 * CORROLARY: Any node that calls an E-ancestor of a parameter p is also an E-ancestor of p.
 *
 *---------------------------------------------------------------------------------------------------
 *
 * Question: Could the results of E-ancestors that are NOT E-parents of split nodes change?
 *
 *---------------------------------------------------------------------------------------------------
 *
 * Algorithm
 * 
 * (1) Find and split all the *shared* E-ancestors of p.
 * 
 * (2) Initialize each split reg with
 *       set_E
 *       set_used_inputs
 *       set_call
 *       copy the result
 *
 * (3) For each split reg with parent q, such that remap(q)=q2
 *       q2.E // remap
 *       q2.used_inputs // remap
 *       q2.call // remap
 *       q2.result = q2.E  [*IF* q2.result and there is NOT a call.]
 *
 * Practically, though, doesn't this include EVERY split node, except the original one?
 * Because a node is split only if it has an E-child that is split.  I think so.
 * Therefore, We may as well initialize them to remapped values.
 *
 * Also, we can find the parents on the periphery, because they are the only E-ancestors
 * of the original nodes that are still in t. By remapping them, they should STOP being
 * E-ancestors of the original nodes.
 *
 * (4) For each reg q that has an updated E and an updated result,
 *       i. find all the call ancestors of q
 *      ii. set their result to q.result
 *
 * Check that a compute expression of IF(2>1,X,0) simplifies to just X.
 *
 * Note that, if a call is not to an E-ancestor of p, then the result should be unchanged.
 *  Idea: For any node whose call is unadjusted, keep the result.
 *        For any node whose call is adjusted by splitting, set that node's result to NULL, and 
 *          find any node that (transitively) calls that node and set its result to NULL. (Sharing, anyone? :-P)
 *        These heap variables can follow call chains and re-add their result
 *        in a forward evaluation, if they want to.
 *        (Result: we don't have to re-run any Ops: the op results have already been updated 
 *
 *        If a node has no call, then it is the end of a (possibly trivial) call chain.
 *        In that case, remap the result.
 *         Q: Will that influence anything else?
 *         A: No.  If the result is used as an argument to an operation, the operation should still
 *            be correct, up to the renaming of reg_vars.
 *
 *        Result: no heap variables that are not split need to have their result adjusted.
 *
 *  Q: What kind of expressions will be split because they reference a split node, but do
 *     not use it? (I'm thinking of If(Z>1,2*X,Y+1).)
 *
 *  Q: Can't we just leave the old results, on the theory that (until something is invalidated)
 *     the old registers are OK - they have the same values?
 *  A: No, because they will reference the wrong parameter values.  Constructors don't change
 *     when the parameters they reference change.
 */

/*
 * It would indeed be faster, when setting a NUMBER of values to remember which parameters are dirty,
 * and to only invalidate downstream nodes when we know that we are going to access them.
 * This is essentially what Andrew and Alexei are doing by requiring the user to check the dirtiness
 *  instead of forcing it do be done as soon as a parameter value is changed.
 */

/*
 * Question: how shall we share sub-expressions between different compute expressions?
 * Question: how shall we (or, should we) pre-execute non-recursive let expressions?
 */ 

reg::reg()
 :changeable(false),
  call(-1),
  // initialize result to NULL, so that inserting copies of a single reg() doesn't result in sharing.
  prev_reg(-1),
  next_reg(-1),
  state(none)
{}

int context::add_note(const expression_ref& E)
{
  notes.push_back(E);
  return notes.size()-1;
}

void reg_heap::clear(int R)
{
  access(R).E = expression_ref();
  access(R).changeable = false;
  access(R).result = expression_ref(); // enforce unsharing

  access(R).used_inputs.clear();
  access(R).call = -1;

  // Upstream objects can NOT still exist - otherwise this object would be used :-)
  access(R).outputs.clear();
  access(R).call_outputs.clear();

  access(R).owners.clear();
}

void reg_heap::set_used_input(int R1, int slot, int R2)
{
  assert(R1 >= 0 and R1 < n_regs());
  assert(R2 >= 0 and R2 < n_regs());

  assert(access(R1).used_inputs[slot] == -1);

  access(R1).used_inputs[slot] = R2;
  access(R2).outputs.insert(pair<int,int>(R1,slot));
}

void reg_heap::clear_used_input(int R, int slot)
{
  int R2 = access(R).used_inputs[slot];
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());

  access(R2).outputs.erase(pair<int,int>(R,slot));
  access(R).used_inputs[slot] = -1;
}

void reg_heap::clear_used_inputs(int R)
{
  for(int i=0;i<access(R).used_inputs.size();i++)
    clear_used_input(R,i);
}

void reg_heap::clear_used_inputs(int R, int S)
{
  clear_used_inputs(R);
  access(R).used_inputs = vector<int>(S, -1);
}

template <typename T>
bool includes(const std::set<T>& s1, const std::set<T>& s2)
{
  return std::includes(s1.begin(), s1.end(), s2.begin(), s2.end());
}

template <typename T>
bool includes(const std::set<T>& s1, const T& t)
{
  return s1.find(t) != s1.end();
}

template <typename T,typename U>
bool includes(const map<T,U>& s1, const T& t)
{
  return s1.find(t) != s1.end();
}

void reg_heap::set_call(int R1, int R2)
{
  // Check that R1 is legal
  assert(0 <= R1 and R1 < n_regs());
  assert(access(R1).state == reg::used);

  // Check that R2 is legal
  assert(0 <= R2 and R2 < n_regs());
  assert(access(R2).state == reg::used);

  // Check that we aren't overriding an existing *call*
  assert(access(R1).call == -1);
  // Check that we aren't overriding an existing *result*
  assert(not access(R1).result);

  access(R1).call = R2;
  access(R2).call_outputs.insert(R1);

  // check that all of the owners of R are also owners of R.call;
  assert(includes(access(R2).owners, access(R1).owners));
}

void reg_heap::clear_call(int R)
{
  int R2 = access(R).call;
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());
  
  access(R).call = -1;
  access(R2).call_outputs.erase(R);
}

void get_exp_refs(const expression_ref& R, set<int>& refs)
{
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( R ))
  {
    refs.insert(RV->target);
  }
  else if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      get_exp_refs(E->sub[i],refs);
  }
}

set<int> get_exp_refs(const expression_ref& R)
{
  set<int> regs;
  get_exp_refs(R,regs);
  return regs;
}

void reg_heap::set_E(int R, const expression_ref& e)
{
  assert(not access(R).owners.empty());
  clear_E(R);

  access(R).E = e;
  access(R).references = get_exp_refs(e);
  foreach(r, access(R).references)
  {
    // check that all of the owners of R are also owners of *r.
    assert(includes(access(*r).owners, access(R).owners) );

    // check that *r is not already marked as being referenced by R
    assert(not includes( access(*r).referenced_by_in_E, R) );

    // mark *r as being referenced by R
    access(*r).referenced_by_in_E.insert(R);
  }
}

void reg_heap::clear_E(int R)
{
  /*
  clear_call(R);
  clear_used_inputs(R);
  access(R).result.reset();
  */

  foreach(r,access(R).references)
    access(*r).referenced_by_in_E.erase(R);

  access(R).references.clear();

  access(R).E = expression_ref();
}

string context::parameter_name(int i) const
{
  expression_ref E = access(*parameters()[i]).E;
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(E))
  {
    return P->parameter_name;
  }
  throw myexception()<<"Parameter "<<i<<" is not a parameter: can't find name!";
}

void context::add_variable(const string& name, int R)
{
  // if there's already an 's', then complain
  if (find_variable(name) != -1)
    throw myexception()<<"Cannot add variable '"<<name<<"': there is already a variable with that name.";

  if (find_parameter(name) != -1)
    throw myexception()<<"Cannot add variable '"<<name<<"': there is already a parameter with that name.";

  assert(access(R).state == reg::used);

  variables.push_back(std::pair<string,int>(name,R));
}

void context::rename_variable(int i, const string& s2)
{
  // zero-length names are not allowed
  const string& s1 = variables[i].first;
  assert(s2.size() != 0);

  // if there's already an 's2', then complain
  if (find_variable(s2) != -1)
    throw myexception()<<"Cannot rename variable '"<<s1<<"' to '"<<s2<<"': there is already a variable with that name.";

  if (find_parameter(s2) != -1)
    throw myexception()<<"Cannot rename variable '"<<s1<<"' to '"<<s2<<"': there is already a parameter with that name.";

  // Remove the old name -> reg mapping
  int R = variables[i].second;

  variables[i].first = s2;

  assert(access(R).state == reg::used);
}

void context::rename_parameter(int i, const string& new_name)
{
  string old_name = parameter_name(i);

  int R = *parameters()[i];

  assert( access(R).changeable == true );
  set_E(R, parameter(new_name) );
}

int incremental_evaluate(const context&, int);

// Is there a way to generalize the updating of reg_var elements of structures,
// when incremental evaluation walks a reg_var chain?

expression_ref full_evaluate(const context& C, int& R)
{
  R = incremental_evaluate(C,R);
  expression_ref result = C.access(R).result;

  {
    // If the result is atomic, then we are done.
    shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(result);
    if (not E) return result;

    // If the result is a lambda function, then we are done.
    // (a) if we are going to USE this, we should just call lazy evaluate! (which return a heap variable)
    // (b) if we are going to PRINT this, then we should probably normalize it more fully....?
    if (not dynamic_pointer_cast<const Function>(E->sub[0])) return result;
  }

  // If the result is a structure, then evaluate its fields and substitute them.
  {
    shared_ptr<expression> E = dynamic_pointer_cast<expression>(result);
    assert(dynamic_pointer_cast<const Function>(E->sub[0]));

    for(int i=1;i<E->size();i++)
    {
      shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(E->sub[i]);
      assert(RV);
      int R2 = RV->target;

      E->sub[i] = full_evaluate(C, R2);
    }
    return result;
  }
}

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::lazy_evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(*this, H);

  return access(H).result;
}

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(*this, H);

  return full_evaluate(*this, H);
}

expression_ref graph_normalize(const context&, const expression_ref&);

shared_ptr<const Object> context::lazy_evaluate_expression(const expression_ref& E) const
{
  int R = *push_temp_head();
  set_E(R, graph_normalize(*this, translate_refs(E)) );

  R = incremental_evaluate(*this,R);
  shared_ptr<const Object> result = access(R).result;

  pop_temp_head();
  return result;
}

shared_ptr<const Object> context::evaluate_expression(const expression_ref& E) const
{
  int R = *push_temp_head();
  set_E(R, graph_normalize(*this, translate_refs(E)) );

  expression_ref result = full_evaluate(*this,R);

  pop_temp_head();
  return result;
}

/// Get the value of a non-constant, non-computed index -- or should this be the nth parameter?
shared_ptr<const Object> context::get_parameter_value(int index) const
{
  int P = *parameters()[index];

  if (not access(P).result)
  {
    // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
    if (access(P).call == -1) return shared_ptr<const Object>();

    // If the value needs to be compute (e.g. its a call expression) then compute it.
    incremental_evaluate(*this, P);
  }

  return access(P).result;
}

/// Get the value of a non-constant, non-computed index
shared_ptr<const Object> context::get_parameter_value(const std::string& name) const
{
  int index = find_parameter(name);
  if (index == -1)
    throw myexception()<<"Cannot find parameter called '"<<name<<"'";

  return get_parameter_value(index);
}

void context::set_parameter_value(int index, const expression_ref& O)
{
  int P = *parameters()[index];

  set_reg_value(P, O);
}

void reg_heap::set_reduction_result(int R, const expression_ref& result)
{
  // Check that there is no result we are overriding
  assert(not access(R).result );

  // Check that there is no previous call we are overriding.
  assert(access(R).call == -1);

  // If the value is a pre-existing reg_var, then call it.
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(result))
  {
    int Q = RV->target;
    
    assert(0 <= Q and Q < n_regs());
    assert(access(Q).state == reg::used);
    
    set_call(R,Q);
  }
  // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the result and call it.
  else
  {
    root_t r = allocate_reg();
    access(*r).owners = access(R).owners;
    set_E(*r, result );
    set_call(R, *r);
    pop_root(r);
  }
}

/// Update the value of a non-constant, non-computed index
void context::set_reg_value(int P, const expression_ref& OO)
{
  // Split this reg and its E-ancestors out from other graphs, if its shared.
  P = memory->uniquify_reg(P,token);

  // Normalize the inputs expression
  expression_ref O = graph_normalize(*this, translate_refs(OO));

  // Check that this reg is indeed settable
  assert(dynamic_pointer_cast<const parameter>(access(P).E));
  assert(access(P).changeable);

  // Clear the call, clear the result, and set the value
  memory->clear_call(P);
  access(P).result.reset();
  set_reduction_result(P, O);

  vector< int > NOT_known_value_unchanged;
  std::set< int > visited;

  // The index that we just altered cannot be known to be unchanged.
  NOT_known_value_unchanged.push_back(P);
  visited.insert(P);

  // For each reg R1 that cannot (w/o recomputing) be known to be unchanged...
  for(int i=0;i<NOT_known_value_unchanged.size();i++)
  {
    int R1 = NOT_known_value_unchanged[i];

    // ... consider each downstream index2 that has index1 in slot2 of its computation (possibly unused).
    foreach(j,access(R1).outputs)
    {
      int R2 = j->first;

      // This one already marked NOT known_value_unchanged
      if (includes(visited, R2)) continue;

      // Since R2 is not known to have identical USED inputs ...
      // ... then it is not known to have identical outputs
      NOT_known_value_unchanged.push_back(R2);
      visited.insert(R2);

      // Since the computation may be different, we don't know if the value has changed.
      access(R2).result.reset();
      memory->clear_call(R2);
    }

    foreach(j,access(R1).call_outputs)
    {
      int R2 = *j;

      // This one already marked NOT known_value_unchanged
      if (includes(visited, R2)) continue;

      // Since R2 is not known to have identical USED inputs ...
      // ... then it is not known to have identical outputs
      NOT_known_value_unchanged.push_back(R2);
      visited.insert(R2);

      // Since the computation may be different, we don't know if the value has changed.
      access(R2).result.reset();
    }
  }
}

/// Update the value of a non-constant, non-computed index
void context::set_parameter_value(const std::string& var, const expression_ref& O)
{
  set_parameter_value(find_parameter(var), O);
}

int context::n_parameters() const
{
  return parameters().size();
}

int context::find_parameter(const string& s) const
{
  for(int i=0;i<n_parameters();i++)
    if (parameter_name(i) == s)
      return i;

    throw myexception()<<"Can't find parameter named '"<<s<<"'";
}

int context::n_variables() const
{
  return variables.size();
}

int context::find_variable(const string& s) const
{
  for(int i=0;i<variables.size();i++)
    if (variables[i].first == s)
      return i;

  return -1;
}

const string& context::variable_name(int i) const
{
  return variables[i].first;
}

int context::add_parameter(const string& name)
{
  assert(name.size() != 0);

  int index = n_parameters();

  root_t r = allocate_reg();
  parameters().push_back( r );

  access(*r).changeable = true;
  set_E(*r, parameter(name) );

  set_parameter_value(index, default_parameter_value(index) );
  
  return index;
}

int reg_heap::n_regs() const
{
  return memory.size();
}

int reg_heap::n_free_regs() const
{
  int here = first_free_reg;
  int count = 0;
  for(;here != -1;here = access(here).next_reg)
    count++;
  return count;
}

int reg_heap::n_used_regs() const
{
  int here = first_used_reg;
  int count = 0;
  for(;here != -1;here = access(here).next_reg)
    count++;
  return count;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const expression_ref& E)
{
  std::cerr<<"add: "<<E->print()<<"\n";

  expression_ref T = graph_normalize(*this, translate_refs(E) );

  root_t r;
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(T))
  {
    assert( includes(access(RV->target).owners, token) );
    
    r = push_root( RV->target );
  }
  else
  {
    r = allocate_reg();
    set_E( *r, T );
  }

  heads().push_back(r);
  return heads().size()-1;
}

/// Add an expression that may be replaced by its reduced form
int context::add_compute_expression(const string& name, const expression_ref& E)
{
  int index = add_compute_expression( E );
  int R = *heads()[index];
  add_variable(name, R);
  return index;
}

int context::n_expressions() const
{
  return heads().size();
}

expression_ref context::get_expression(int i) const
{
  return access(*heads()[i]).E;
}

int reg_heap::add_reg_to_free_list(int r)
{
  clear(r);
  access(r).state = reg::free;
  access(r).prev_reg = -1;
  access(r).next_reg = first_free_reg;
  if (first_free_reg != -1)
    access(first_free_reg).prev_reg = r;
  first_free_reg = r;
  return r;
}

int reg_heap::get_free_reg()
{
  int r = first_free_reg;
  if (r != -1)
  {
    assert(access(r).state == reg::free);
    first_free_reg = access(r).next_reg;
    access(r).prev_reg = -1;
    access(r).next_reg = -1;
    access(r).state = reg::none;
  }
  return r;
}

int reg_heap::add_reg_to_used_list(int r)
{
  access(r).state = reg::used;
  access(r).prev_reg = -1;
  access(r).next_reg = first_used_reg;
  if (first_used_reg != -1)
    access(first_used_reg).prev_reg = r;
  first_used_reg = r;
  return r;
}

void reg_heap::remove_reg_from_used_list(int r)
{
  int P = access(r).prev_reg;
  int N = access(r).next_reg;

  if (P == -1)
    first_used_reg = N;
  else
    access(P).next_reg = N;

  if (N == -1)
    ;
  else
    access(N).prev_reg = P;

  access(r).state = reg::none;
}

void reg_heap::reclaim_used_reg(int r)
{
  // FIXME - we need to carefully clear references to things that might reference us back.. don't we?

  // Downstream objects could still exist
  clear_used_inputs(r);
  clear_call(r);
  clear_E(r);

  remove_reg_from_used_list(r);
  add_reg_to_free_list(r);
}

reg_heap::root_t reg_heap::push_root(int R)
{
  assert(0 <= R and R < n_regs());
  return roots.insert(roots.end(), R);
}

void reg_heap::pop_root(reg_heap::root_t r)
{
  roots.erase(r);
}

void reg_heap::expand_memory(int s)
{
  assert(n_regs() == n_used_regs() + n_free_regs());

  int k = memory.size();
  memory.resize(memory.size()+s);
  for(int i=k;i<memory.size();i++)
    add_reg_to_free_list(i);

  assert(n_regs() == n_used_regs() + n_free_regs());
}

reg_heap::root_t reg_heap::allocate_reg()
{
  assert(n_regs() == n_used_regs() + n_free_regs());

  int r = get_free_reg();

  // allocation failed
  if (r == -1)
  {
    collect_garbage();
    assert(n_used_regs() + n_free_regs() == n_regs());
    if (memory.size() < n_used_regs()*2+10)
      expand_memory(memory.size()*2+10);
    r = get_free_reg();
    assert(r != -1);
  }

  add_reg_to_used_list(r);

  assert(n_regs() == n_used_regs() + n_free_regs());
  assert(access(r).state == reg::used);

  root_t root = roots.insert(roots.end(), r);

  return root;
}

reg_heap::root_t context::push_temp_head() const
{
  temp_heads().push_back( allocate_reg() );
  return temp_heads().back();
}

void context::pop_temp_head() const
{
  root_t head = temp_heads().back();
  temp_heads().pop_back();
  pop_root(head);
}

void context::collect_garbage() const
{
  memory->collect_garbage();
}

void reg_heap::remove_unused_ownership_marks()
{
  // Clear ownership marks
  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    R.owners.clear();

    here = R.next_reg;
  }

  // Mark ownership on regs according to reachability.
  for(int t=0;t<get_n_tokens();t++)
  {
    // Don't compute reachability from unused tokens.
    if (not token_is_used(t)) continue;

    // Find the all the regs reachable from heads in t
    vector<int> regs = find_all_regs_in_context(t);

    // Mark regs reachable in t as being owned by t
    for(int i=0;i<regs.size();i++)
    {
      int R = regs[i];
      access(R).owners.insert(t);
    }
  }

  // Check that we have no un-owned objects that are used
  here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    assert(not R.owners.empty());

    here = R.next_reg;
  }
}

void reg_heap::collect_garbage()
{
  std::cerr<<"***********Garbage Collection******************"<<std::endl;
  assert(n_regs() == n_used_regs() + n_free_regs());

  vector<int> scan;
  foreach(i,roots)
    scan.push_back(*i);

  while (not scan.empty())
  {
    vector<int> next_scan;
    for(int i=0;i<scan.size();i++)
    {
      reg& R = access(scan[i]);
      assert(R.state != reg::free);
      if (R.state == reg::checked) continue;

      R.state = reg::checked;

      // Make sure that we have already correctly got all the references!
      assert(get_exp_refs(R.E) == R.references);
   
      // Count the references from E
      next_scan.insert(next_scan.end(), R.references.begin(), R.references.end());

      // Count also the references from the call
      if (R.call != -1) 
	next_scan.insert(next_scan.end(), R.call);
    }
    scan = next_scan;
  }

  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    int next = access(here).next_reg;
    if (R.state == reg::checked)
      R.state = reg::used;
    else 
      reclaim_used_reg(here);

    here = next;
  }

  remove_unused_ownership_marks();
}

int reg_heap::get_unused_token()
{
  if (unused_tokens.empty())
  {
    unused_tokens.push_back(get_n_tokens());
    token_roots.push_back(graph_roots());
  }

  int t = unused_tokens.back();
  unused_tokens.pop_back();

  assert(not token_is_used(t));

  token_roots[t].used = true;

  return t;
}

vector<int> reg_heap::find_call_ancestors_in_context(int R,int t) const
{
  vector<int> ancestors;

  // Add the call parents of R
  foreach (i, access(R).call_outputs)
  {
    int Q = *i;
    assert(access(Q).state == reg::used);

    access(Q).state = reg::checked;

    assert(reg_is_owned_by(R,t) and not reg_is_shared(R));
    ancestors.push_back(Q);
  }

  // Recursively add the call parents
  for(int i=0;i<ancestors.size();i++)
  {
    int Q1 = ancestors[i];

    assert(access(Q1).state == reg::checked);

    foreach(j, access(Q1).call_outputs)
    {
      int Q2 = *j;

      // Skip regs that have been seen before.
      if (access(Q2).state == reg::checked) continue;

      assert( access(Q2).state == reg::used);

      assert(reg_is_owned_by(R,t) and not reg_is_shared(R));

      ancestors.push_back(Q2);
    }
  }

  // Reset the mark
  for(int i=0;i<ancestors.size();i++)
    access(ancestors[i]).state = reg::used;

  return ancestors;
}

vector<int> reg_heap::find_shared_ancestor_regs_in_context(int R, int t) const
{
  vector<int> scan;
  scan.push_back(R);
  assert(reg_is_owned_by(R,t));

  vector<int> unique;
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);

    // Regs should be on the used list
    assert(R.state != reg::free and R.state != reg::none);

    // Only consider each reg at most once
    if (R.state == reg::checked) continue;

    // A reg must have at least 1 owner.
    assert(not R.owners.empty());

    // Skip this node if its not in context t
    if (not reg_is_owned_by(scan[i],t)) continue;

    // Skip this node if its already unique
    if (not reg_is_shared(scan[i])) continue;

    R.state = reg::checked;
    unique.push_back(scan[i]);

    // Count the references from E in other regs
    scan.insert(scan.end(), R.referenced_by_in_E.begin(), R.referenced_by_in_E.end());

    // Count the references from calls by other regs
    scan.insert(scan.end(), R.call_outputs.begin(), R.call_outputs.end());
  }

  for(int i=0;i<unique.size();i++)
  {
    const reg& R = access(unique[i]);
    assert(R.state == reg::checked);

    R.state = reg::used;
  }

  return unique;
}

expression_ref remap_regs(const expression_ref R, const map<int, reg_heap::root_t>& new_regs)
{
  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    bool different = false;
    shared_ptr<expression> E2 ( new expression );
    E2->sub.resize(E->size());
    for(int i=0;i<E->size();i++)
    {
      E2->sub[i] = remap_regs(E->sub[i], new_regs);
      if (E2->sub[i] != E->sub[i])
	different = true;
    }
    if (different)
      return shared_ptr<const expression>(E2);
    else
      return R;
  }
  else if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(R))
  {
    map<int, reg_heap::root_t>::const_iterator loc = new_regs.find(RV->target);
    if (loc == new_regs.end())
      return R;
    else
      return new reg_var(*loc->second);
  }
  // This case handles NULL in addition to atomic objects.
  else
    return R;
}

bool reg_heap::reg_is_shared(int R) const
{
  const std::set<int>& owners = access(R).owners;

  std::set<int>::const_iterator loc = owners.begin();

  assert(loc != owners.end());

  loc++;

  return (loc != owners.end());
}

bool reg_heap::reg_is_owned_by(int R, int t) const
{
  const std::set<int>& owners = access(R).owners;

  return includes(owners, t);
}

int remap(int R, const map<int,reg_heap::root_t>& new_regs)
{
  map<int,reg_heap::root_t>::const_iterator loc = new_regs.find(R);
  if (loc == new_regs.end())
    return R;
  else
    return *loc->second;
}

int reg_heap::uniquify_reg(int R, int t)
{
  // If the reg is already unique, then we don't need to do anything.
  if (not reg_is_shared(R))
  {
    assert(reg_is_owned_by(R,t));
    return R;
  }

  vector<int> changed_results;

  // 1. Find all ancestors with name 't' that are *shared*
  vector<int> shared_ancestors = find_shared_ancestor_regs_in_context(R,t);

  // 2. Split these shared regs, and copy the old contents over, remapping as we go.
  map<int,root_t> new_regs;
  for(int i=0;i<shared_ancestors.size();i++)
  {
    int R1 = shared_ancestors[i];
    // 2. Allocate new regs for each ancestor reg
    root_t root = allocate_reg();
    int R2 = *root;
    new_regs[R1] = root;

    // Check no mark on R2
    assert(access(R2).state == reg::used);
    
    // 3. Move ownership from the old regs to the new regs.
    access(R1).owners.erase(t);
    access(R2).owners.insert(t);
    
    assert( not access(R1).owners.empty() );

    // 4. Initialize fields in the new node

    // 4a. Initialize/Remap E
    set_E(R2, remap_regs( access(R1).E, new_regs) );

    // 4b. Initialize/Remap call
    if (access(R1).call != -1)
      set_call(R2, remap(access(R1).call, new_regs) );

    // 4c. Initialize/Remap used_inputs
    access(R2).used_inputs = std::vector<int>( access(R1).used_inputs.size() , -1);
    for(int slot=0;slot<access(R1).used_inputs.size();slot++)
    {
      int I = access(R1).used_inputs[slot];
      if (I == -1) continue;
      
      set_used_input(R2, slot, remap(I, new_regs) );
    }

    // 4d. Initialize/Remap result if E is in WHNF.
    if (access(R2).call == -1 and access(R1).result)
    {
      access(R2).result = access(R2).E;
      changed_results.push_back(R2);
    }
    // 4d. Initialize/Copy result otherwise.
    else
      access(R2).result = access(R1).result;

    // 4e. Initialize/Copy changeable
    access(R2).changeable = access(R1).changeable;
  }

  // 4a. Adjust heads to point to the new regs
  for(int j=0;j<token_roots[t].heads.size();j++)
  {
    int R1 = *token_roots[t].heads[j];
    if (includes(new_regs, R1))
      *token_roots[t].heads[j] = *new_regs[R1];
  }

  // 4b. Adjust parameters to point to the new regs
  for(int j=0;j<token_roots[t].parameters.size();j++)
  {
    int R1 = *token_roots[t].parameters[j];
    if (includes(new_regs, R1))
      *token_roots[t].parameters[j] = *new_regs[R1];
  }

  // 5. Find the unsplit parents of split regs
  //    These will be the only parents of the old regs that have context t.
  vector<int> unsplit_parents;
  foreach(i,new_regs)
  {
    int R1 = i->first;

    foreach(j,access(R1).referenced_by_in_E)
    {
      int Q1 = *j;

      // Skip regs that we've handled already.
      if (access(Q1).state == reg::checked) continue;

      // We are only interested in the E-ancestors in t.
      if (not reg_is_owned_by(Q1, t)) continue;

      // This reg is a parent of a split reg, but is not split, and so must not be shared itself.
      assert(not reg_is_shared(Q1));

      // Mark Q1
      assert(access(Q1).state == reg::used);
      access(Q1).state = reg::checked;
      unsplit_parents.push_back(Q1);
    }
  }

  // Unmark the unsplit parents;
  for(int i=0;i<unsplit_parents.size();i++)
    access(unsplit_parents[i]).state = reg::used;

  // Check that marks were removed.
  foreach(i,new_regs)
  {
    int R1 = i->first;
    int R2 = *(i->second);

    // Original nodes should never have been marked.
    assert( access(R1).state == reg::used );

    // Split nodes should not have been marked.
    assert( access(R2).state == reg::used );

    // The split nodes should now be E-ancestors in t
    foreach(j,access(R2).referenced_by_in_E)
    {
      assert( access(R2).state == reg::used );
    }

    // The split nodes should now be E-ancestors in t
    foreach(j,access(R1).referenced_by_in_E)
    {
      assert( access(R2).state == reg::used );
    }
  }
  
  for(int i=0;i<unsplit_parents.size();i++)
  {
    int Q1 = unsplit_parents[i];

    // a. Remap E
    set_E(Q1, remap_regs(access(Q1).E, new_regs) );
    
    // b. Remap call
    if (access(Q1).call != -1)
    {
      int old_call = access(Q1).call;
      int new_call = remap( old_call , new_regs);

      if (old_call != new_call)
      {
	clear_call(Q1);
	set_call(Q1, new_call);
      }
    }
    
    // c. Adjust use edges
    for(int slot=0;slot<access(Q1).used_inputs.size();slot++)
    {
      int I1 = access(Q1).used_inputs[slot];
      if (I1 == -1) continue;
      
      int I2 = remap(I1, new_regs);
      if (I1 == I2) continue;
      
      clear_used_input(Q1, slot);
      set_used_input(Q1, slot, I2);
    }
    
    // d. Remap result if E is in WHNF
    if (access(Q1).call != -1 and access(Q1).result)
    {
      access(Q1).result = access(Q1).E;
      changed_results.push_back(Q1);
    }
  }


  // update the call outputs
  for(int i=0;i<changed_results.size();i++)
  {
    int Q = changed_results[i];

    expression_ref result = access(Q).result;

    vector<int> regs = find_call_ancestors_in_context( Q, t);

    for(int j=0;j<regs.size();j++)
      access(regs[j]).result = result;
  }

  int R2 = *new_regs[R];

  // 5. Remove root references to new regs.
  //    Remove t-ownership from old regs.
  foreach(i,new_regs)
  {
    pop_root(i->second);
  }

  return R2;
}

vector<int> reg_heap::find_all_regs_in_context(int t) const
{
  vector<int> scan;
  foreach(i,token_roots[t].temp)
    scan.push_back(**i);
  foreach(i,token_roots[t].heads)
    scan.push_back(**i);
  foreach(i,token_roots[t].parameters)
    scan.push_back(**i);

  vector<int> unique;
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);
    assert(R.state != reg::free and R.state != reg::none);
    if (R.state == reg::checked) continue;

    R.state = reg::checked;
    unique.push_back(scan[i]);

    // Make sure that we have already correctly got all the references!
    assert(get_exp_refs(R.E) == R.references);
    
    // Count the references from E
    scan.insert(scan.end(), R.references.begin(), R.references.end());
    
    // Count also the references from the call
    if (R.call != -1) 
      scan.insert(scan.end(), R.call);
  }

  for(int i=0;i<unique.size();i++)
  {
    const reg& R = access(unique[i]);
    assert(R.state == reg::checked);

    R.state = reg::used;
  }

  return unique;
}

void reg_heap::release_token(int t)
{
  assert(token_is_used(t));

  // remove ownership mark from used regs in this context
  vector<int> token_regs = find_all_regs_in_context(t);
  for(int i=0;i<token_regs.size();i++)
    access(token_regs[i]).owners.erase(t);

  // We shouldn't have any temporary heads still on the stack, here!
  assert(token_roots[t].temp.empty());

  // remove the roots for the temporary heads of graph t
  foreach(i,token_roots[t].temp)
  {
    pop_root(*i);
  }
  token_roots[t].temp.clear();

  // remove the roots for the heads of graph t
  foreach(i,token_roots[t].heads)
  {
    pop_root(*i);
  }
  token_roots[t].heads.clear();

  // remove the roots for the parameters of graph t
  foreach(i,token_roots[t].parameters)
  {
    pop_root(*i);
  }
  token_roots[t].parameters.clear();

  // mark unused
  unused_tokens.push_back(t);
  token_roots[t].used = false;
}

bool reg_heap::token_is_used(int t) const
{
  return token_roots[t].used;
}

int reg_heap::copy_token(int t)
{
  int t2 = get_unused_token();

  foreach(i,token_roots[t].heads)
  {
    token_roots[t2].heads.insert( token_roots[t2].heads.end(), push_root(**i) );
  }

  foreach(i,token_roots[t].parameters)
  {
    token_roots[t2].parameters.insert( token_roots[t2].parameters.end(), push_root(**i) );
  }

  // remove ownership mark from used regs in this context
  vector<int> token_regs = find_all_regs_in_context(t2);
  for(int i=0;i<token_regs.size();i++)
  {
    std::set<int>& owners = access(token_regs[i]).owners;
    //    assert( not includes(owners, t2) );
    owners.insert(t2);
  }

  return t2;
}

reg_heap::reg_heap()
  :first_free_reg(-1),
   first_used_reg(-1)
{ }

expression_ref context::translate_refs(const expression_ref& R) const
{
  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (shared_ptr<const parameter> P = dynamic_pointer_cast<const parameter>(R))
  {
    int param_index = find_parameter(P->parameter_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<P->parameter_name<<"' in expression!";

    int param_location = *parameters()[param_index];

    return expression_ref(new reg_var(param_location) );
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (shared_ptr<const var> V = dynamic_pointer_cast<const var>(R))
  {
    int loc = find_variable(V->name);
    if (loc == -1)
      throw myexception()<<"Can't translate undefined variable '"<<V->name<<"' in expression!";

    int R = variables[loc].second;

    return expression_ref(new reg_var(R) );
  }

  // Other constants have no parts, and don't need to be translated
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);
  if (not E) return R;

  // Translate the parts of the expression
  expression_ref R2 = R;
  shared_ptr<expression> V ( new expression(*E) );
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i]);

  return shared_ptr<const expression>(V);
}

int context::find_match_notes(const expression_ref& query, std::vector<expression_ref>& results, int start) const
{
  assert(start >= 0);
  for(int i=start;i<n_notes();i++)
  {
    results.clear();
    if (find_match(query, get_note(i), results))
      return i;
  }
  return -1;
}

context& context::operator=(const context& C)
{
  memory->release_token(token);
  
  memory = C.memory;
  token = memory->copy_token(C.token);
  variables = C.variables;
  notes     = C.notes;

  return *this;
}

context::context()
  :memory(new reg_heap()),
   token(memory->get_unused_token())
{ }

context::context(const context& C)
  :memory(C.memory),
   token(memory->copy_token(C.token)),
   variables(C.variables),
   notes(C.notes)
{ }

context::~context()
{
  memory->release_token(token);
}

shared_ptr<const Object> context::default_parameter_value(int i) const
{
  expression_ref default_value = lambda_expression(data_function("default_value",2));

  vector<expression_ref> results;
  expression_ref query = default_value( parameter( parameter_name(i) ) )(match(0));
  int found = find_match_notes(query, results, 0);

  if (found != -1)
  {
    assert(results.size());
    return results[0];
  }
  else
    return shared_ptr<const Object>();
}

context::context(const vector<expression_ref>& N)
  :memory(new reg_heap()),
   token(memory->get_unused_token()),
   notes(N)
{
  std::set<string> names = find_named_parameters(notes);
  
  // Then set all default values.
  foreach(i,names)
    add_parameter(*i);
 }

expression_ref graph_normalize(const context& C, const expression_ref& R)
{
  if (not R) return R;

  // 1. Var
  if (is_dummy(R))
    return R;
  
  shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R);

  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E) return R;
  
  // 2. Lambda
  shared_ptr<const lambda> L = dynamic_pointer_cast<const lambda>(E->sub[0]);
  if (L)
  {
    assert(E->size() == 3);
    shared_ptr<expression> V ( new expression(*E) );
    V->sub[2] = graph_normalize(C,E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return shared_ptr<const expression>(V);
  }

  // 3. Application
  if (dynamic_pointer_cast<const Apply>(E->sub[0]))
  {
    assert(E->size() == 3);
    expression_ref f = graph_normalize(C,E->sub[1]);
    expression_ref x = graph_normalize(C,E->sub[2]);

    int var_index = get_safe_binder_index(R);
    expression_ref f_ = dummy(var_index++);
    expression_ref x_ = dummy(var_index++);

    if (is_dummy(x))
    { 
      return let_expression(f_, f, apply_expression(f_,x));
    }
    else
    {
      vector<expression_ref> vars;
      vector<expression_ref> bodies;

      vars.push_back(f_);
      vars.push_back(x_);

      bodies.push_back(f);
      bodies.push_back(x);

      return let_expression(vars, bodies, apply_expression(f_,x_));
    }
  }

  // 6. Case
  shared_ptr<const Case> IsCase = dynamic_pointer_cast<const Case>(E->sub[0]);
  if (IsCase)
  {
    shared_ptr<expression> V ( new expression(*E) );

    V->sub[1] = graph_normalize(C,V->sub[1]);

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[2]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> alternative = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(alternative);
      alternative->sub[2] = graph_normalize(C,alternative->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    if (is_dummy(V->sub[1]))
      return shared_ptr<const expression>(V);
    else
    {
      int var_index = get_safe_binder_index(R);
      expression_ref x = dummy(var_index);
      expression_ref obj = V->sub[1];
      V->sub[1] = x;

      return let_expression(x,obj,shared_ptr<const expression>(V));
    }
  }

  // 4. Constructor
  if (dynamic_pointer_cast<const Function>(E->sub[0]) or 
      dynamic_pointer_cast<const Operation>(E->sub[0]))
  {
    int var_index = get_safe_binder_index(R);

    shared_ptr<expression> Con ( new expression );
    Con->sub.push_back(E->sub[0]);

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=1;i<E->size();i++)
    {
      if (is_dummy(E->sub[i]))
      {
	Con->sub.push_back(E->sub[i]);
      }
      else
      {
	expression_ref var = dummy( var_index++ );
	Con->sub.push_back( var );
	vars.push_back( var );
	bodies.push_back( graph_normalize(C,E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, shared_ptr<const expression>(Con));
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    shared_ptr<expression> V ( new expression(*E) );

    shared_ptr<expression> bodies = dynamic_pointer_cast<expression>(V->sub[1]);
    while(bodies)
    {
      assert(bodies->size() == 3);
      shared_ptr<expression> let_group = dynamic_pointer_cast<expression>(bodies->sub[1]);
      assert(let_group);
      let_group->sub[2] = graph_normalize(C,let_group->sub[2]);
      bodies = dynamic_pointer_cast<expression>(bodies->sub[2]);
    }
    
    V->sub[2] = graph_normalize(C,V->sub[2]);

    return shared_ptr<const expression>(V);
  }

  throw myexception()<<"graph_normalize: I don't recognize expression '"+ R->print() + "'";
}

#include "computation.H"

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
struct RegOperationArgs: public OperationArgs
{
  shared_ptr<const expression> E;

  const int R;

  const context& C;

  boost::shared_ptr<const Object> reference(int slot) const
  {
    return E->sub[slot+1];
  }

  /*
   * NOTE: We cannot cache full_evaluate( ) results, because a WHNF constructor expression
   *       does not USE the result of its arguments, and so will not be marked for
   *       recomputation when they change.
   */

  boost::shared_ptr<const Object> evaluate(int slot)
  {
    // Any slot that we are going to evaluate needs to point to another node
    shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( reference(slot) );
    assert(RV);
    int R2 = RV->target;

    if (not C[R].used_inputs[slot] != -1)
    {
      // evaluate R
      R2 = incremental_evaluate(C, R2);

      // Adjust the reference, if it changed.
      if (R2 != RV->target)
      {
	expression_ref E2 = C[R].E;
	dynamic_pointer_cast<expression>(E2)->sub[slot+1] = new reg_var(R2);
	C.set_E(R, E2);
      }

      // mark R2 used by R in the correct slot
      C.set_used_input(R, slot, R2);

      // If R2 -> result was changeable, then R -> result will be changeable as well.
      if (C[R2].changeable) 
	C[R].changeable = true;
    }

    return full_evaluate(C,R2);
  }

  boost::shared_ptr<const Object> lazy_evaluate(int slot)
  {
    // Any slot that we are going to evaluate needs to point to another node
    shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( reference(slot) );
    assert(RV);
    int R2 = RV->target;

    if (not C[R].used_inputs[slot] != -1)
    {
      // evaluate R
      R2 = incremental_evaluate(C, R2);

      // Adjust the reference, if it changed.
      if (R2 != RV->target)
      {
	expression_ref E2 = C[R].E;
	dynamic_pointer_cast<expression>(E2)->sub[slot+1] = new reg_var(R2);
	C.set_E(R, E2);
      }

      // mark R2 used by R in the correct slot
      C.set_used_input(R, slot, R2);

      // If R2 -> result was changeable, then R -> result will be changeable as well.
      if (C[R2].changeable) 
	C[R].changeable = true;
    }

    return C[R2].result;
  }

  shared_ptr<const Object> evaluate_expression(const expression_ref& e)
  {
    return C.evaluate_expression(e);
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, const context& c)
    :R(r),C(c)
  { 
    // The object we are evaluating had better be a class expression (with parts).
    E = dynamic_pointer_cast<const expression>(C[R].E);
    assert(E);

    C.clear_used_inputs(R, E->size()-1);
  }
};

expression_ref compact_graph_expression(const context& C, const expression_ref& R);

  /*
   * eval r: p[r] = E
   * 
   *   if p[r] = E => F then
   *      return r.
   *  
   *   else if p[r] = E has a call to p[s] then
   *      s = eval s
   *      p[r] = E => (result of p[s])
   *      if p[r] is not changeable
   *         r = s
   *      return r;
   *  
   *   else if E is WHNF then
   *      p[r] = E => E
   *
   *   else if E is a variable p[s] then
   *      p[r] = p[s] => p[s]
   *
   *   else if E is a parameter then the result must already be set
   *      p[r] = parameter => R
   *     
   *   else
   *      assert(E is not a parameter)
   *      reduce E -> (F,changable)         [operations + let expression]
   *      if E->F is unchangeable
   *         p[r] = F
   *         restart
   *      else
   *         if (F is a variable p[s])
   *            p[r] = E => p[s]
   *         else if (F is WHNF)
   *            p[r] = E => F
   *         else
   *            s = allocate new reg
   *            p[r] = E => p[s]
   *         end if
   *   end if
   *
   *   Cases at this point:
   *   - NOT p[r] = parameter => E
   *   - NOT p[r] = parameter => p[s]
   *   -     p[r] = E => E
   *   -     p[r] = p[s] => p[s]
   *   -     p[r] = E => p[s] @ changeable
   *   -     p[r] = E => F    @ changeable
   *   - NOT p[r] = E => p[s] 
   *     --> p[r] = p[s] / restart
   *   - NOT p[r] = E => F 
   *     --> p[r] = F    / restart
   *
   *   if p[r] = E => p[s] then
   *      if (not changeable)
   *         r = s
   *      else
   *         set_call(r,s)
   *         restart
   *
   *   Cases at this point:
   *
   *   - NOT p[r] = parameter => E
   *   - NOT p[r] = parameter => p[s]
   *   -     p[r] = E => E
   *   - NOT p[r] = p[s] => p[s]
   *     --> p[r] = p[s] -> call s
   *   - NOT p[r] = E => p[s] @ changeable
   *     --> p[r] = p[s] -> call s
   *   -     p[r] = E => F    @ changeable
   *   - NOT p[r] = E => p[s] 
   *     --> p[r] = p[s] / restart
   *   - NOT p[r] = E => F 
   *     --> p[r] = F    / restart
   */

/// Evaluate C[R] and return a reg containing the results that looks through unchangeable redirections = reg_var chains
int incremental_evaluate(const context& C, int R)
{
  assert(R >= 0 and R < C.n_regs());
  assert(C[R].state == reg::used);
  assert(get_exp_refs(C.access(R).E) == C.access(R).references);
  assert(includes(C.access(R).owners, C.get_token()));

  if (not C[R].result) std::cerr<<"Statement: "<<R<<":   "<<C[R].E->print()<<std::endl;

  while (not C[R].result)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    std::cerr<<"   statement: "<<R<<":   "<<C[R].E->print()<<std::endl;
    assert(get_exp_refs(C.access(R).E) == C.access(R).references);

    // If we know what to call, then call it and use it to set the result
    if (C[R].call != -1)
    {
      // Evaluate C[S], looking through unchangeable redirections
      int S = incremental_evaluate(C, C[R].call);

      // R gets its result from S.
      C[R].result = C[S].result;

      // If C[R].call can be evaluated to refer to S w/o moving through any changable operations, 
      // then it should be safe to change C[R].call to refer to S, even if R is changeable.
      C[R].call = S;

      // However, we can only update R to refer to S if R itself isn't changeable.
      if (not C[R].changeable)
	R = S;
    }

    /*---------- Below here, there is no call, and no result. ------------*/

    // Check if E is a reference to a heap variable
    else if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(C[R].E))
      C.set_call(R, RV->target);

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(C[R].E))
      C[R].result = C[R].E;

    // A parameter has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (shared_ptr<const parameter> p = dynamic_pointer_cast<const parameter>(C[R].E))
      throw myexception()<<"Parameter with no result?! (Changeable = "<<C[R].changeable<<")";

    
    // Reduction: let expression
    else if (parse_let_expression(C[R].E, vars, bodies, T))
    {
      vector<shared_ptr<reg_var> > new_reg_vars;
      for(int i=0;i<vars.size();i++)
      {
	int V = *C.push_temp_head();
	// Don't set ownership here, where it could be cleared by further allocate()s.
	new_reg_vars.push_back( shared_ptr<reg_var>(new reg_var(V)) );
      }
      
      // Substitute the new heap vars for the dummy vars in expression T and in the bodies
      for(int i=0;i<vars.size();i++) 
      {
	// if the body is already a reg_var, let's not add a new reg_var just to point to it!
	expression_ref replacement_reg_var = new_reg_vars[i]->clone();
	if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(bodies[i]))
	  replacement_reg_var = bodies[i];

	for(int j=0;j<vars.size();j++)
	  bodies[j] = substitute(bodies[j], vars[i], *replacement_reg_var);
	
	T = substitute(T, vars[i], *replacement_reg_var);
      }
      
      assert(not C[R].changeable);

      for(int i=0;i<vars.size();i++) 
      {
	int V = new_reg_vars[i]->target;

	// Set ownership here, where it will not be cleared by futher allocate calls.
	C.access(V).owners = C.access(R).owners;
	// Set the bodies of the new reg_vars
	C.set_E(V , bodies[i]);
      }

      C.set_E(R, T);

      // Remove the new heap vars from the list of temp heads in reverse order.
      for(int i=0;i<new_reg_vars.size(); i++)
	C.pop_temp_head();
      
      assert(C[R].call == -1);
      assert(not C[R].result);
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(C[R].E);
      assert(E);
      
      shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(E->sub[0]);
      assert(O);

      // Although the reg itself is not a parameter, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not C[R].changeable);" here.

      RegOperationArgs Args(R, C);
      expression_ref result = (*O)(Args);

      // Check that the result of applying the operation only uses regs referenced from E.
      for(int j=0;j<C.access(R).used_inputs.size();j++)
	if (C.access(R).used_inputs[j] != -1)
	  assert( includes(C.access(R).references, C.access(R).used_inputs[j]) );

      // If the reduction doesn't depend on parameters, then replace E with the result.
      if (not C[R].changeable)
      {
	// The old used_input slots are not invalid, which is OK since none of them are changeable.
	assert(C.access(R).call == -1);
	assert(not C.access(R).result);
	C.clear_used_inputs(R);
	C.set_E(R, result);
      }
      // Otherwise, set the reduction result.
      else
	C.set_reduction_result(R, result );

#ifndef NDEBUG
      // std::cerr<<"Executing statement: "<<compact_graph_expression(C,E)<<"\n";
      std::cerr<<"Executing operation: "<<O<<"\n";
      std::cerr<<"Result changeable: "<<C[R].changeable<<"\n\n"<<endl;
#endif
    }
  }

#ifndef NDEBUG
  //  std::cerr<<"Result = "<<compact_graph_expression(*C[R].result)<<"\n";
  //  std::cerr<<"Result changeable: "<<C[R].changeable<<"\n\n";
#endif

  assert(C[R].result);
  assert(is_WHNF(C[R].result));
  assert(not dynamic_pointer_cast<const reg_var>(C[R].result));

  return R;
}

void discover_graph_vars(const context& C, const expression_ref& R, map< int, std::string>& names)
{
  if (shared_ptr<const reg_var> H = dynamic_pointer_cast<const reg_var>(R))
  {
    if (includes( names, H->target))
    {
      // back out, we've been through this node before.
    }

    if (not includes(names, H->target))
    {
      int num = names.size()+1;
      // give this node a name and mark it visited
      names[H->target] = "p"+convertToString(num);

      discover_graph_vars(C, C[H->target].E, names);
    }

  }

  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    for(int i=0;i<E->size();i++)
      discover_graph_vars(C, E->sub[i], names);
  }
}

expression_ref compact_graph_expression(const context& C, const expression_ref& R_)
{
  //  return R_;
  expression_ref R = R_;
  map< int, std::string> names;

  int var_index = get_safe_binder_index(R);

  discover_graph_vars(C, R,names);

  //  std::cerr<<R<<std::endl;
  vector< expression_ref > replace;
  foreach(i,names)
  {
    replace.push_back( reg_var( i->first) );
    var_index = std::max(var_index, get_safe_binder_index(C[i->first].E) );
    //    std::cerr<<"<"<<i->first->name<<"> = "<<i->first->E<<std::endl;
  }
  //  std::cerr<<R<<std::endl;
  vector<expression_ref> vars;
  vector<expression_ref> bodies;
  foreach(i,names)
  {
    vars.push_back(dummy(var_index++));
    bodies.push_back( C[i->first].E );
  }

  for(int i=0;i<bodies.size();i++)
  {
    //    std::cerr<<"------\n";
    //    std::cerr<<replace[i]<<" -> "<<vars[i]<<":\n";
    for(int j=0;j<bodies.size();j++)
    {
      bodies[j] = substitute(bodies[j], replace[i], vars[i]);
      //      std::cerr<<vars[j]<<" = "<<bodies[j]<<std::endl;
    }

    R = substitute(R, replace[i], vars[i]);
    //    std::cerr<<"R = "<<R<<std::endl;
  }

  R = let_expression(vars, bodies, R);
  //  std::cerr<<R<<std::endl;
  R = launchbury_unnormalize(R);
  //  std::cerr<<"substituted = "<<launchbury_unnormalize(R)<<std::endl;
  return R;
}

vector<expression_ref> add_prefix(const string& prefix, const vector<expression_ref>& notes)
{
  vector<expression_ref> notes2(notes.size());
  for(int i=0;i<notes.size();i++)
    notes2[i] = add_prefix(prefix, notes[i]);
  return notes2;
}

boost::shared_ptr<context> prefix_formula(const std::string& prefix, const boost::shared_ptr<const context>& C)
{
  shared_ptr<context> C2(C->clone());
  // prefix the parameter names
  for(int i=0;i<C2->n_parameters();i++)
    C2->rename_parameter(i, prefix + "::" + C2->parameter_name(i));

  // prefix the variable names
  for(int i=0;i<C2->n_variables();i++)
    C2->rename_variable(i, prefix + "::" + C2->variable_name(i));

  // prefix the names in the model
  C2->get_notes() = add_prefix(prefix, C2->get_notes());
  return C2;
}

vector<expression_ref> combine(const vector<expression_ref>& N1, const vector<expression_ref>& N2)
{
  vector<expression_ref> N3 = N1;
  N3.insert(N3.end(), N2.begin(), N2.end());
  return N3;
}

std::ostream& operator<<(std::ostream& o, const context& C)
{
  for(int index = 0;index < C.n_expressions(); index++)
  {
    o<<index<<" "<<C.get_expression(index);
    o<<"\n";
  }
  return o;
}
