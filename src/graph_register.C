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


/*
 * If a reg is shared, then all of its descendants must be shared as well.  (Here, "descendants" means
 * the terms in E, which determine the computation to be done.)  Therefore, we have a single version of edges in
 * - E
 * - used_inputs
 * - call
 * - changeable
 * Since these terms describe the computation.
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
 * 3. Splitting a node.
 * When splitting a node, we must necessarily split all nodes from which it is transitively
 *  reachable through references in E.
 * 
 * 1. Find all the ancestors with name N -> regs.
 * 2. Allocate names for them -> new_regs.
 * 3. Move ownership in N from the old regs to the new regs.
 * 4. We then adjust the relevant heads to point to the new regs.
 * 5. Finally, we adjust the expressions (as well as the values/calls of parameters!)
 *    (a) map references in E to the new references.
 *    (b) map references in call to the new references.
 *    (c) map references in result to the new references.
 *    (d) changeable should stay the same.
 *    Q: Do we really want to map result and call? we could also just invalidate them.
 *    A: Yes, some of these things may not have USED the changed parameter, just REFERENCED iit.
 *       That means that, in theory, they COULD use it, in the future, and therefore must be
 *        modified.  But, as of yet, that has not happened.
 */

/*
 * It would indeed be faster, when setting a NUMBER of values to remember which parameters are dirty,
 * and to only invalidate downstream nodes when we know that we are going to access them.
 * This is essentially what Andrew and Alexei are doing by requiring the user to check the dirtiness
 *  instead of forcing it do be done as soon as a parameter value is changed.
 */

/*
 * Still todo: (1) splitting graphs. (2) Propagating ownership.
 *
 *
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

  // fixme
  if (access(R1).used_inputs[slot] != -1) return;

  access(R1).used_inputs[slot] = R2;
  access(R2).outputs.insert(pair<int,int>(R1,slot));
}

void reg_heap::clear_used_input(int R, int slot)
{
  int R2 = access(R).used_inputs[slot];
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());

  // FIXME - we should treat different slots differently
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
bool includes(const std::map<T,U>& s1, const T& t)
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

/// Return the value of a particular index, computing it if necessary
shared_ptr<const Object> context::evaluate(int index) const
{
  int& H = *heads()[index];

  H = incremental_evaluate(*this, H);

  return access(H).result;
}

expression_ref graph_normalize(const context&, const expression_ref&);

shared_ptr<const Object> context::evaluate_expression(const expression_ref& E) const
{
  root_t r = allocate_reg();
  int& R = *r;
  set_E(R, graph_normalize(*this, translate_refs(E)) );

  R = incremental_evaluate(*this,R);
  shared_ptr<const Object> result = access(R).result;
  pop_root(r);

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

void context::set_call_if_reg_result(int R) const
{
  if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(access(R).result))
  {
    int R2 = RV->target;
    
    assert(0 <= R2 and R2 < n_regs());
    assert(access(R2).state == reg::used);

    // clear the result slot
    access(R).result.reset();
    
    set_call(R,R2);
  }
}

/// Update the value of a non-constant, non-computed index
void context::set_reg_value(int P, const expression_ref& OO)
{
  memory->uniquify_reg(P,token);

  expression_ref O = graph_normalize(*this, translate_refs(OO));

  assert(dynamic_pointer_cast<const parameter>(access(P).E));
  assert(access(P).changeable);
  memory->clear_call(P);

  if (not is_WHNF(O))
  {
    root_t r = allocate_reg();
    set_E(*r, O);
    O = expression_ref( new reg_var(*r) );

    access(P).result = O;
    set_call_if_reg_result(P);
    pop_root(r);
  }
  else
  {
    access(P).result = O;
    if (O)
      set_call_if_reg_result(P);
  }

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

void context::collect_garbage() const
{
  memory->collect_garbage();
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

vector<int> reg_heap::find_ancestor_regs_in_context(int R, int t) const
{
  vector<int> scan;
  scan.push_back(R);
  assert(reg_is_owned_by(R,t));

  vector<int> unique;
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);
    assert(R.state != reg::free and R.state != reg::none);
    if (R.state == reg::checked) continue;

    // skip this node if its not in context t
    if (not reg_is_owned_by(scan[i],t)) continue;

    R.state = reg::checked;
    unique.push_back(scan[i]);

    // Count the references from E in other regs
    scan.insert(scan.end(), R.referenced_by_in_E.begin(), R.referenced_by_in_E.end());

    // Count the references from calls by other regs
    scan.insert(scan.end(), R.call_outputs.begin(), R.call_outputs.end());
    
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

expression_ref remap_regs(const expression_ref R, const std::map<int, reg_heap::root_t>& new_regs)
{
  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    bool different = false;
    expression* E2 = new expression;
    E2->sub.resize(E->size());
    for(int i=0;i<E->size();i++)
    {
      E2->sub[i] = remap_regs(E->sub[i], new_regs);
      if (E2->sub[i] != E->sub[i])
	different = true;
    }
    if (different)
      return E2;
    else
      return R;
  }
  else if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(R))
  {
    std::map<int, reg_heap::root_t>::const_iterator loc = new_regs.find(RV->target);
    if (loc == new_regs.end())
      return R;
    else
      return new reg_var(*loc->second);
  }
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

int reg_heap::uniquify_reg(int R, int t)
{
  // If the reg is already unique, then we don't need to do anything.
  if (not reg_is_shared(R))
  {
    assert(reg_is_owned_by(R,t));
    return R;
  }

  // 1. Find all the ancestors with name 't'
  vector<int> ancestors = find_ancestor_regs_in_context(R,t);

  std::map<int,root_t> new_regs;
  for(int i=0;i<ancestors.size();i++)
  {
    // 2. Allocate new regs for each ancestor reg
    root_t root = allocate_reg();
    int R2 = *root;
    new_regs[ancestors[i]] = root;

    // 3. Move ownership from the old regs to the new regs.
    access(R2).owners.insert(t);
    access(ancestors[i]).owners.erase(t);
  }

  // 4a. Adjust heads to point to the new regs
  for(int j=0;j<token_roots[t].heads.size();j++)
  {
    int R1 = *token_roots[t].heads[j];
    if (includes(new_regs, R1))
      token_roots[t].heads[j] = new_regs[R1];
  }

  // 4b. Adjust parameters to point to the new regs
  for(int j=0;j<token_roots[t].heads.size();j++)
  {
    int R1 = *token_roots[t].heads[j];
    if (includes(new_regs, R1))
      token_roots[t].heads[j] = new_regs[R1];
  }

  // 5. Adjust E, call, and result to point to the new references.
  foreach(i,new_regs)
  {
    int R1 = i->first;
    int R2 = *(i->second);

    // Adjust E
    set_E(R2, remap_regs(access(R1).E, new_regs) );

    // Adjust result
    access(R2).result = remap_regs(access(R1).result, new_regs);

    // Adjust call
    if (access(R1).call != -1)
    {
      int c = access(R1).call;
      assert(includes(new_regs, c));
      set_call(R2, *new_regs[ c ] );
    }

    // Set changeable
    access(R2).changeable = access(R1).changeable;

    // Should we set the used_inputs?
  }

   return *new_regs[R];
}

vector<int> reg_heap::find_all_regs_in_context(int t) const
{
  vector<int> scan;
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
    assert( not includes(owners, t2) );
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
  expression* V = new expression(*E);
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i]);

  return V;
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
    expression* V = new expression(*E);
    V->sub[2] = graph_normalize(C,E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return V;
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
    expression* V = new expression(*E);

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
      return V;
    else
    {
      int var_index = get_safe_binder_index(R);
      expression_ref x = dummy(var_index);
      expression_ref obj = V->sub[1];
      V->sub[1] = x;

      return let_expression(x,obj,V);
    }
  }

  // FIXME! Handle operations.
  // Extend the stack handling to be able to work on more than one argument.
  // Currently there is no need to evaluate arguments before applying them to functions.
  // Can we avoid evaluating functions before calling an operation?
  // - well, we could make the operation throw an exception identifying which is the first argument that needs to be
  //   evaluated.
  // - then, we could put the operation on the stack and begin evaluating just that one argument.
  
  // 4. Constructor
  if (dynamic_pointer_cast<const Function>(E->sub[0]) or 
      dynamic_pointer_cast<const Operation>(E->sub[0]))
  {
    int var_index = get_safe_binder_index(R);

    expression* Con = new expression;
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

    return let_expression(vars, bodies, Con);
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    expression* V = new expression(*E);

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

    return V;
  }

  throw myexception()<<"graph_normalize: I don't recognize expression '"+ R->print() + "'";
}

int incremental_evaluate(const context&, int);

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
int  incremental_evaluate(const context& C, int R)
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

      C[R].result = C[S].result;

      if (not C[R].changeable)
	R = S;

      continue;
    }

    /*---------- Below here, there is no call, and no result. ------------*/

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
      vector<reg_heap::root_t> new_regs;
      for(int i=0;i<vars.size();i++)
      {
	new_regs.push_back(C.allocate_reg());
	C.access(*new_regs.back()).owners = C.access(R).owners;
	new_reg_vars.push_back( shared_ptr<reg_var>(new reg_var(*new_regs.back())) );
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
	C.set_E( new_reg_vars[i]->target , bodies[i]);

      C.set_E(R, T);

      for(int i=0;i<new_regs.size(); i++)
	C.pop_root( new_regs[i] );
      
      assert(C[R].call == -1);
      assert(not C[R].result);
      continue;
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

      if (not C[R].changeable)
      {
	// The old used_input slots are not invalid, which is OK since none of them are changeable.
	assert(C.access(R).call == -1);
	assert(not C.access(R).result);
	C.clear_used_inputs(R);
	C.set_E(R, result);
	continue;
      }
      else
      {
	// Check for WHNF *OR* heap variables
	if (is_WHNF(result))
	  C[R].result = result;
	else {
	  reg_heap::root_t r2 = C.allocate_reg();
	  C.access(*r2).owners = C.access(R).owners;
	  C.set_E(*r2, result );
	  C[R].result = shared_ptr<const Object>(new reg_var(*r2));
	  C.pop_root(r2);
	}
      }
	
#ifndef NDEBUG
      // std::cerr<<"Executing statement: "<<compact_graph_expression(C,E)<<"\n";
      std::cerr<<"Executing operation: "<<O<<"\n";
      std::cerr<<"Result changeable: "<<C[R].changeable<<"\n\n"<<endl;
#endif
    }

    // 4. We can't exit the loop with a reg_var in the result slot. Change to a call.
    C.set_call_if_reg_result(R);
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
