#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include <algorithm>
#include <fstream>

using boost::shared_ptr;
using boost::dynamic_pointer_cast;
using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::ofstream;

using std::cerr;
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
 * Check that we correctly compute the result and also split the graph when computing X*Y with Y=2 and X=Y.
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
 *
 * 1. How can I make M8 models using the new model framework?
 *
 * 2. How do we share computations between heads?
 *
 *   - First, float unbound let-expressions up as high as they can go. (let_float)
 *     + This allows us to share case results, as well as the results of e.g. \x->5 and \x->2*y).
 *     + To share computations between different branches of execution DYNAMICALLY, though, we need arrays.
 *
 *   - Second, we can let-evaluate all top-level expressions once.  Whenever adding a new heap var,
 *     we can check to see if its sub-expressions already have heap vars for them.
 *
 * 3. [POSTPONE indefinitely!] How can we benefit from partial evaluation, by e.g. changing
 *    \n.\x.case n of {...} to \n.case n of \x.{...}?
 *
 * 4. [POSTPONE] How could we benefit from switching to the rho-calculus?
 *
 * 5. How can we make the model expressions PRINT more clearly?
 *
 *    - Suppress printing of mere conversion functions, on the theory
 *      that functions which convey no information should not be printed?
 *
 *    - Suppress "uniform discretization", or develope a stylized form
 *      of output for it?
 *
 *    - show all the {\pi[i]} as just \pi?
 *
 *    - *? Allow actual greek letters - i.e. use unicode?
 *
 * 6. Could I switch to thunks?
 *
 *    - If so, could I keep the Mark 1 machine around?
 *
 * 8. I also need to be able to NOT recompute things when the change
 *    in value is small!
 *
 * 9. Finally, implement the Hindley-Milner type system?
 *
 *
 * TODO
 *  (a) Make M8 evaluate function calls via the \-calculus  -> (c)
 *  (b) Eliminate the old Context -> (d),(h), (i)
 *  (c) Turn smodel objects into algebraic data types.
 *      - DiscreteDistribution [(Double,a)] & ExtendDiscreteDistribution 
 *      - SModelObject
 *  (d) (DONE) let-floating.  (POSTPONE: Completely lazy evaluation=2)
 *  (e) Sharing of sub-expressions=2 (d)
 *  (f) Efficient evaluation of if statements=2 (d)
 *  (g) Move eigensystem computation and caching to the new model framework. (e)
 *  (h) share sub-expressions between heads.
 *  (i) avoid recalculating some expressions.
 */ 

/*
 * Perhaps entering a meta-variable triggers a substitution - or pushes arguments
 * onto the stack!
 */


/*
 * Issue: Regs that are reachable by back-references from reachable regs stil need to be in a
 *        consistent state.  That is because these regs are reachable in backward walks, such as
 *        in uniquify and set_reg_value( ).
 * 
 * Answer: If we are going to remove ONLY ownership token t, then we need to 
 *         (a) remove it from ALL used regs, or
 *         (b) remove it from unused regs that reference used regs.
 *
 * The only routines that clear ownership now should be:
 *
 * - clear( ).  This is OK because we only clear regs that are not in state reg::used.  Any references
 *              to these regs is a different problem, and can be caught by finding references to unused
 *              regs.
 *
 * - remove_unused_ownership_marks( ).  This is OK, because we construct the minimal
 *              and correct ownership marking.
 * 
 * - uniquify_reg( ).  Here, we only remove ownership from the regs we split.  We remap any (unsplit) 
 *                     parents of these regs to refer to the new regs.  Therefore, there should not be
 *                     any regs in context t that still refer to the old regs.
 */


bool is_reg_var(const expression_ref& R)
{
  if (dynamic_cast<const reg_var*>(&*R)) return true;

  return false;
}

bool is_var(const expression_ref& R)
{
  if (dynamic_cast<const var*>(&*R)) return true;

  return false;
}

bool is_reglike(const expression_ref& R)
{
  return is_dummy(R) or is_parameter(R) or is_reg_var(R) or is_var(R);
}

/*
 * Issue: How can we share eigensystems between Q matrices that differ only by rate?
 *
 * Issue: How can we share eigensystems between Q matrices that are identical by are at separate
 *        positions in the list?
 *
 * Issue: I guess we want to want Q matrices to carry their eigensystem with them, although it will
 *        be computed lazily...
 *
 * OK, so we pass around (ReversibleMarkovModel q pi Eigen(Q,pi) t).  Then any copy of this will copy
 * the node for the (uncomputed) Eigen(Q,pi).
 *
 * Perhaps there is a simple way to compute this as just GetRMM(Q), as opposed to RMM(Q,pi).
 */

expression_ref graph_normalize(const expression_ref& R)
{
  if (not R) return R;

  // 1. Var
  if (is_reglike(R))
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
    V->sub[2] = graph_normalize(E->sub[2]);

    if (V->sub[2] == E->sub[2])
      return R;
    else
      return shared_ptr<const expression>(V);
  }

  // 3. Application
  if (dynamic_pointer_cast<const Apply>(E->sub[0]))
  {
    assert(E->size() == 3);
    expression_ref f = graph_normalize(E->sub[1]);
    expression_ref x = graph_normalize(E->sub[2]);

    int var_index = get_safe_binder_index(R);
    expression_ref f_ = dummy(var_index++);
    expression_ref x_ = dummy(var_index++);

    if (is_reglike(x) and is_reglike(f))
      return (f,x);
    else if (is_reglike(x))
    { 
      return let_expression(f_, f, apply_expression(f_,x));
    }
    else if (is_reglike(f))
    {
      return let_expression(x_, x, apply_expression(f,x_));
    }
    else
      return let_expression({f_, x_}, {f, x}, apply_expression(f_,x_));
  }

  // 6. Case
  shared_ptr<const Case> IsCase = dynamic_pointer_cast<const Case>(E->sub[0]);
  if (IsCase)
  {
    shared_ptr<expression> V ( new expression(*E) );

    V->sub[1] = graph_normalize(V->sub[1]);

    expression_ref* tail = &(V->sub[2]);
    while(shared_ptr<const expression> cons = dynamic_pointer_cast<const expression>(*tail))
    {
      // Create a new Cons
      assert(cons->size() == 3);
      shared_ptr<expression> new_cons ( cons->clone() );

      // Create a new alternative
      shared_ptr<expression> new_alternative ( dynamic_pointer_cast<const expression>(cons->sub[1])->clone());
      new_alternative->sub[2] = launchbury_normalize(new_alternative->sub[2]);

      // Make the new Cons point to the new alternative
      new_cons->sub[1] = shared_ptr<const Object>(new_alternative);

      // Make the level higher up point to the new cons
      (*tail) = shared_ptr<const Object>(new_cons);

      // Go to the next alternative
      tail = &(new_cons->sub[2]);
    }
    
    if (is_reglike(V->sub[1]))
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
  if (dynamic_pointer_cast<const constructor>(E->sub[0]) or 
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
      if (is_reglike(E->sub[i]))
      {
	Con->sub.push_back(E->sub[i]);
      }
      else
      {
	expression_ref var = dummy( var_index++ );
	Con->sub.push_back( var );
	vars.push_back( var );
	bodies.push_back( graph_normalize(E->sub[i]) );
      }
    }

    return let_expression(vars, bodies, shared_ptr<const expression>(Con));
  }

  // 5. Let 
  shared_ptr<const let_obj> Let = dynamic_pointer_cast<const let_obj>(E->sub[0]);
  if (Let)
  {
    shared_ptr<expression> V ( new expression(*E) );

    expression_ref* tail = &(V->sub[1]);
    while(shared_ptr<const expression> cons = dynamic_pointer_cast<const expression>(*tail))
    {
      // Create a new Cons
      assert(cons->size() == 3);
      shared_ptr<expression> new_cons ( cons->clone() );

      // Create a new definition
      shared_ptr<expression> new_def ( dynamic_pointer_cast<const expression>(cons->sub[1])->clone());
      new_def->sub[2] = launchbury_normalize(new_def->sub[2]);

      // Make the new Cons point to the new alternative
      new_cons->sub[1] = shared_ptr<const Object>(new_def);

      // Make the level higher up point to the new cons
      (*tail) = shared_ptr<const Object>(new_cons);

      // Go to the next alternative
      tail = &(new_cons->sub[2]);
    }

    V->sub[2] = graph_normalize(V->sub[2]);

    return shared_ptr<const expression>(V);
  }

  throw myexception()<<"graph_normalize: I don't recognize expression '"+ R->print() + "'";
}

reg::reg()
 :changeable(false),
  call(-1),
  // initialize result to NULL, so that inserting copies of a single reg() doesn't result in sharing.
  prev_reg(-1),
  next_reg(-1),
  state(none)
{}

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

void reg_heap::set_used_input(int R1, int R2)
{
  assert(R1 >= 0 and R1 < n_regs());
  assert(R2 >= 0 and R2 < n_regs());

  assert(access(R1).E);
  assert(access(R2).E);

  // It IS possible to add an input that's already used.
  // This happens if we evaluate a new used input R2' to an already used input R2
  //   through regvar chains that are not changeable.

  // R1 shouldn't have any used inputs if it isn't changeable.
  assert(access(R1).changeable);
  // Don't add unchangeable results as inputs
  assert(access(R2).changeable);
  // Don't add a reg as input if no reduction has been performed.
  // assert(access(R2).result or access(R2).call != -1);

  access(R1).used_inputs.insert(R2);
  access(R2).outputs.insert(R1);
}

void reg_heap::clear_used_input(int R1, int R2)
{
  assert(R1 >= 0 and R1 < n_regs());
  assert(R2 >= 0 and R2 < n_regs());
  set<int>& used_inputs = access(R1).used_inputs;

  set<int>::iterator loc = used_inputs.find(R2);
  assert(loc != used_inputs.end());
  used_inputs.erase(loc);

  // If this reg is unused, then upstream regs are in the process of being destroyed.
  // However, if this reg is used, then upstream regs may be live, and so should have
  //  correct edges.
  assert( access(R1).state != reg::used or includes( access(R2).outputs, R1) );

  access(R2).outputs.erase(R1);
}

void reg_heap::clear_used_inputs(int R)
{
  set<int> used_inputs = access(R).used_inputs;
  for(auto i: used_inputs)
    clear_used_input(R, i);

  assert(access(R).used_inputs.empty());
}

// set_call (or set_call_unsafe) is only called when
// 1. uniquify_reg( ): A  call is being remapped
// 2. incremental_evaluate( ):
// - an existing call is being remapping to the end of an unchangeable indirection chain.
// - access(R).E is a reg_var
// * a CHANGEABLE operation was performed (see set_reduction_result)
// 3. set_reduction_result( )
// - a parameter value is being set.
// - an operation was just performed AND

// Q: OK, so why is it OK to not create a new node with a redirection when we 
// CHANGEABLY evaluate to a reg_var?
// A: Well, it seems that the answer is that when we changably call <a>, and <a>
//    *unchangeably* redirects to <b>, then we can call directly to <b>, although we
//    have to invalidate this when the reduction result is invalidated.

void reg_heap::set_call_unsafe(int R1, int R2)
{
  // Check that R1 is legal
  assert(0 <= R1 and R1 < n_regs());
  assert(access(R1).state == reg::used);

  // Check that R2 is legal
  assert(0 <= R2 and R2 < n_regs());
  assert(access(R2).state == reg::used);

  // Check that we aren't overriding an existing *call*
  assert(access(R1).call == -1);

  access(R1).call = R2;
  access(R2).call_outputs.insert(R1);

  // check that all of the owners of R are also owners of R.call;
  assert(includes(access(R2).owners, access(R1).owners));
}


void reg_heap::set_call(int R1, int R2)
{
  set_call_unsafe(R1, R2);

  // Check that we aren't overriding an existing *result*
  assert(not access(R1).result);
}

void reg_heap::clear_call(int R)
{
  int R2 = access(R).call;
  if (R2 == -1) return;
  assert(R2 >= 0 and R2 < n_regs());
  
  access(R).call = -1;

  // If this reg is unused, then upstream regs are in the process of being destroyed.
  // However, if this reg is used, then upstream regs may be live, and so should have
  //  correct edges.
  assert( access(R).state != reg::used or includes(access(R2).call_outputs, R) );

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
  assert(e);
  assert(not access(R).owners.empty());
  clear_E(R);

  access(R).E = e;
  access(R).references = get_exp_refs(e);
  for(int r: access(R).references)
  {
    // check that all of the owners of R are also owners of *r.
    assert(includes(access(r).owners, access(R).owners) );

    // check that *r is not already marked as being referenced by R
    assert(not includes( access(r).referenced_by_in_E, R) );

    // mark *r as being referenced by R
    access(r).referenced_by_in_E.insert(R);
  }
}

void reg_heap::clear_E(int R)
{
  for(int r: access(R).references)
    access(r).referenced_by_in_E.erase(R);

  access(R).references.clear();

  access(R).E = expression_ref();
}

void reg_heap::set_reduction_result(int R, const expression_ref& result)
{
  // Check that there is no result we are overriding
  assert(not access(R).result );

  // Check that there is no previous call we are overriding.
  assert(access(R).call == -1);

  // if the result is NULL, just leave the result and call both unset.
  if (not result) return;

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
void reg_heap::set_reg_value(int P, const expression_ref& OO,int token)
{
  // Check that reg P is owned by context token.
  assert(reg_is_owned_by(P,token));

  // Normalize the inputs expression
  expression_ref O = let_float(graph_normalize(OO));

  // Split this reg and its E-ancestors out from other graphs, if its shared.
  P = uniquify_reg(P,token);

  // Check that this reg is indeed settable
  assert(dynamic_pointer_cast<const parameter>(access(P).E));
  assert(access(P).changeable);

  // Clear the call, clear the result, and set the value
  assert(access(P).used_inputs.empty());
  clear_call(P);
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
    set<int> outputs = access(R1).outputs;
    for(int R2: outputs)
    {
      // This one already marked NOT known_value_unchanged
      if (includes(visited, R2)) continue;

      // Since R2 is not known to have identical results for its USED inputs ...
      // ... then it is not known to have identical outputs
      NOT_known_value_unchanged.push_back(R2);
      visited.insert(R2);

      // Since the computation may be different, we don't know if the value has changed.
      access(R2).result.reset();
      // We don't know what the reduction result is, so invalidate the call.
      clear_call(R2);
      // Remember to clear the used inputs.
      clear_used_inputs(R2);
    }
    assert(access(R1).outputs.empty());

    for(int R2: access(R1).call_outputs)
    {
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
  // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
  remove_reg_from_used_list(r);

  // Downstream objects could still exist
  clear_used_inputs(r);
  clear_call(r);
  clear_E(r);

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

reg_heap::root_t reg_heap::push_temp_head(int t)
{
  set<int> tokens;
  tokens.insert(t);
  return push_temp_head(tokens);
}

reg_heap::root_t reg_heap::push_temp_head(const std::set<int>& tokens)
{
  root_t r = allocate_reg();
  access(*r).owners = tokens;
  for(int t: tokens)
    token_roots[t].temp.push_back(r);

  return r;
}

void reg_heap::pop_temp_head(int t)
{
  set<int> tokens;
  tokens.insert(t);
  pop_temp_head(tokens);
}

void reg_heap::pop_temp_head(const std::set<int>& tokens)
{
  int t0 = *tokens.begin();
  root_t r0 = token_roots[t0].temp.back();

  for(int t: tokens)
  {
    root_t r = token_roots[t].temp.back();
    assert( r == r0 );
    assert( includes( access(*r).owners, t) );
    token_roots[t].temp.pop_back();
  }

  pop_root(r0);
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

void reg_heap::remove_unused_ownership_marks()
{
  // Clear ownership marks
  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
#ifndef NDEBUG
    R.temp_owners = R.owners;
#endif
    R.owners.clear();

    here = R.next_reg;
  }

  // Mark ownership on regs according to reachability.
  for(int t=0;t<get_n_tokens();t++)
  {
    // Don't compute reachability from unused tokens.
    if (not token_is_used(t)) continue;

    // Find the all the regs reachable from heads in t
    vector<int> regs = find_all_regs_in_context_no_check(t);

    // Mark regs reachable in t as being owned by t
    for(int i=0;i<regs.size();i++)
    {
      int R = regs[i];
      access(R).owners.insert(t);
    }
  }

#ifndef NDEBUG
  // Check that we did not ADD any ownership marks!
  here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    assert(includes(R.temp_owners, R.owners) );
    R.temp_owners.clear();

    here = R.next_reg;
  }
#endif
}

void reg_heap::collect_garbage()
{
#ifndef NDEBUG
  std::cerr<<"***********Garbage Collection******************"<<std::endl;
  check_used_regs();
#endif
  assert(n_regs() == n_used_regs() + n_free_regs());

  vector<int> scan;
  for(int i: roots)
    scan.push_back(i);

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

#ifndef NDEBUG
  cerr<<"Regs: "<<n_used_regs()<<"/"<<n_regs()<<endl;
  cerr<<"#roots = "<<roots.size()<<endl;
  check_used_regs();
#endif

  remove_unused_ownership_marks();

  // Check that we have no un-owned objects that are used
  here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    assert(not R.owners.empty());

    here = R.next_reg;
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

vector<int> reg_heap::find_call_ancestors_in_context(int R,int t) const
{
  vector<int> ancestors;

  // Add the call parents of R
  for(int Q: access(R).call_outputs)
  {
    assert(access(Q).state == reg::used);

    // Skip ancestors not in this context
    if (not reg_is_owned_by(R,t)) continue;

    access(Q).state = reg::checked;
    ancestors.push_back(Q);
  }

  // Recursively add the call parents
  for(int i=0;i<ancestors.size();i++)
  {
    int Q1 = ancestors[i];

    assert(access(Q1).state == reg::checked);

    for(int Q2: access(Q1).call_outputs)
    {
      // Skip regs that have been seen before.
      if (access(Q2).state == reg::checked) continue;

      assert( access(Q2).state == reg::used);

      // Skip ancestors not in this context
      if (not reg_is_owned_by(Q2,t)) continue;

      access(Q2).state = reg::checked;
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
  vector<int> scan = {R};
  assert(reg_is_owned_by(R,t));

  vector<int> unique;
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);

    // Regs should be on the used list
    assert(R.state != reg::free and R.state != reg::none);

    // Only consider each reg at most once
    if (R.state == reg::checked) continue;

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

expression_ref remap_regs(const expression_ref R, const map<int, int>& new_regs)
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
    map<int, int>::const_iterator loc = new_regs.find(RV->target);
    if (loc == new_regs.end())
      return R;
    else
      return new reg_var(loc->second);
  }
  // This case handles NULL in addition to atomic objects.
  else
    return R;
}

bool reg_heap::reg_is_shared(int R) const
{
  const std::set<int>& owners = access(R).owners;

  std::set<int>::const_iterator loc = owners.begin();

  if (loc == owners.end()) return false;

  loc++;

  return (loc != owners.end());
}

bool reg_heap::reg_is_owned_by(int R, int t) const
{
  const std::set<int>& owners = access(R).owners;

  return includes(owners, t);
}

int remap(int R, const map<int,int>& new_regs)
{
  map<int,int>::const_iterator loc = new_regs.find(R);
  if (loc == new_regs.end())
    return R;
  else
    return loc->second;
}

void reg_heap::check_results_in_context(int t) const
{
  vector<int> WHNF_results;
  vector<int> regs = find_all_regs_in_context(t);
  for(int i=0;i<regs.size();i++)
  {
    int Q = regs[i];
    if (access(Q).result and access(Q).call == -1)
    {
      assert(access(Q).E->maybe_equals( *access(Q).result) );
      WHNF_results.push_back(Q);
    }
  }

  // Update the call outputs
  for(int i=0;i<WHNF_results.size();i++)
  {
    int Q = WHNF_results[i];

    expression_ref result = access(Q).result;

    vector<int> regs = find_call_ancestors_in_context( Q, t);

    for(int j=0;j<regs.size();j++)
      if (access(regs[j]).result)
	assert( access(regs[j]).result == result );
  }

}

int reg_heap::uniquify_reg(int R, int t)
{
#ifndef NDEBUG
  check_results_in_context(t);

  // This checks that ownership and references are consistent
  find_all_regs_in_context(t);
#endif  

  assert(token_roots[t].temp.empty());

  // If the reg is already unique, then we don't need to do anything.
  if (not reg_is_shared(R))
  {
    assert(reg_is_owned_by(R,t));
    return R;
  }

  vector<int> changed_results;

  // 1. Find all ancestors with name 't' that are *shared*
  // (Some of these could be unreachable!)
  vector<int> shared_ancestors = find_shared_ancestor_regs_in_context(R,t);
  int n_new_regs = 0;

  // 2. Allocate new regs for each *shared* ancestor reg in context t
  map<int,int> new_regs;
  for(int i=0;i<shared_ancestors.size();i++)
  {
    int R1 = shared_ancestors[i];
    int R2 = *push_temp_head(t);
    n_new_regs++;
    
    new_regs[R1] = R2;
  }


  // 4e. Initialize/Copy changeable
  // 2. Remove regs that got deallocated from the list.
  // Alternatively, I could LOCK them in place.
  for(map<int,int>::iterator i = new_regs.begin(); i!= new_regs.end();)
  {
    int R1 = i->first;
    int R2 = i->second;
    if (access(R1).state == reg::used and reg_is_shared(R1))
    {
      access(R2).changeable = access(R1).changeable;

      i++;
    }
    else
      new_regs.erase(i++);
  }

  // 2a. Copy the over and remap E
  //     This is separate to that avoid linking to regs with no E.
  for(const auto& i: new_regs)
  {
    int R1 = i.first;
    int R2 = i.second;

    // Check no mark on R2
    assert(access(R1).state == reg::used);
    assert(access(R2).state == reg::used);
    
    assert( not access(R1).owners.empty() );

    // 4. Initialize fields in the new node

    // 4a. Initialize/Remap E
    set_E(R2, remap_regs( access(R1).E, new_regs) );
  }

  // 2b.  Copy over and remap the call, used_inputs, and result
  //      This is after copying E to avoid linking to regs with no E.
  for(const auto& i: new_regs)
  {
    int R1 = i.first;
    int R2 = i.second;

    // 4b. Initialize/Remap call
    if (access(R1).call != -1)
      set_call(R2, remap(access(R1).call, new_regs) );

    // 4c. Initialize/Remap used_inputs
    for(int j: access(R1).used_inputs)
      set_used_input(R2, remap(j, new_regs) );

    // 4d. Initialize/Remap result if E is in WHNF.
    if (access(R2).call == -1 and access(R1).result)
    {
      access(R2).result = access(R2).E;
      changed_results.push_back(R2);
    }
    // 4d. Initialize/Copy result otherwise.
    else
      access(R2).result = access(R1).result;
  }

  // 4a. Adjust heads to point to the new regs
  for(int j=0;j<token_roots[t].heads.size();j++)
  {
    int R1 = *token_roots[t].heads[j];
    if (includes(new_regs, R1))
      *token_roots[t].heads[j] = new_regs[R1];
  }

  // 4b. Adjust parameters to point to the new regs
  for(int j=0;j<token_roots[t].parameters.size();j++)
  {
    int R1 = *token_roots[t].parameters[j];
    if (includes(new_regs, R1))
      *token_roots[t].parameters[j] = new_regs[R1];
  }

  // 4c. Adjust identifiers to point to the new regs
  for(const auto& j: token_roots[t].identifiers)
  {
    // Hmmm.... this could be a lot of identifiers to scan...
    int R1 = *j.second;
    if (includes(new_regs, R1))
      *j.second = new_regs[R1];
  }

  // 5. Find the unsplit parents of split regs
  //    These will be the only parents of the old regs that have context t.
  vector<int> unsplit_parents;
  for(const auto& i: new_regs)
  {
    int R1 = i.first;

    set<int> parents = access(R1).referenced_by_in_E;
    add(parents, access(R1).outputs);
    // A node can only call another node if ... ??
    add(parents, access(R1).call_outputs);

    for(int Q1: parents)
    {
      // Skip regs that we've handled already.
      if (access(Q1).state == reg::checked) continue;

      // NOTE: we could have parent that are in t that are shared, but these
      // should be original regs that will eventually be removed from t.

      // We are only interested in the unshared E-ancestors in t.
      if (reg_is_shared(Q1)) continue;

      // We are only interested in the unshared E-ancestors in t.
      if (not reg_is_owned_by(Q1, t)) continue;

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
  for(const auto& i: new_regs)
  {
    int R1 = i.first;
    int R2 = i.second;

    // Original nodes should never have been marked.
    assert( access(R1).state == reg::used );

    // Split nodes should not have been marked.
    assert( access(R2).state == reg::used );

    // The split nodes should now be E-ancestors in t
    for(int j: access(R2).referenced_by_in_E)
      assert( access(j).state == reg::used );

    // The split nodes should now be E-ancestors in t
    for(int j: access(R1).referenced_by_in_E)
      assert( access(j).state == reg::used );
  }
  
  // Remap the unsplit parents. (The parents don't move, but they reference children that do.)
  for(int Q1: unsplit_parents)
  {
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
	set_call_unsafe(Q1, new_call);
      }
    }
    
    // c. Adjust use edges
    set<int> old_used_inputs = access(Q1).used_inputs;
    clear_used_inputs(Q1);
    for(int j: old_used_inputs)
    {
      set_used_input(Q1, remap(j, new_regs));
    }
    
    // d. Remap result if E is in WHNF
    if (access(Q1).call == -1 and access(Q1).result)
    {
      access(Q1).result = access(Q1).E;
      changed_results.push_back(Q1);
    }
  }

  // Remove ownership from the old regs.
  foreach(i,new_regs)
  {
    int Q = i->first;

    // These regs should be shared.
    assert(reg_is_shared(Q));

    // These regs should have originally contained t.
    assert(includes(access(Q).owners, t) );

    // But now remove membership in t from these regs.
    access(Q).owners.erase(t);
  }
    

  // Update the call outputs
  for(int i=0;i<changed_results.size();i++)
  {
    int Q = changed_results[i];

    expression_ref result = access(Q).result;

    vector<int> regs = find_call_ancestors_in_context( Q, t);

    for(int j=0;j<regs.size();j++)
    {
      int S = regs[j];
      access(S).result = result;

      // In general, the owners of a parent (S) should all be owners of a child (S).
      // This allows S to have no owners, which could happen if S became unreachable.

      // Any call ancestors of E-ancestors of p should be E-ancestors of p, and therefore should be in t.
      assert(access(S).owners.empty() or reg_is_owned_by(S,t));

      // Any call ancestors of E-ancestors of p should be E-ancestors of p, and therefore should be uniquified.
      assert(not reg_is_shared(S));
    }
  }

  int R2 = new_regs[R];

#ifndef NDEBUG
  // This checks that ownership and references are consistent
  find_all_regs_in_context(t);
#endif

#ifndef NDEBUG
  foreach(i,new_regs)
  {
    int R1 = i->first;
    int R2 = i->second;

    // Check that ownership has been properly split
    assert(not includes(access(R1).owners, t) );
    assert(includes(access(R2).owners, t) );
    assert(not reg_is_shared(R2));

    // R2 should have a result IFF R1 has a result
    assert(not access(R1).result or access(R2).result);
    assert(not access(R2).result or access(R1).result);

    // R2 should have a call IFF R1 has a call
    assert(access(R1).call == -1 or access(R2).call != -1);
    assert(access(R2).call == -1 or access(R1).call != -1);
  }
#endif

  // 5. Remove root references to new regs.
  //    Remove t-ownership from old regs.
  for(int i=0;i<n_new_regs;i++)
    pop_temp_head(t);

  assert(token_roots[t].temp.empty());

#ifndef NDEBUG
  check_results_in_context(t);
#endif  

  return R2;
}

void reg_heap::check_used_reg(int index) const
{
  const reg& R = access(index);

  // Check that we have already correctly recorded all the references!
  assert(get_exp_refs(R.E) == R.references);

  foreach(r, R.references)
  {
    // Check that referenced regs are owned by the owners of R
    assert(includes(access(*r).owners, R.owners) );
    
    // Check that referenced regs are have back-references to R
    assert(includes( access(*r).referenced_by_in_E, index) );
  }
  
  foreach(r, R.used_inputs)
  {
    // Check that used regs are owned by the owners of R
    assert(includes(access(*r).owners, R.owners) );

    // Check that used regs are have back-references to R
    assert(includes( access(*r).outputs, index) );
  }

  if (R.call != -1)
  {
    // Check that the call-used reg is owned by owners of R
    assert(includes(access(R.call).owners, R.owners) );

    // Check that the call-used reg has back-references to R
    assert(includes(access(R.call).call_outputs, index) );
  }
}

void reg_heap::check_used_regs() const
{
  // check_used_regs
  for(int here = first_used_reg;here != -1;)
  {
    check_used_reg(here);
    here = access(here).next_reg;
  }
}

vector<int> reg_heap::find_all_regs_in_context(int t) const
{
  vector<int> scan;
  foreach(i,token_roots[t].temp)
  {
    assert(reg_is_owned_by(**i, t));
    scan.push_back(**i);
  }
  foreach(i,token_roots[t].heads)
  {
    assert(reg_is_owned_by(**i, t));
    scan.push_back(**i);
  }

  foreach(i,token_roots[t].parameters)
  {
    assert(reg_is_owned_by(**i, t));
    scan.push_back(**i);
  }

  foreach(i,token_roots[t].identifiers)
  {
    assert(reg_is_owned_by(*(i->second), t));
    scan.push_back(*(i->second));
  }

  vector<int> unique;
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);
    assert(reg_is_owned_by(scan[i], t));
    assert(R.state != reg::free and R.state != reg::none);
    if (R.state == reg::checked) continue;

    R.state = reg::checked;
    unique.push_back(scan[i]);

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
    assert(includes(R.owners, t) );

    R.state = reg::used;

    check_used_reg(unique[i]);
  }

  return unique;
}

vector<int> reg_heap::find_all_regs_in_context_no_check(int t) const
{
  vector<int> scan;
  foreach(i,token_roots[t].temp)
    scan.push_back(**i);
  foreach(i,token_roots[t].heads)
    scan.push_back(**i);

  foreach(i,token_roots[t].parameters)
    scan.push_back(**i);
  foreach(i,token_roots[t].identifiers)
  {
    scan.push_back(*(i->second));
  }


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

  // We shouldn't have any temporary heads still on the stack, here!
  assert(token_roots[t].temp.empty());

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

  foreach(i,token_roots[t].identifiers)
  {
    pop_root(i->second);
  }
  token_roots[t].identifiers.clear();

  // mark unused
  unused_tokens.push_back(t);
  token_roots[t].used = false;

  // 
  remove_unused_ownership_marks();
}

bool reg_heap::token_is_used(int t) const
{
  return token_roots[t].used;
}

int reg_heap::copy_token(int t)
{
  int t2 = get_unused_token();

  assert(token_roots[t].temp.empty());

  foreach(i,token_roots[t].heads)
  {
    token_roots[t2].heads.insert( token_roots[t2].heads.end(), push_root(**i) );
  }

  foreach(i,token_roots[t].parameters)
  {
    token_roots[t2].parameters.insert( token_roots[t2].parameters.end(), push_root(**i) );
  }

  token_roots[t2].identifiers = token_roots[t].identifiers;
  foreach(i,token_roots[t2].identifiers)
  {
    i->second = push_root(*(i->second));
  }

  // remove ownership mark from used regs in this context
  vector<int> token_regs = find_all_regs_in_context_no_check(t2);
  for(int i=0;i<token_regs.size();i++)
  {
    std::set<int>& owners = access(token_regs[i]).owners;
    owners.insert(t2);
  }

  return t2;
}

reg_heap::reg_heap()
  :first_free_reg(-1),
   first_used_reg(-1)
{ }

#include "computation.H"

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs: public OperationArgs
{
  const int R;

  reg_heap& M;

  const int t;

  std::set<int> owners;

  int n_allocated;

  /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
  int lazy_evaluate_reg(int R2)
  {
    set<int>::const_iterator loc = M[R].used_inputs.find(R2);

    // We only need to record the usage, adjust the reference, or mark R as changeable if we haven't already.
    if (loc == M[R].used_inputs.end())
    {
      // Compute the result, and follow non-changeable call chains.
      int R3 = M.incremental_evaluate(R2, t);

      if (M[R3].changeable) 
      {
	// If R2 -> result was changeable, then R -> result will be changeable as well.
	M[R].changeable = true;

	// Note that although R2 is newly used, R3 might be already used if it was 
	// found from R2 through a non-changeable reg_var chain.
	M.set_used_input(R, R3);
      }
    }

    return R2;
  }

  // Note: see note below on evaluate_structure( ) on the issue of returning lambdas.

  /// Reduce the WHNF expression to either a lambda or a constructor, but evaluating a reg_var if passed.
  expression_ref lazy_evaluate_structure(const expression_ref& S)
  {
    // Any slot that we are going to evaluate needs to point to another node
    if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( S ))
    {

      int R2 = RV->target;

      R2 = lazy_evaluate_reg(R2);

      /*
       * We could update E1->sub[slot+1] = new reg_var(R2) if R2 != RV->target.  However:
       *
       * - Updating WHNF regs is problematic because it could make the old reg unused
       *   although it was still used in the result and the results of call-ancestors.
       *   Therefore all call-ancestors would need to be updated.
       *
       * - Updating non-WHNF regs is problematic because we might need to update the used_inputs
       *   to refer to the new reg.  This is because the old one might become unused
       *   (and therefore be garbage-collected.)
       */

      return M.access(R2).result;
    }
    else
      return S;
  }

  /*
   * NOTE: When fully evaluating a structure, we must record uses for all of the regs that we
   *       access, including constructor fields.
   */

  /*
   * NOTE: It is probably the case that taking structures like this allows evaluate( )
   *       and lazy_evaluate( ) below to handle arguments not being reg_vars.
   *
   *       But I'm not sure we want to preserve that, and I'm not sure it works!
   *
   * We could also define evaluate_structure as a wrapper for another routine that takes a reg index.
   */
  
  expression_ref evaluate_structure(const expression_ref& S)
  {
    if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>( S ))
    {
      int R2 = lazy_evaluate_reg(RV->target);

      /* IDEA: only allow evaluation of reg_vars, constants, and constructors 
	       any reg_var that evaluates to a lambda stays a reg_var.
	       that is the only use way of using the result.
       */

      return evaluate_structure( M.access(R2).result );
    }
    else if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(S))
    {
      // If the "structure" is a lambda function, then we are done.
      // (a) if we were going to USE this, we should just call lazy evaluate! (which return a heap variable)
      // (b) if we are going to PRINT this, then we should probably normalize it more fully....?
      // See note above on returning lambdas as reg_vars.
      if (dynamic_pointer_cast<const lambda>(E->sub[0])) return S;

      assert(dynamic_pointer_cast<const constructor>(E->sub[0]));

      // If the result is a constructor expression, then evaluate its fields also.
      shared_ptr<expression> E2 ( dynamic_pointer_cast<const expression>(S)->clone() );
      
      bool different = false;
      for(int i=1;i<E2->size();i++)
      {
	E2->sub[i] = evaluate_structure(E->sub[i]);
	if (E2->sub[i] != E->sub[i])
	  different = true;
      }

      if (different)
	return shared_ptr<const Object>(E2);
      else
	return S;
    }
    else
      return S;
  }

public:

  boost::shared_ptr<const Object> reference(int slot) const
  {
    return dynamic_pointer_cast<const expression>(M[R].E)->sub[slot+1];
  }

  boost::shared_ptr<const Object> evaluate(int slot)
  {
    return evaluate_structure(reference(slot));
  }

  boost::shared_ptr<const Object> lazy_evaluate(int slot)
  {
    return lazy_evaluate_structure(reference(slot));
  }

  shared_ptr<const Object> evaluate_expression(const expression_ref&)
  {
    std::abort();
  }

  int allocate(const expression_ref& R)
  {
    int r = *M.push_temp_head( owners );
    M.set_E(r, R);
    n_allocated++;
    return r;
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, reg_heap& m, int T)
    :R(r),M(m),t(T),owners(M.access(R).owners), n_allocated(0)
  { 
    M.clear_used_inputs(R);
  }

  ~RegOperationArgs()
  {
    for(int i=0;i<n_allocated;i++)
      M.pop_temp_head( owners );
  }
};

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, reg_heap::root_t>&);

  /*
   * incremental_eval R1
   * 
   *   while(not R1.result) do:
   *
   *   If R1.E = (Op or parameter) with call
   *      assert(R1.changeable == true)
   *      R1.call = incremental_evaluate(R1.call)
   *      R1.result = R1.call.result
   *      <break>
   *   
   *   If R1.E = <R2>
   *      assert(R1.changeable == false)
   *      R3 = incremental_evaluate(R2)
   *      (assert R2.changeable = R3.changeable)
   *      R1.changeable = R3.call.changeable
   *      if (R3 != R2)
   *         R1.E = <R3>
   *      R1.call = R3
   *      R1.result = R1.call.result
   *      R1 = R1.call                 // This returns R1.call
   *      <break>
   *  
   *   If (R1.E is WHNF)
   *      R1.result = R1.E
   *      <break>
   *
   *   If R1.E = parameter and no call
   *      Complain: parameters should always have a call!
   *  
   *   If R1.E = Op args (no call)
   *      **Execute reduction**
   *      R1.changeable = reduction changeable
   *      If (changeable)
   *         R1.call = new reg (reduction result)
   *      Else
   *         R1.E = reduction result
   *      <continue>
   *
   *   If R1.E = let expression
   *      R1.E = reduction result
   *      assert(not changeable)
   *      assert(no call)
   *      assert(no result)
   *      <continue>
   *
   *   assert(R1 has a result)
   *   assert(R1.result is WHNF)
   *   assert(R1.result is not a reg_var <*>)
   *   return R1
   */

/// Evaluate R and look through reg_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not a reg_var.
int reg_heap::incremental_evaluate(int R, int t)
{
  assert(R >= 0 and R < n_regs());
  assert(access(R).state == reg::used);
  assert(get_exp_refs(access(R).E) == access(R).references);
  assert(includes(access(R).owners, t));
  assert(is_WHNF(access(R).result));

#ifndef NDEBUG
  //  if (not access(R).result) std::cerr<<"Statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

  while (not access(R).result)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

#ifndef NDEBUG
    //    std::cerr<<"   statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif
    assert(get_exp_refs(access(R).E) == access(R).references);

    // If we know what to call, then call it and use it to set the result
    if (access(R).call != -1)
    {
      // This should only be an Operation or a Parameter.
      assert(access(R).changeable);

      // Evaluate S, looking through unchangeable redirections
      int call = incremental_evaluate(access(R).call, t);

      // If access(R).call can be evaluated to refer to S w/o moving through any changable operations, 
      // then it should be safe to change access(R).call to refer to S, even if R is changeable.
      if (call != access(R).call)
      {
	clear_call(R);
	set_call(R,call);
      }

      // R gets its result from S.
      access(R).result = access(call).result;
    }

    /*---------- Below here, there is no call, and no result. ------------*/

    else if (shared_ptr<const reg_var> RV = dynamic_pointer_cast<const reg_var>(access(R).E))
    {
      assert( access(R).call == -1);

      int C = incremental_evaluate(RV->target, t);

      set_call(R, C);

      assert(not access(R).changeable);

      access(R).changeable = access(C).changeable;

      access(R).result = access(C).result;

      // If we point to C through an intermediate reg_var chain, then change us to point to the end
      if (C != RV->target)
	set_E(R, reg_var(C));

      return C;
    }

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(access(R).E))
      access(R).result = access(R).E;

    // A parameter has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (shared_ptr<const parameter> p = dynamic_pointer_cast<const parameter>(access(R).E))
      throw myexception()<<"Parameter with no result?! (Changeable = "<<access(R).changeable<<")";

    
    // Reduction: let expression
    else if (parse_let_expression(access(R).E, vars, bodies, T))
    {
      set<int> owners = access(R).owners;

      vector<shared_ptr<reg_var> > new_reg_vars;
      for(int i=0;i<vars.size();i++)
      {
	int V = *push_temp_head(owners);
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
      
      assert(not access(R).changeable);

      // Set the bodies of the new reg_vars
      for(int i=0;i<vars.size();i++) 
      {
	int V = new_reg_vars[i]->target;

	set_E(V , bodies[i]);
      }

      set_E(R, T);

      // Remove the new heap vars from the list of temp heads in reverse order.
      for(int i=0;i<new_reg_vars.size(); i++)
	pop_temp_head(owners);
      
      assert(access(R).call == -1);
      assert(not access(R).result);
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(access(R).E);
      assert(E);
      
      shared_ptr<const Operation> O = dynamic_pointer_cast<const Operation>(E->sub[0]);
      assert(O);

      // Although the reg itself is not a parameter, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not access(R).changeable);" here.

#ifndef NDEBUG
      string SS = "";
      SS = compact_graph_expression(*this, R, get_identifiers_for_context(t))->print();
      if (false)
      {
	std::ofstream f("token.dot");
	dot_graph_for_token(*this, t, f);
	f.close();
      }
#endif

      RegOperationArgs Args(R, *this, t);
      expression_ref result = (*O)(Args);

      // NOTE: While not all used_inputs are E-children, they SHOULD all be E-descendents.
      //       How could we assert that?

      // If the reduction doesn't depend on parameters, then replace E with the result.
      if (not access(R).changeable)
      {
	// The old used_input slots are not invalid, which is OK since none of them are changeable.
	assert(access(R).call == -1);
	assert(not access(R).result);
	clear_used_inputs(R);
	set_E(R, result);
      }
      // Otherwise, set the reduction result.
      else
	set_reduction_result(R, result );

#ifndef NDEBUG
      //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
      std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
    }
  }

  assert(access(R).result);
  assert(is_WHNF(access(R).result));
  assert(not dynamic_pointer_cast<const reg_var>(access(R).result));

  return R;
}

expression_ref subst_referenced_vars(const expression_ref& R, const map<int, expression_ref>& names)
{
  if (shared_ptr<const expression> E = dynamic_pointer_cast<const expression>(R))
  {
    bool different = false;
    shared_ptr<expression> E2 ( new expression );
    E2->sub.resize(E->size());
    for(int i=0;i<E->size();i++)
    {
      E2->sub[i] = subst_referenced_vars(E->sub[i], names);
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
    map<int, expression_ref>::const_iterator loc = names.find(RV->target);
    if (loc == names.end())
      return R;
    else
    {
      //      assert(get_free_indices(loc->second).empty());
      return loc->second;
    }
  }
  // This case handles NULL in addition to atomic objects.
  else
    return R;
}

void discover_graph_vars(const reg_heap& C, int R, map<int,expression_ref>& names, const map<string, reg_heap::root_t>& id)
{
  expression_ref E = C.access(R).E;
  set<int> refs = get_exp_refs(E);

  // If there are no references, then we are done.
  if (refs.empty()) 
  {
    names[R] = E;
    return;
  }

  // If R references R, then terminate the recursion.
  if (includes(names, R))
  {
    if (not names[R])
      names[R] = E;
    return;
  }

  // avoid infinite loops because of re-entering R
  names[R] = expression_ref();

  // find the names for each referenced var.
  foreach(i, refs)
  {
    discover_graph_vars(C, *i, names, id);
  }

  names[R] = subst_referenced_vars(E, names);
}

string wrap(const string& s, int w)
{
  string s2 = s;
  string result;
  while (s2.size())
  {
    int pos = -1;
    if (s2.size() > w)
      pos = s2.find(' ',w);

    if (result.size())
      result += "\\n";

    if (pos == -1)
    {
      result += s2;
      s2 = "";
    }
    else
    {
      result += s2.substr(0,pos);
      s2 = s2.substr(pos+1);
    }
  }
  return result;
}

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, reg_heap::root_t>& ids)
{
  map< int, expression_ref> names;
  foreach(id,ids)
  {
    int R = *(id->second);
    string name = id->first;
    names[R] = expression_ref(new var(name) );
  }
  discover_graph_vars(C, R, names, ids);

  return launchbury_unnormalize(names[R]);
}

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o)
{
  vector<int> regs = C.find_all_regs_in_context(t);

  o<<"digraph \"token"<<t<<"\" {\n";
  o<<"graph [ranksep=0.25, fontname=Arial, nodesep=0.125];\n";
  o<<"node [fontname=Arial, style=filled, height=0, width=0, shape=box];\n";
  o<<"edge [style=\"setlinewidth(2)\"];\n";
  for(int R:regs)
  {
    string name = "n" + convertToString(R);
    // node name
    o<<name<<" ";
    o<<"[";
    string label = wrap(C.access(R).E->print(), 40);
    o<<"label = \""<<R<<": "<<label<<"\"";
    if (C.access(R).changeable)
      o<<",style=\"dashed,filled\",color=red";

    if (C.access(R).result)
      o<<",fillcolor=\"#007700\",fontcolor=white";
    else if (C.access(R).changeable)
      o<<",fillcolor=\"#770000\",fontcolor=white";
    o<<"];\n";

    // out-edges
    for(int R2: C.access(R).references)
    {
     string name2 = "n" + convertToString(R2);
     o<<name<<" -> "<<name2<<";\n";
    }

    // call-edges
    if (C.access(R).call != -1)
    {
      string name2 = "n" + convertToString(C.access(R).call);
      o<<name<<" -> "<<name2<<" ";
      o<<"[";
      o<<"color=\"#007700\"";
      o<<"];\n";
    }

    // used_inputs
    for(int R2: C.access(R).used_inputs)
    {
     string name2 = "n" + convertToString(R2);
      o<<name<<" -> "<<name2<<" ";
      o<<"[";
      o<<"color=\"#007777\"";
      o<<",style=dashed";
      o<<"];\n";
    }

  }
  o<<"}"<<std::endl;
}
