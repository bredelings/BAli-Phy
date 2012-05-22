#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include <algorithm>
#include <fstream>
#include "util.H"

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::multiset;
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


bool includes(const owner_set_t& S1, const owner_set_t& S2)
{
  return (S2 & ~S1).none();
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

expression_ref graph_normalize(const expression_ref& E)
{
  if (not E) return E;

  // 1. Var
  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E->size()) return E;
  
  // 2. Lambda
  object_ptr<const lambda> L = is_a<lambda>(E);
  if (L)
  {
    assert(E->size() == 2);
    object_ptr<expression> V ( new expression(*E) );
    V->sub[1] = graph_normalize(E->sub[1]);

    if (V->sub[1] == E->sub[1])
      return E;
    else
      return V;
  }

  // 3. Application
  if (is_a<Apply>(E))
  {
    assert(E->size() == 2);
    expression_ref f = graph_normalize(E->sub[0]);
    expression_ref x = graph_normalize(E->sub[1]);

    int var_index = get_safe_binder_index(E);
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
  object_ptr<const Case> IsCase = is_a<Case>(E);
  if (IsCase)
  {
    object_ptr<expression> V ( E->clone() );

    // Normalize the object
    V->sub[0] = graph_normalize(V->sub[0]);

    const int L = (V->sub.size()-1)/2;
    // Just normalize the bodies
    for(int i=0;i<L;i++)
      V->sub[2+2*i] = graph_normalize(V->sub[2+2*i]);
    
    if (is_reglike(V->sub[0]))
      return object_ptr<const expression>(V);
    else
    {
      int var_index = get_safe_binder_index(E);
      expression_ref x = dummy(var_index);
      expression_ref obj = V->sub[0];
      V->sub[0] = x;

      return let_expression(x,obj,V);
    }
  }

  // 4. Constructor
  if (is_a<constructor>(E) or is_a<Operation>(E))
  {
    int var_index = get_safe_binder_index(E);

    object_ptr<expression> Con ( new expression );
    Con->head = E->head;

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=0;i<E->size();i++)
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

    return let_expression(vars, bodies, object_ptr<const expression>(Con));
  }

  // 5. Let 
  object_ptr<const let_obj> Let = is_a<let_obj>(E);
  if (Let)
  {
    object_ptr<expression> V ( new expression(*E) );

    // Normalize the object
    V->sub[0] = graph_normalize(V->sub[0]);

    const int L = (V->sub.size()-1)/2;

    // Just normalize the bodies, not the vars
    for(int i=0;i<L;i++)
      V->sub[2 + 2*i] = graph_normalize(V->sub[2 + 2*i]);

    return V;
  }

  throw myexception()<<"graph_normalize: I don't recognize expression '"+ E->print() + "'";
}


reg& reg::operator=(reg&& R) noexcept
{
  owners = std::move( R.owners );
  ownership_category = std::move( R.ownership_category );
  C = std::move(R.C);
  referenced_by_in_E_reverse = std::move( R.referenced_by_in_E_reverse );
  changeable = R.changeable;
  result = R.result;
  call = R.call;
  call_reverse = std::move( R.call_reverse );
  used_inputs  = std::move( R.used_inputs );
  outputs = std::move( R.outputs );
  call_outputs = std::move( R.call_outputs );
  referenced_by_in_E = std::move( R.referenced_by_in_E );
  prev_reg = R.prev_reg;
  next_reg = R.next_reg;
  state = R.state;
  temp_owners = std::move( R.temp_owners );
  temp = R.temp;

  R.prev_reg = -1;
  R.next_reg = -1;
  R.state = reg::none;

  return *this;
}

reg::reg(reg&& R) noexcept
 :owners( std::move( R.owners ) ),
  ownership_category( std::move( R.ownership_category) ),
  C( std::move(R.C) ),
  referenced_by_in_E_reverse ( std::move( R.referenced_by_in_E_reverse ) ),
  changeable( R.changeable ),
  result( R.result ),
  call ( R.call ),
  call_reverse ( std::move( R.call_reverse) ),
  used_inputs ( std::move(R.used_inputs) ),
  outputs ( std::move( R.outputs) ),
  call_outputs ( std::move( R.call_outputs) ),
  referenced_by_in_E ( std::move( R.referenced_by_in_E) ),
  prev_reg( R.prev_reg ),
  next_reg ( R.next_reg ),
  state ( R.state ),
  temp_owners ( std::move( R.temp_owners ) ),
  temp ( R.temp )
{ 
  R.prev_reg = -1;
  R.next_reg = -1;
  R.state = reg::none;
}

void reg_heap::clear(int R)
{
  access(R).C.clear();
  access(R).changeable = false;
  clear_result(R);

  access(R).used_inputs.clear();
  access(R).call = -1;
  access(R).call_reverse = reg::back_edge_deleter();

  // Upstream objects can NOT still exist - otherwise this object would be used :-)
  access(R).outputs.clear();
  access(R).call_outputs.clear();
  access(R).referenced_by_in_E.clear();

  reg_clear_owners(R);

  // This should already be cleared.
  assert( access(R).temp == -1);
}

void reg_heap::set_used_input(int R1, int R2)
{
  assert(R1 > 0 and R1 < n_regs());
  assert(R2 > 0 and R2 < n_regs());

  assert(access(R1).C);
  assert(access(R2).C);

  // It IS possible to add an input that's already used.
  // This happens if we evaluate a new used input R2' to an already used input R2
  //   through regvar chains that are not changeable.

  // R1 shouldn't have any used inputs if it isn't changeable.
  assert(access(R1).changeable);
  // Don't add unchangeable results as inputs
  assert(access(R2).changeable);
  // Don't add a reg as input if no reduction has been performed.
  // assert(access(R2).result or access(R2).call != -1);

  reg::back_edge_deleter D = access(R2).outputs.insert(access(R2).outputs.end(), R1);
  access(R1).used_inputs.emplace_back(R2,D);
}

int count(const std::vector<int>& v, int I)
{
  int c = 0;
  for(int i: v)
    if (i == I)
      c++;
  return c;
}


// Called from: set_reg_value( ), reclaim_used_reg( ), uniquify_reg( ), incremental_evaluate( ).

void reg_heap::clear_used_inputs(int R1)
{
  assert(R1 > 0 and R1 < n_regs());

  // If this reg is unused, then upstream regs are in the process of being destroyed.
  // However, if this reg is used, then upstream regs may be live, and so should have
  //  correct edges.

  // We shouldn't need to call this on regs that are already on the free list.
  assert( access(R1).state != reg::free );

  // Remove the back edges from each used_input reg that is not on the free list.
  for(const auto& i: access(R1).used_inputs)
  {
    int R2 = i.first;
    assert(R2 > 0 and R2 < n_regs());

    if (access(R2).state == reg::free)
      assert( access(R2).outputs.empty() );
    else
    {
      assert( not access(R2).outputs.empty() );

      reg::back_edge_deleter D = i.second;
      assert( *D == R1 );

      access(R2).outputs.erase(D);
    }
  }

  // Remove the forward edges.
  access(R1).used_inputs.clear();

  assert(access(R1).used_inputs.empty());
}

// set_call (or set_call_unsafe) is only called when
// 1. uniquify_reg( ): A  call is being remapped
// 2. incremental_evaluate( ):
// - an existing call is being remapping to the end of an unchangeable indirection chain.
// - access(R).C is a reg_var
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
  access(R1).call_reverse = access(R2).call_outputs.insert(access(R2).call_outputs.end(), R1);
  assert( *access(R1).call_reverse == R1 );

  // check that all of the owners of R are also owners of R.call;
  assert( reg_is_owned_by_all_of(R2, get_reg_owners(R1)) );
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
  assert(R2 > 0 and R2 < n_regs());
  
  assert( *access(R).call_reverse == R );
  access(R).call = -1;

  // If this reg is unused, then upstream regs are in the process of being destroyed.
  // However, if this reg is used, then upstream regs may be live, and so should have
  //  correct edges.
  assert( access(R).state != reg::used or access(R2).call_outputs.count(R) );

  // If the call points to a freed reg, then its call_outputs list should already be cleared.
  if (access(R2).state == reg::free)
    assert( access(R2).call_outputs.empty() );
  // If the call points to a used reg, then we need to notify it that the incoming call edge is being removed.
  else {
    assert( access(R2).state == reg::used or access(R2).state == reg::checked );
    assert( not access(R2).call_outputs.empty() );
    access(R2).call_outputs.erase( access(R).call_reverse );
  }

  access(R).call_reverse = reg::back_edge_deleter();
}

void reg_heap::set_C(int R, closure&& C)
{
  assert(C);
  assert(not is_a<expression>(C.exp));
  assert(not reg_is_unowned(R) );
  clear_C(R);

  access(R).C = std::move(C);
#ifndef NDEBUG
  for(int r: access(R).C.Env)
  {
    assert(0 <= r and r < n_regs());

    // check that all of the owners of R are also owners of *r.
    assert(reg_is_owned_by_all_of(r, get_reg_owners(R)) );

    // check that *r is not already marked as being referenced by R
    assert(not access(r).referenced_by_in_E.count(R) );
  }
#endif

  // mark R2 as being referenced by R
  for(int R2: access(R).C.Env)
  {
    reg::back_edge_deleter D = access(R2).referenced_by_in_E.push_back(R);
    access(R).referenced_by_in_E_reverse.push_back(D);
  }
}

void reg_heap::clear_C(int R)
{
  for(int i=0;i<access(R).C.Env.size();i++)
  {
    int R2 = access(R).C.Env[i];
    reg::back_edge_deleter& D = access(R).referenced_by_in_E_reverse[i];
    if (access(R2).state != reg::free)
    {
      assert( not access(R2).referenced_by_in_E.empty() );
      access(R2).referenced_by_in_E.erase(D);
    }
    else
      assert( access(R2).referenced_by_in_E.empty() );
  }

  access(R).C.clear();
  access(R).referenced_by_in_E_reverse.clear();
}

void reg_heap::set_reduction_result(int R, closure&& result)
{
  // Check that there is no result we are overriding
  assert(not access(R).result );

  // Check that there is no previous call we are overriding.
  assert(access(R).call == -1);

  // if the result is NULL, just leave the result and call both unset.
  //  (this could happen if we set a parameter value to null.)
  if (not result) return;

  // If the value is a pre-existing reg_var, then call it.
  if (object_ptr<const index_var> V = is_a<index_var>(result.exp))
  {
    int Q = result.lookup_in_env( V->index );
    
    assert(0 <= Q and Q < n_regs());
    assert(access(Q).state == reg::used);
    
    set_call(R,Q);
  }
  // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the result and call it.
  else
  {
    root_t r = allocate_reg();
    set_reg_ownership_category(*r, get_reg_ownership_category(R));
    set_C(*r, std::move( result ) );
    set_call(R, *r);
    pop_root(r);
  }
}

/// Update the value of a non-constant, non-computed index
void reg_heap::set_reg_value(int P, closure&& C, int token)
{
  // Check that reg P is owned by context token.
  assert(reg_is_owned_by(P,token));

  // Split this reg and its E-ancestors out from other graphs, if its shared.
  P = uniquify_reg(P,token);

  // Check that this reg is indeed settable
  assert(is_parameter(access(P).C.exp));
  assert(access(P).changeable);

  // Clear the call, clear the result, and set the value
  assert(access(P).used_inputs.empty());
  clear_call(P);
  clear_result(P);

  const int mark_call_result = 1;
  const int mark_result = 2;

  vector< int > call_and_result_may_be_changed;
  call_and_result_may_be_changed.reserve(n_regs());
  vector< int > result_may_be_changed;
  result_may_be_changed.reserve(n_regs());

  // The index that we just altered cannot be known to be unchanged.
  call_and_result_may_be_changed.push_back(P);
  access(P).temp = mark_call_result;
  result_may_be_changed.push_back(P);

  int i=0;
  int j=0;
  while(i < call_and_result_may_be_changed.size() or j < result_may_be_changed.size())
  {
    // First find all users or callers of regs where the result is out of date.
    for(;j<result_may_be_changed.size();j++)
    {
      int R1 = result_may_be_changed[j];
      assert(access(R1).temp == mark_call_result or access(R1).temp == mark_result);

      // Since the computation may be different, we don't know if the value has changed.
      clear_result(R1);

      // Scan regs that used R2 directly and put them on the invalid-call/result list.
      for(int R2: access(R1).outputs)
      {
	if (access(R2).temp == mark_call_result) continue;
	access(R2).temp = mark_call_result;
	call_and_result_may_be_changed.push_back(R2);
      }

      // Scan regs that call R2 directly and put them on the invalid-result list.
      for(int R2: access(R1).call_outputs)
      {
	if (access(R2).temp != -1) continue;
	access(R2).temp = mark_result;
	result_may_be_changed.push_back(R2);
      }
    }

    // Second find all users or callers of regs where the result AND CALL are out of date.
    for(;i<call_and_result_may_be_changed.size();i++)
    {
      int R1 = call_and_result_may_be_changed[i];
      assert(access(R1).temp == mark_call_result);

      // Since the computation may be different, we don't know if the value has changed.
      clear_result(R1);
      // We don't know what the reduction result is, so invalidate the call.
      clear_call(R1);
      // Remember to clear the used inputs.
      clear_used_inputs(R1);

      // Scan regs that used R2 directly and put them on the invalid-call/result list.
      for(int R2: access(R1).outputs)
      {
	if (access(R2).temp == mark_call_result) continue;
	access(R2).temp = mark_call_result;
	call_and_result_may_be_changed.push_back(R2);
      }

      // Scan regs that call R2 directly and put them on the invalid-result list.
      for(int R2: access(R1).call_outputs)
      {
	if (access(R2).temp != -1) continue;
	access(R2).temp = mark_result;
	result_may_be_changed.push_back(R2);
      }
    }
  }

#ifndef NDEBUG
  for(int R: result_may_be_changed)
    assert(access(R).temp == mark_result or access(R).temp == mark_call_result);

  for(int R: call_and_result_may_be_changed)
    assert(access(R).temp == mark_call_result);
#endif

  // Clear the marks
  for(int R: result_may_be_changed)
    access(R).temp = -1;

  // Clear the marks
  for(int R: call_and_result_may_be_changed)
    access(R).temp = -1;

  // Finally set the new value.
  set_reduction_result(P, std::move(C) );
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
  if (first_free_reg == -1) return -1;

  int r = first_free_reg;

#ifndef NDEBUG
  {
    const reg& R = access(r);
    
    assert(not R.C);
    assert(R. referenced_by_in_E_reverse.empty());
    assert(not R.result);
    assert(R.call == -1);
    assert(R.used_inputs.empty());
    assert(R.call_outputs.empty());
    assert(R.referenced_by_in_E.empty());
  }
#endif

  assert(access(r).state == reg::free);
  first_free_reg = access(r).next_reg;
  access(r).prev_reg = -1;
  access(r).next_reg = -1;
  access(r).state = reg::none;

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

  // Upstream regs must also be dead, since if they were live, this reg would be live as well.
  // Therefore, we do not need to update upstream regs even when we destroy incoming edges.

  // However, downstream regs may be live, and therefore when we destroy outgoing edges, we
  // need to notify downstream regs of the absence of these incoming edges.
  clear_used_inputs(r);
  clear_call(r);
  clear_C(r);

  add_reg_to_free_list(r);
}

reg_heap::root_t reg_heap::push_root(int R)
{
  assert(0 <= R and R < n_regs());
  return roots.insert(roots.end(), R);
}

void reg_heap::pop_root(reg_heap::root_t r)
{
  if (r != roots.end())
    roots.erase(r);
}

reg_heap::root_t reg_heap::push_temp_head(int t)
{
  owner_set_t tokens;
  tokens.set(t,true);
  return push_temp_head(tokens);
}

reg_heap::root_t reg_heap::push_temp_head(const owner_set_t& tokens)
{
  root_t r = allocate_reg();
  set_reg_owners( *r, tokens );
  for(int t=0;t< tokens.size();t++)
  {
    if (not tokens.test(t)) continue;

    token_roots[t].temp.push_back(r);
  }

  return r;
}

void reg_heap::pop_temp_head(int t)
{
  owner_set_t tokens;
  tokens.set(t,true);
  pop_temp_head(tokens);
}



void reg_heap::pop_temp_head(const owner_set_t& tokens)
{
  int t0 = -1;
  root_t r0;

  for(int t=0;t< tokens.size();t++)
  {
    if (not tokens.test(t)) continue;
    root_t r = token_roots[t].temp.back();

    if (t0 == -1)
    {
      t0 = t;
      r0 = r;
    }
    else
      assert( r == r0 );

    assert( reg_is_owned_by(*r, t) );
    token_roots[t].temp.pop_back();
  }

  pop_root(r0);
}

void reg_heap::expand_memory(int s)
{
  assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());

  int k = memory.size();
  memory.resize(memory.size()+s);
  for(int i=k;i<memory.size();i++)
    add_reg_to_free_list(i);

  assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());
}

reg_heap::root_t reg_heap::allocate_reg()
{
  assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());

  int r = get_free_reg();

  // allocation failed
  if (r == -1)
  {
    collect_garbage();
    assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());
    if (memory.size() < n_used_regs()*2+10)
      expand_memory(memory.size()*2+10);
    r = get_free_reg();
    assert(r != -1);
  }

  add_reg_to_used_list(r);
  access(r).ownership_category = ownership_categories.begin();

  assert( reg_is_unowned(r) );

  assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());
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
    R.temp_owners = get_reg_owners(here);
#endif
    R.owners.reset();

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
      access(R).owners.set(t,true);
    }
  }

#ifndef NDEBUG
  // Check that we did not ADD any ownership marks!
  here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    assert( includes(R.temp_owners, R.owners) );
    R.temp_owners.reset();

    here = R.next_reg;
  }
#endif
}

void reg_heap::trace_and_reclaim_unreachable()
{
  vector<int> scan;
  scan.reserve(n_regs());

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

      // Count the references from E
      next_scan.insert(next_scan.end(), R.C.Env.begin(), R.C.Env.end());

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

void reg_heap::compute_ownership_categories()
{
  ownership_categories.clear();
  canonical_ownership_categories.clear();
  {
    owner_set_t empty;
    canonical_ownership_categories[empty] = ownership_categories.push_back(empty);
  }

  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);

    set_reg_owners(here, access(here).owners );

    here = R.next_reg;
  }
}

void reg_heap::collect_garbage()
{
#ifndef NDEBUG
  std::cerr<<"***********Garbage Collection******************"<<std::endl;
  check_used_regs();
#endif
  assert(n_regs() == n_used_regs() + n_free_regs() + n_null_regs());

  trace_and_reclaim_unreachable();

#ifndef NDEBUG
  cerr<<"Regs: "<<n_used_regs()<<"/"<<n_regs()<<endl;
  cerr<<"#roots = "<<roots.size()<<endl;
  check_used_regs();
#endif

  // Currently called only from garbage collector.
  remove_unused_ownership_marks();

  compute_ownership_categories();

  // Check that we have no un-owned objects that are used
  int here = first_used_reg;
  for(;here != -1;)
  {
    reg& R = access(here);
    assert(not reg_is_unowned(here) );

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
  unique.reserve(n_regs());
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

int reg_heap::remap_reg(int R) const
{
  int R2 = access(R).temp;
  if (R2 == -1)
    return R;
  else
  {
    assert(R2 > 0 and R2 < n_regs());
    assert(access(R2).state == reg::used);
    return R2;
  }
}

closure reg_heap::remap_regs(closure C) const
{
  for(int& R: C.Env)
    R = remap_reg(R);

  return C;
}

const owner_set_t& reg_heap::get_reg_owners(int R) const
{
  return *get_reg_ownership_category(R);
}

const ownership_category_t& reg_heap::get_reg_ownership_category(int R) const
{
  assert(access(R).ownership_category != ownership_categories.end());
  //  assert(access(R).owners == *access(R).ownership_category);
  return access(R).ownership_category;
}

void reg_heap::set_reg_owners(int r, const owner_set_t& owners)
{
  // Find or create the category for this specific bitmask.
  if (not canonical_ownership_categories.count(owners))
    canonical_ownership_categories[owners] = ownership_categories.push_back(owners);

  set_reg_ownership_category(r, canonical_ownership_categories[owners] );
}

void reg_heap::set_reg_ownership_category(int r, const ownership_category_t& c)
{
  reg& R = access(r);
  R.ownership_category = c;
  //  R.owners = *c;

  assert(R.ownership_category != ownership_categories.end());
  //  assert(R.owners == *R.ownership_category);
}

void reg_heap::reg_add_owner(int r, int t)
{
  owner_set_t owners = get_reg_owners(r);
  owners.set(t, true);
  set_reg_owners(r, owners);
}

void reg_heap::reg_clear_owner(int r, int t)
{
  owner_set_t owners = get_reg_owners(r);
  owners.set(t, false);
  set_reg_owners(r, owners);
}

void reg_heap::reg_clear_owners(int r)
{
  set_reg_ownership_category(r, ownership_categories.begin() );
  assert( access(r).ownership_category->none());
}

bool reg_heap::reg_is_owned_by(int r, int t) const
{
  return get_reg_owners(r).test(t);
}

bool reg_heap::reg_is_owned_by_only(int r, int t) const
{
  owner_set_t owners;
  owners.set(t,true);
  return owners == get_reg_owners(r);
}

bool reg_heap::reg_is_owned_by_all_of(int r, const owner_set_t& owners) const
{
  return includes(get_reg_owners(r), owners);
}

int reg_heap::n_reg_owners(int r) const
{
  return get_reg_owners(r).count();
}

bool reg_heap::reg_is_unowned(int r) const
{
  return get_reg_owners(r).none();
}

bool reg_heap::reg_is_shared(int r) const
{
  return n_reg_owners(r) > 1;
}

void reg_heap::check_results_in_context(int t) const
{
  vector<int> WHNF_results;
  vector<int> regs = find_all_regs_in_context(t);
  for(int Q: regs)
  {
    if (access(Q).call != -1)
      assert( *access(Q).call_reverse == Q );
      
    if (access(Q).result and access(Q).call == -1)
    {
      assert(access(Q).result == Q);
      WHNF_results.push_back(Q);
    }
  }

  // Check the call outputs
  for(int Q: WHNF_results)
  {
    vector<int> regs = find_call_ancestors_in_context( Q, t);

    for(int j=0;j<regs.size();j++)
      if (access(regs[j]).result)
	assert( access(regs[j]).result == Q );
  }
}

vector<int> reg_heap::find_unsplit_parents(const vector<int>& split, int t) const
{
  vector<int> unsplit_parents;

  for(int R1: split)
  {
    // parents are: (a) referenced_by_in_E + (b) outputs + (c) call_outputs

    // NOTE: we could have parent that are in t that are shared, but these
    // should be original regs that will eventually be removed from t.

    for(int Q1: access(R1).referenced_by_in_E)
    {
      // Skip regs that we've handled already.
      if (access(Q1).state == reg::checked) continue;

      // We are only interested in the unshared E-ancestors in t.
      if (not reg_is_owned_by_only(Q1, t)) continue;

      // Mark Q1
      assert(access(Q1).state == reg::used);
      access(Q1).state = reg::checked;
      unsplit_parents.push_back(Q1);
    }

    for(int Q1: access(R1).outputs)
    {
      // Skip regs that we've handled already.
      if (access(Q1).state == reg::checked) continue;

      // We are only interested in the unshared E-ancestors in t.
      if (not reg_is_owned_by_only(Q1, t)) continue;

      // Mark Q1
      assert(access(Q1).state == reg::used);
      access(Q1).state = reg::checked;
      unsplit_parents.push_back(Q1);
    }

    for(int Q1: access(R1).call_outputs)
    {
      // Skip regs that we've handled already.
      if (access(Q1).state == reg::checked) continue;

      // We are only interested in the unshared E-ancestors in t.
      if (not reg_is_owned_by_only(Q1, t)) continue;

      // Mark Q1
      assert(access(Q1).state == reg::used);
      access(Q1).state = reg::checked;
      unsplit_parents.push_back(Q1);
    }
  }

  // Unmark the unsplit parents;
  for(int i=0;i<unsplit_parents.size();i++)
    access(unsplit_parents[i]).state = reg::used;

#ifndef NDEBUG
  // Check that marks were removed.
  for(int R1: split)
  {
    int R2 = remap_reg(R1);

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
#endif

  return unsplit_parents;
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

  // Track WHNF regs that have moved.
  vector<int> changed_results;

  // 1. Find all ancestors with name 't' that are *shared*
  // (Some of these could be unreachable!)
  vector<int> shared_ancestors = find_shared_ancestor_regs_in_context(R,t);
  int n_new_regs = shared_ancestors.size();

  // 2. Allocate new regs for each *shared* ancestor reg in context t
  for(int R1:shared_ancestors)
    push_temp_head(t);

  const std::vector<root_t>& temp_heads = get_temp_heads_for_context(t);

  // 4e. Initialize/Copy changeable
  // 2. Remove regs that got deallocated from the list.
  // Alternatively, I could LOCK them in place.
  vector<int> split;
  split.reserve(shared_ancestors.size());
  for(int i=0;i<shared_ancestors.size();i++)
  {
    int R1 = shared_ancestors[i];

    if (access(R1).state == reg::used and reg_is_shared(R1))
    {
      int R2 = *temp_heads[temp_heads.size()-1-i];
      access(R1).temp = R2;
      access(R2).changeable = access(R1).changeable;
      split.push_back(R1);
    }
  }


  // NOTE: We HAVE to quit now since R has been removed from new_regs.
  // If for some reason the reg is no longer shared, then we can quit now.
  assert( reg_is_owned_by(R, t) );
  if (not reg_is_shared(R)) 
  {
    for(int i=0;i<n_new_regs;i++)
      pop_temp_head(t);

    assert(token_roots[t].temp.empty());
    
    for(int R1: split)
      access(R1).temp = -1;
    
#ifndef NDEBUG
    for(int R1: shared_ancestors)
      assert( access(R1).temp == -1);
    
    check_results_in_context(t);
#endif  
    
    return R;
  }

  // 2a. Copy the over and remap C
  for(int R1: split)
  {
    int R2 = remap_reg(R1);

    // Check no mark on R2
    assert(access(R1).state == reg::used);
    assert(access(R2).state == reg::used);
    
    assert( not reg_is_unowned(R1) );

    // 4. Initialize fields in the new node

    // 4a. Initialize/Remap C
    set_C(R2, remap_regs( access(R1).C ) );
  }

  // 2b.  Copy over and remap the call, used_inputs, and result
  //      This is after copying C to avoid linking to regs with no C
  for(int R1: split)
  {
    int R2 = remap_reg(R1);

    // 4b. Initialize/Remap call
    if (access(R1).call != -1)
      set_call(R2, remap_reg(access(R1).call ) );

    // 4c. Initialize/Remap used_inputs
    for(const auto& i: access(R1).used_inputs)
      set_used_input(R2, remap_reg(i.first) );

    // 4d. Initialize/Remap result if E is in WHNF.
    if (access(R2).call == -1 and access(R1).result)
    {
      assert( access(R1).result == R1);
      access(R2).result = R2;
      changed_results.push_back(R2);
    }
    // 4d. Initialize/Copy result otherwise.
    else
    {
      assert( access(R1).result != R1);
      // Q: Why is it OK to use the un-remapped result?
      // A: Because we remap calls here; later we trace backwards along these
      //    remapped call chains to set any results to the proper WHNF expression.
      access(R2).result = access(R1).result;
    }
  }

  // 4a. Adjust heads to point to the new regs
  for(int j=0;j<token_roots[t].heads.size();j++)
  {
    int R1 = *token_roots[t].heads[j];
    *token_roots[t].heads[j] = remap_reg(R1);
  }

  // 4b. Adjust parameters to point to the new regs
  for(int j=0;j<token_roots[t].parameters.size();j++)
  {
    int R1 = *token_roots[t].parameters[j];
    *token_roots[t].parameters[j] = remap_reg(R1);
  }

  // 4c. Adjust identifiers to point to the new regs
  for(const auto& j: token_roots[t].identifiers)
  {
    // Hmmm.... this could be a lot of identifiers to scan...
    int R1 = *j.second;
    *j.second = remap_reg(R1);
  }

  // 5. Find the unsplit parents of split regs
  //    These will be the only parents of the old regs that have context t.
  vector<int> unsplit_parents = find_unsplit_parents(split, t);
  
  // Remap the unsplit parents. (The parents don't move, but they reference children that do.)
  for(int Q1: unsplit_parents)
  {
    // a. Remap E
    set_C(Q1, remap_regs(access(Q1).C ) );
    
    // b. Remap call
    if (access(Q1).call != -1)
    {
      int old_call = access(Q1).call;
      int new_call = remap_reg( old_call );

      if (old_call != new_call)
      {
	clear_call(Q1);
	set_call_unsafe(Q1, new_call);
      }
    }
    
    // c. Adjust use edges
    vector< pair<int,reg::back_edge_deleter> > old_used_inputs = access(Q1).used_inputs;
    clear_used_inputs(Q1);
    for(const auto& i: old_used_inputs)
      set_used_input(Q1, remap_reg(i.first));
  }

  // Remove ownership from the old regs.
  for(int Q: split)
  {
    // These regs should be shared.
    assert(reg_is_shared(Q));

    // These regs should have originally contained t.
    assert( reg_is_owned_by(Q, t) );

    // But now remove membership in t from these regs.
    reg_clear_owner(Q, t);
  }

  // Update regs that indirectly call WHNF regs that have moved.
  for(int Q: changed_results)
  {
    assert(access(Q).result == Q);

    vector<int> regs = find_call_ancestors_in_context( Q, t);
    for(int S: regs)
    {
      access(S).result = Q;

      // In general, the owners of a parent (S) should all be owners of a child (S).
      // This allows S to have no owners, which could happen if S became unreachable.

      // Any call ancestors of E-ancestors of p should be E-ancestors of p, and therefore should be in t.
      assert(reg_is_unowned(S) or reg_is_owned_by(S,t));

      // Any call ancestors of E-ancestors of p should be E-ancestors of p, and therefore should be uniquified.
      assert(not reg_is_shared(S));
    }
  }

#ifndef NDEBUG
  // This checks that ownership and references are consistent
  find_all_regs_in_context(t);
#endif

#ifndef NDEBUG
  for(int R1: split)
  {
    int R2 = remap_reg(R1);

    // Check that ownership has been properly split
    assert(not reg_is_owned_by(R1,t) );
    assert(reg_is_owned_by(R2, t));
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
  //    Remove remapping info from regs.
  for(int i=0;i<n_new_regs;i++)
    pop_temp_head(t);

  assert(token_roots[t].temp.empty());

  int R2 = remap_reg(R);

  for(int R1: split)
    access(R1).temp = -1;

#ifndef NDEBUG
  for(int R1: shared_ancestors)
    assert( access(R1).temp == -1);

  check_results_in_context(t);
#endif  

  assert(R2 != R);
  return R2;
}

void reg_heap::check_used_reg(int index) const
{
  const reg& R = access(index);

  // This should check the ownership is working correctly.
  // (i.e. Does the bitmap match the category bitmap?  It might not need too.)
  get_reg_owners(index);

  for(int r: R.C.Env)
  {
    // Check that referenced regs are owned by the owners of R
    assert(reg_is_owned_by_all_of(r, get_reg_owners(index) ) );
    
    // Check that referenced regs are have back-references to R
    assert(access(r).referenced_by_in_E.count(index) );
  }
  
  for(const auto& i: R.used_inputs)
  {
    int r = i.first;

    // Check that used regs are owned by the owners of R
    assert( reg_is_owned_by_all_of(r, get_reg_owners(index) ) );

    // Check that used regs are have back-references to R
    assert( access(r).outputs.count(index) );
  }

  if (R.call != -1)
  {
    // Check that the pointer to the reverse edge iterator is intact.
    assert( *R.call_reverse == index );

    // Check that the call-used reg is owned by owners of R
    assert( reg_is_owned_by_all_of(R.call, get_reg_owners(index) ) );

    // Check that the call-used reg has back-references to R
    assert( access(R.call).call_outputs.count(index) == 1 );
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

// TODO - search: shared memory garbage collection.

void reg_heap::remove_ownership_mark(int t)
{
#ifndef NDEBUG
  for(const auto& i: canonical_ownership_categories)
    assert(i.first == *i.second);
  check_used_regs();
#endif

  // Clear ownership marks for token t and recompute the canonical reverse hashes.
  canonical_ownership_categories.clear();
  for(auto i = ownership_categories.begin(); i != ownership_categories.end(); i++)
  {
    i->set(t,false);
    if (not canonical_ownership_categories.count(*i))
      canonical_ownership_categories[*i] = i;
  }
  /*
  int here = first_used_reg;
  for(;here != -1;here = access(here).next_reg)
    access(here).owners.set(t,false);
  */

#ifndef NDEBUG
  for(const auto& i: canonical_ownership_categories)
    assert(i.first == *i.second);
  check_used_regs();
#endif
}

void reg_heap::duplicate_ownership_mark(int t1, int t2)
{
#ifndef NDEBUG
  for(const auto& i: canonical_ownership_categories)
    assert(i.first == *i.second);
  check_used_regs();
#endif

  // Clear ownership marks for token t and recompute the canonical reverse hashes.
  canonical_ownership_categories.clear();
  for(auto i = ownership_categories.begin(); i != ownership_categories.end(); i++)
  {
    if (i->test(t1))
      i->set(t2,true);
    if (not canonical_ownership_categories.count(*i))
      canonical_ownership_categories[*i] = i;
  }

  /*
  int here = first_used_reg;
  for(;here != -1;here = access(here).next_reg)
    if (access(here).owners.test(t1))
      access(here).owners.set(t2,true);
  */

#ifndef NDEBUG
  for(const auto& i: canonical_ownership_categories)
    assert(i.first == *i.second);
  check_used_regs();
#endif
}

vector<int> reg_heap::find_all_regs_in_context_no_check(int t) const
{
  vector<int> scan;
  scan.reserve(roots.size());
  for(const auto& i: token_roots[t].temp)
    scan.push_back(*i);

  for(const auto& i: token_roots[t].heads)
    scan.push_back(*i);

  for(const auto& i: token_roots[t].parameters)
    scan.push_back(*i);

  for(const auto& i: token_roots[t].identifiers)
    scan.push_back(*(i.second));

  vector<int> unique;
  unique.reserve(n_regs());
  for(int i=0;i<scan.size();i++)
  {
    const reg& R = access(scan[i]);
    assert(R.state != reg::free and R.state != reg::none);
    if (R.state == reg::checked) continue;

    R.state = reg::checked;
    unique.push_back(scan[i]);
  }

  for(int i=0;i<unique.size();i++)
  {
    const reg& R = access(unique[i]);
    assert(R.state != reg::free and R.state != reg::none);
    assert(R.state == reg::checked);

    for(int j:R.C.Env)
    {
      const reg& R2 = access(j);
      if (R2.state == reg::used)
      {
	R2.state = reg::checked;
	unique.push_back(j);
      }
    }

    // Count also the references from the call
    if (R.call != -1 and access(R.call).state == reg::used)
    {
      access(R.call).state = reg::checked;
      unique.push_back(R.call);
    }
  }

#ifndef NDEBUG
  for(int i=0;i<unique.size();i++)
    for(int j=0;j<i;j++)
      assert(unique[i] != unique[j]);
#endif

  for(int i=0;i<unique.size();i++)
  {
    const reg& R = access(unique[i]);
    assert(R.state == reg::checked);

    R.state = reg::used;
  }

  return unique;
}

// This routine is separate from the *_no_check variant because the
// checks don't hold in all cases.
vector<int> reg_heap::find_all_regs_in_context(int t) const
{
  vector<int> unique = find_all_regs_in_context_no_check(t);

#ifndef NDEBUG
  for(int R: unique)
  {
    assert(reg_is_owned_by(R,t));
    check_used_reg(R);
  }
#endif

  return unique;
}

void reg_heap::release_token(int t)
{
  assert(token_is_used(t));

  // NOTE: Don't spent more than O(used_regs) time clearing ownership.
  //       This strategy allows us to be on the same order of magnitude
  //       as copying and de-allocating the structure instead of sharing.

  // NOTE: Clearing ownership is not NECESSARY but it avoids updating
  //       unused ancestors when we change parameters.

  // remove ownership marks on all of our used regs.

#ifdef NDEBUG
  remove_ownership_mark(t);
#else
  find_all_regs_in_context(t);
#endif


  // We shouldn't have any temporary heads still on the stack, here!
  assert(token_roots[t].temp.empty());

  // remove the roots for the heads of graph t
  for(const auto& i: token_roots[t].heads)
    pop_root(i);
  token_roots[t].heads.clear();

  // remove the roots for the parameters of graph t
  for(const auto&i: token_roots[t].parameters)
    pop_root(i);
  token_roots[t].parameters.clear();

  for(const auto& i: token_roots[t].identifiers)
    pop_root(i.second);
  token_roots[t].identifiers.clear();

  // mark token for this context unused
  unused_tokens.push_back(t);
  token_roots[t].used = false;

  // This is a good tradeoff between clearing ALL unused ownership (which is too expensive)
  // and clearing no unused ownership (which makes uniquify reg do too much extra work)

#ifndef NDEBUG
  // ISSUE!  Now parents of the regs whose ownership we cleared may still
  //         reference children they don't own.
  check_used_regs();
#endif
}

bool reg_heap::token_is_used(int t) const
{
  return token_roots[t].used;
}

int reg_heap::copy_token(int t)
{
  int t2 = get_unused_token();

  assert(token_roots[t].temp.empty());

  for(const auto& i: token_roots[t].heads)
    token_roots[t2].heads.insert( token_roots[t2].heads.end(), push_root(*i) );

  for(const auto& i: token_roots[t].parameters)
    token_roots[t2].parameters.insert( token_roots[t2].parameters.end(), push_root(*i) );

  token_roots[t2].identifiers = token_roots[t].identifiers;
  for(auto& i: token_roots[t2].identifiers)
    i.second = push_root(*i.second);

  // remove ownership mark from used regs in this context
  duplicate_ownership_mark(t, t2);

  return t2;
}

int reg_heap::n_null_regs() const
{
  return 1;
}

reg_heap::root_t reg_heap::add_identifier_to_context(int t, const string& name)
{
  map<string,root_t>& identifiers = get_identifiers_for_context(t);

  // if there's already an 's', then complain
  if (identifiers.count(name))
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already an identifier with that name.";

  root_t r = allocate_reg();
  reg_add_owner(*r, t);
  identifiers[name] = r;
  return r;
}

reg_heap::reg_heap()
  :first_free_reg(-1),
   first_used_reg(-1)
{ 
  memory.resize(1);

  owner_set_t empty;
  canonical_ownership_categories[empty] = ownership_categories.push_back(empty);
}

#include "computation.H"

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs: public OperationArgs
{
  const int R;

  reg_heap& M;

  const int t;

  owner_set_t owners;

  int n_allocated;

  const closure& current_closure() const {return M[R].C;}

  const expression& get_E() const {return *current_closure().exp;}

  /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
  int lazy_evaluate_reg(int R2)
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

    return R2;
  }

  /// Evaluate the reg R2, record dependencies, and return the result.
  const closure& lazy_evaluate_reg_closure(int R2)
  {
    return M.access_result( lazy_evaluate_reg(R2) );
  }

  /// Evaluate the reg R2, record dependencies, and return the result.
  const object_ref evaluate_reg(int R2)
  {
    return M.access_result( lazy_evaluate_reg(R2) ).exp->head;
  }

  // Note: see note below on evaluate_structure( ) on the issue of returning lambdas.

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
  
  expression_ref evaluate_structure_(closure C)
  {
    if (object_ptr<const index_var> V = is_a<index_var>(C.exp) )
    {
      int R2 = C.lookup_in_env(V->index);
      int R3 = lazy_evaluate_reg(R2);

      /* IDEA: only allow evaluation of reg_vars, constants, and constructors 
	       any reg_var that evaluates to a lambda stays a reg_var.
	       that is the only use way of using the result.
       */

      return evaluate_structure_( M.access_result(R3) );
    }
    else if (C.exp->size())
    {
      // If the "structure" is a lambda function, then we are done.
      // (a) if we were going to USE this, we should just call lazy evaluate! (which return a heap variable)
      // (b) if we are going to PRINT this, then we should probably normalize it more fully....?
      // See note above on returning lambdas as reg_vars.
      if (is_a<lambda2>(C.exp)) return C.exp;

      assert(is_a<constructor>(C.exp));

      // If the result is a constructor expression, then evaluate its fields also.
      object_ptr<expression> V = C.exp->clone();
      
      for(int i=0;i<V->size();i++)
	V->sub[i] = evaluate_structure_({C.exp->sub[i], C.Env});

      return V;
    }
    else
      return C.exp;
  }

public:

  expression_ref reference(int slot) const
  {
    return get_E().sub[slot];
  }

  // This computes everything
  const closure& lazy_evaluate(int slot)
  {
    int index = assert_is_a<index_var>(reference(slot))->index;

    int R2 = M[R].C.lookup_in_env(index);

    R2 = lazy_evaluate_reg(R2);

    /*
     * We could update 'index' in Env to R2' if R2' != R2.  However:
     *
     * - Updating WHNF regs is problematic because ... we never evaluate them :-)
     *
     * - Updating non-WHNF regs is problematic because we might need to update the used_inputs
     *   to refer to the new reg.  This is because the old one might become unused
     *   (and therefore be garbage-collected.)
     *
     * Therefore, we might consider updating index_var chains during garbage collection.
     */
    
    return M.access_result(R2);
  }

  // This fills out an entire structure!
  expression_ref evaluate_structure(int slot)
  {
    return evaluate_structure_(lazy_evaluate(slot));
  }

  // This just returns the head of the structure.
  object_ref evaluate(int slot)
  {
    expression_ref result = lazy_evaluate(slot).exp;
    assert(not is_a<lambda2>(result));
    return result->head;
  }

  object_ref evaluate_expression(const expression_ref&)
  {
    std::abort();
  }

  int allocate(closure&& C)
  {
    int r = *M.push_temp_head( owners );
    M.set_C(r, std::move(C) );
    n_allocated++;
    return r;
  }

  int n_args() const {return get_E().sub.size();}

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, reg_heap& m, int T)
    :R(r),M(m),t(T),owners(M.get_reg_owners(R)), n_allocated(0)
  { 
    // I think these should already be cleared.
    assert(M.access(R).used_inputs.empty());
  }

  ~RegOperationArgs()
  {
    for(int i=0;i<n_allocated;i++)
      M.pop_temp_head( owners );
  }
};


expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, reg_heap::root_t>&);
expression_ref untranslate_vars(const expression_ref& E, const map<string, reg_heap::root_t>& ids);

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

// Perhaps rewrite the expression system to
// (a) Separate the head (object_ref) from the other args (expression_ref)
// (b) Make a constructor take some number of arguments.
// (c) Change the interpretation of closure constructors so that they are always C n n-1 ... 1 0.
//     I guess if we don't then we have to actually look into the constructor expression.
// (d) Remove Operation::evaluate( ) and just use lazy_evaluate( ).
// (e) Make translate_refs use only one names for refs that occur twice.
// (f) Make a pretty printer for expression_ref?

/// Evaluate R and look through reg_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not a reg_var.
int reg_heap::incremental_evaluate(int R, int t)
{
  assert(R > 0 and R < n_regs());
  assert(access(R).state == reg::used);
  assert(reg_is_owned_by(R,t));
  assert(not is_a<expression>(access(R).C.exp));
  assert(not access(R).result or is_WHNF(access_result(R).exp));
  assert(not access(R).result or not is_a<expression>(access_result(R).exp));
  assert(not access(R).result or not is_a<index_var>(access_result(R).exp));

#ifndef NDEBUG
  //  if (not access(R).result) std::cerr<<"Statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

  while (not access(R).result)
  {
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    assert(access(R).C.exp);

#ifndef NDEBUG
    //    std::cerr<<"   statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

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

    else if (object_ptr<const index_var> V = is_a<index_var>(access(R).C.exp))
    {
      assert( access(R).call == -1);

      int R2 = access(R).C.lookup_in_env( V->index );

      int C = incremental_evaluate(R2, t);

      set_call(R, C);

      assert(not access(R).changeable);

      access(R).changeable = access(C).changeable;

      access(R).result = access(C).result;

      // If we point to C through an intermediate reg_var chain, then change us to point to the end
      if (C != R2) {
	// FIXME - eventually 
	set_C(R, closure(index_var(0),{C}));
      }

      return C;
    }

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(access(R).C.exp))
      access(R).result = R;

#ifndef NDEBUG
    else if (is_a<Trim>(access(R).C.exp))
      std::abort();
#endif

    // A parameter has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (is_parameter(access(R).C.exp))
      throw myexception()<<"Parameter with no result?! (Changeable = "<<access(R).changeable<<")";

    // Reduction: let expression
    else if (parse_indexed_let_expression(access(R).C.exp, bodies, T))
    {
      owner_set_t owners = get_reg_owners(R);

      vector<int> local_env = access(R).C.Env;

      vector<int> new_heap_vars;
      for(int i=0;i<bodies.size();i++)
      {
	// FIXME - do we really want to add a new heap var to point to indirection nodes?
	// And, what would this mean, anyway?

	// Hmm... should this happen at all?  How?

	int V = *push_temp_head(owners);
	new_heap_vars.push_back( V );
	local_env.push_back( V );
      }
      
      set_C(R, get_trimmed({T, local_env}));

      // Substitute the new heap vars for the dummy vars in expression T and in the bodies
      for(int i=0;i<bodies.size();i++) 
	set_C(new_heap_vars[i], get_trimmed({bodies[i],local_env}));

      assert(not access(R).changeable);

      // Remove the new heap vars from the list of temp heads in reverse order.
      for(int i=0;i<new_heap_vars.size(); i++)
	pop_temp_head(owners);
      
      assert(access(R).call == -1);
      assert(not access(R).result);
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      object_ptr<const Operation> O = is_a<Operation>( access(R).C.exp );
      assert(O);

      // Although the reg itself is not a parameter, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not access(R).changeable);" here.

#ifndef NDEBUG
      string SS = "";
      SS = compact_graph_expression(*this, R, get_identifiers_for_context(t))->print();
      string SSS = untranslate_vars(deindexify(trim_unnormalize(access(R).C)),  
				    get_identifiers_for_context(t))->print();
      if (log_verbose)
      {
	std::ofstream f("token.dot");
	dot_graph_for_token(*this, t, f);
	f.close();
      }
#endif

      RegOperationArgs Args(R, *this, t);
      closure result = (*O)(Args);

      // NOTE: While not all used_inputs are E-children, they SHOULD all be E-descendents.
      //       How could we assert that?

      // If the reduction doesn't depend on parameters, then replace E with the result.
      if (not access(R).changeable)
      {
	// The old used_input slots are not invalid, which is OK since none of them are changeable.
	assert(access(R).call == -1);
	assert(not access(R).result);
	clear_used_inputs(R);
	set_C(R, std::move(result) );
      }
      // Otherwise, set the reduction result.
      else
	set_reduction_result(R, std::move(result) );

#ifndef NDEBUG
      //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
      std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
    }
  }

  assert(access(R).result);
  assert(is_WHNF(access_result(R).exp));
  assert(not is_a<index_var>(access_result(R).exp));
  assert(not is_a<expression>(access_result(R).exp));

  return R;
}

// Fixme!
// Here we have handled neither depths, nor trim.
expression_ref subst_referenced_vars(const expression_ref& E, const vector<int>& Env, const map<int, expression_ref>& names)
{
  if (E->size())
  {
    bool different = false;
    object_ptr<expression> E2 ( new expression(E->head) );
    E2->sub.resize(E->size());
    for(int i=0;i<E->size();i++)
    {
      E2->sub[i] = subst_referenced_vars(E->sub[i], Env, names);
      if (E2->sub[i] != E->sub[i])
	different = true;
    }
    if (different)
      return object_ptr<const expression>(E2);
    else
      return E;
  }
  else if (object_ptr<const index_var> V = is_a<index_var>(E) )
  {
    const auto loc = names.find( lookup_in_env(Env, V->index) );
    if (loc == names.end())
      return E;
    else
    {
      //      assert(get_free_indices(loc->second).empty());
      return loc->second;
    }
  }
  // This case handles NULL in addition to atomic objects.
  else
    return E;
}

void discover_graph_vars(const reg_heap& H, int R, map<int,expression_ref>& names, const map<string, reg_heap::root_t>& id)
{
  const closure& C = H.access(R).C;

  // If there are no references, then we are done.
  if (C.Env.empty()) 
  {
    names[R] = C.exp;
    return;
  }

  // If R references R, then terminate the recursion.
  if (names.count(R))
  {
    if (not names[R])
      names[R] = C.exp;
    return;
  }

  // Add R to the hash in order to avoid infinite loops because of re-entering R
  names[R] = expression_ref();

  // find the names for each referenced var.
  for(int i: C.Env)
    discover_graph_vars(H, i, names, id);

  names[R] = subst_referenced_vars(C.exp, C.Env, names);
}

string escape(const string& s)
{
  string s2;
  s2.resize(s.size()*2);
  int l=0;
  for(int i=0;i<s.size();i++)
  {
    if (s[i] == '\n')
    {
      s2[l++] = '\\';
      s2[l++] = 'n';
      continue;
    }

    bool escape_next = (s[i] == '\\') or (s[i] == '\n') or (s[i] == '"');

    if (escape_next)
      s2[l++] = '\\';
    s2[l++] = s[i];
  }
  s2.resize(l);
  return s2;
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
      result += "\n";

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

expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids)
{
  if (not E->size())
  {
    if (object_ptr<const reg_var> RV = is_a<reg_var>(E))
    {
      auto loc = ids.find(RV->target);
      if (loc != ids.end())
	return var(loc->second);
      else
	return E;
    }
    else
      return E;
  }

  object_ptr<expression> V = E->clone();
  for(int i=0;i<E->size();i++)
    V->sub[i] = untranslate_vars(V->sub[i], ids);
  return V;
}

map<int,string> get_register_names(const map<string, reg_heap::root_t>& ids)
{
  map<int,string> ids2;
  for(const auto i:ids)
    ids2[*i.second] = i.first;
  return ids2;
}

expression_ref untranslate_vars(const expression_ref& E, const map<string, reg_heap::root_t>& ids)
{
  return untranslate_vars(E, get_register_names(ids));
}

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, reg_heap::root_t>& ids)
{
  return C[R].C.exp;

  map< int, expression_ref> names;
  for(const auto& id: ids)
  {
    int R = *(id.second);
    string name = id.first;
    names[R] = expression_ref(new var(name) );
  }
  discover_graph_vars(C, R, names, ids);

  return launchbury_unnormalize(names[R]);
}

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o)
{
  map<int,string> reg_names = get_register_names(C.get_identifiers_for_context(t));

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

    // node label = R/name: expression
    string label = convertToString(R);
    if (reg_names.count(R))
      label += "/" + reg_names[R];
    label += ": ";
    expression_ref E = untranslate_vars(deindexify(trim_unnormalize(C.access(R).C)), reg_names);

    label += E->print();
    label = escape(wrap(label,40));
    o<<"label = \""<<label<<"\"";
    if (C.access(R).changeable)
      o<<",style=\"dashed,filled\",color=red";

    if (C.access(R).result)
      o<<",fillcolor=\"#007700\",fontcolor=white";
    else if (C.access(R).changeable)
      o<<",fillcolor=\"#770000\",fontcolor=white";
    o<<"];\n";

    // out-edges
    for(int R2: C.access(R).C.Env)
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
    for(const auto& i: C.access(R).used_inputs)
    {
      int R2 = i.first;

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

/*
 * To evaluate a reg with a closure, we have to know
 * (a) how to save, modify and trim environments.
 * (b) how to report the final answer back to calling program with all the
 *     free variables removed, and also fully evaluated.
 *
 * Eval R -> (e,E)
 *
 * If e is a free variable x AND E[x]=p AND p->(e', E') then
 * - Execute it (This resolves it to either a lambda or a constructor, and updates it).
 * - Set our result to (e', E')
 * - Set the *call* to R2.
 * - 
 *
 * + Push an update marker to p onto the stack
 * + Set e=e', E=E', throwing away the current expression, and environment.
 *
 * If e is a lambda (\x->e') , then
 * - Do we only stop if there are no args on the stack?
 * - If there are args on the stack, then
 * + Take an arg (from stack to E top)
 * + e = e'
 *
 * - If we run out of args, then rebind [e,args+E]
 * 
 * If e is an application (x y) then
 * - Push y
 * - e = x
 * 
 * If e is a let expression, then
 * - 
 *
 * If e is a constructor, then 
 * - stop.
 *

f x y {} : | f -> [/\/\.(1+0)+3,{z}]
f x {} : y | f -> [/\/\.(1+0)+3,{z}]
f {} : x y | f -> [/\/\.(1+0)+3,{z}]

So, when we ENTER f, how do we know to UPDATE f?
			      - Hmm.. well, there were
			        some update marker things.

f {} : y x | f -> [/\/\.(1+0)+3,{z}]
/\/\.(1+0)+3 {z} : y x | f -> [/\/\.(1+0)+3,{z}]
/\/\.(1+0)+3 {z,y} : x | f -> [/\/\.(1+0)+3,{z}]
/\/\.(1+0)+3 {z,y,x} : | f -> [/\/\.(1+0)+3,{z}]

*/
