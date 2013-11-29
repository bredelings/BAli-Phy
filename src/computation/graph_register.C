#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include "program.H"
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
 * Goal: Share computation of WHNF structures between contexts, even when those
 *       stuctures are uncomputed at the time the contexts are split.
 *
 *       Rolling back to a previous context should not require recomputing anything
 *       that was previously known, and should take advantage of anything we computed
 *       for the next context that is also used by the old one.
 *
 * In order to share partially evaluated expressions between contexts, we need
 * these contexts to share a memory, since constructor expressions reference other
 * entries in the memory.
 *
 * Forward edges consist of
 * - E edges
 * - used edges (forward: used_inputs, backward: used_by)
 * - call edges (forward: call, backward: called_by)
 * - result edges (computed by following call edges).
 * The called_by back edges indicate that a result is being used by another result that calls us.
 * Thus called_by edges need not be set when setting a call, but only when setting the result.
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

    object_ptr<expression> E2 ( E->clone() );

    // Actually we probably just need x[i] not to be free in E->sub[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=0;i<E2->size();i++)
    {
      E2->sub[i] = graph_normalize(E->sub[i]);

      if (not is_reglike(E2->sub[i]))
      {
	expression_ref var = dummy( var_index++ );

	// 1. Let-bind the argument expression
       	vars.push_back( var );
	bodies.push_back( E->sub[i] );

	// 2. Replace the argument expression with the let var.
	E2->sub[i] = var;
      }
    }

    return let_expression(vars, bodies, object_ptr<const expression>(E2));
  }

  // 5. Let 
  if (object_ptr<const let_obj> Let = is_a<let_obj>(E))
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


// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.

void computation::clear()
{
  source = -1;
  result = 0;
  call = 0;
  used_inputs.clear();
  used_by.clear();
  called_by.clear();

  // This should already be cleared.
  assert(temp == -1);
}

void computation::check_cleared()
{
  assert(not call);
  assert(used_inputs.empty());
  assert(called_by.empty());
}

computation& computation::operator=(computation&& R) noexcept
{
  result = R.result;
  source = R.source;
  call = R.call;
  used_inputs  = std::move( R.used_inputs );
  used_by = std::move( R.used_by );
  called_by = std::move( R.called_by );
  temp = R.temp;

  return *this;
}

computation::computation(computation&& R) noexcept
:source(R.source),
  result (R.result), 
  call ( R.call ),
  used_inputs ( std::move(R.used_inputs) ),
  used_by ( std::move( R.used_by) ),
  called_by ( std::move( R.called_by) ),
  temp ( R.temp )
{ }

reg& reg::operator=(reg&& R) noexcept
{
  C = std::move(R.C);

  changeable = R.changeable;

  re_evaluate = R.re_evaluate;

  type = R.type;

  return *this;
}

reg::reg(reg&& R) noexcept
:C( std::move(R.C) ),
  changeable( R.changeable ),
  re_evaluate( R.re_evaluate ),
  type ( R.type )
{ }

void reg::clear()
{
  C.clear();
  changeable = false;
  re_evaluate = false;
  type = type_t::unknown;
}

void reg::check_cleared()
{
  assert(not C);
  assert(not changeable);
  assert(not re_evaluate);
  assert(type == type_t::unknown);
}

bool mapping::has_value(int r) const {return operator[](r);}

void mapping::add_value(int r, int v) 
{
  assert(not has_value(r));
  assert(v);

  address A(v);
  A.index = modified_.size();

  values[r] = A;
  modified_.push_back(r);

  assert(modified_[values[r].index] == r);
}

int mapping::erase_value(int r)
{
  int v = values[r].value;

  // The reg should be mapped.
  assert(v);

  // Check correspondence between modified_ and values
  assert(modified_[values[r].index] == r);

  // Lookup the position in modified_ where we mention r
  int index = values[r].index;

  int r2 = modified_.back();
  modified_.pop_back();

  // If we are deleting from the middle, move the last element to the middle
  if (index < modified_.size())
  {
    modified_[index] = r2;
    values[r2].index = index;
    assert(modified_[values[r2].index] == r2);
  }

  values[r] = {};
  return v;
}

int mapping::replace_value(int r, int v)
{
  assert(values[r].value);
  assert(v);
  int v_old = values[r].value;
  values[r].value = v;
  return v_old;
}

int mapping::set_value(int r, int v)
{
  if (v)
  {
    if (has_value(r))
      return replace_value(r,v);
    else
    {
      add_value(r,v);
      return 0;
    }
  }
  else if (has_value(r))
    return erase_value(r);
  else
  {
    return 0;
  }
}

void mapping::clear()
{
  for(int r: modified_)
    values[r] = {};
  modified_.clear();
}

void mapping::resize(int s)
{
  values.resize(s);
}

int mapping::size() const
{
  return values.size();
}

bool mapping::empty() const
{
  return modified_.empty();
}

const std::vector<int>& reg_heap::triggers(int t) const {assert(is_root_token(t));return token_roots[t].triggers;}
      std::vector<int>& reg_heap::triggers(int t)       {assert(is_root_token(t));return token_roots[t].triggers;}

int reg_heap::computation_index_for_reg(int t, int r) const 
{
  return find_computation_for_reg(t,r);
}

const computation& reg_heap::computation_for_reg(int t, int r) const 
{ 
  int rc = computation_index_for_reg(t,r);
  return computations.access_unused(rc);
}

computation& reg_heap::computation_for_reg(int t, int r)
{ 
  int rc = computation_index_for_reg(t,r);
  return computations.access_unused(rc);
}

int reg_heap::abs_computation_index_for_reg(int t, int r) const 
{
  return find_computation_for_reg(t,r);
}

const computation& reg_heap::abs_computation_for_reg(int t, int r) const 
{ 
  int rc = computation_index_for_reg(t,r);
  return computations.access_unused(rc);
}

computation& reg_heap::abs_computation_for_reg(int t, int r)
{ 
  int rc = computation_index_for_reg(t,r);
  return computations.access_unused(rc);
}

const closure& reg_heap::access_result_for_reg(int t, int R1) const
{
  assert(not t or is_root_token(t));
  int R2 = result_for_reg(t,R1);
  assert(R2);
  return access(R2).C;
}

bool reg_heap::reg_has_result(int t, int r) const
{
  if (access(r).type == reg::type_t::constant)
    return true;
  else
    return reg_has_computation_result(t,r);
}

bool reg_heap::reg_has_computation_result(int t, int r) const
{
  return has_computation(t,r) and computation_result_for_reg(t,r);
}

bool reg_heap::reg_has_call(int t, int r) const
{
  return has_computation(t,r) and call_for_reg(t,r);
}

int reg_heap::call_for_reg(int t, int r) const
{
  return computation_for_reg(t,r).call;
}

bool reg_heap::reg_has_result_(int t, int r) const
{
  if (access(r).type == reg::type_t::constant)
    return true;
  else
    return reg_has_computation_result_(t,r);
}

bool reg_heap::reg_has_computation_result_(int t, int r) const
{
  return has_computation_(t,r) and computation_result_for_reg_(t,r);
}

bool reg_heap::reg_has_call_(int t, int r) const
{
  return has_computation_(t,r) and call_for_reg_(t,r);
}

int reg_heap::call_for_reg_(int t, int r) const
{
  return computation_for_reg_(t,r).call;
}

vector<pool<computation>::weak_ref>& clean_weak_refs(vector<pool<computation>::weak_ref>& v, const pool<computation>& P)
{
  for(int i=0; i < v.size();)
  {
    int rc = v[i].get(P);
    if (rc and not P.is_marked(rc))
    {
      P.set_mark(rc);
      i++;
    }
    else
    {
      auto wr = v.back();
      v.pop_back();
      if (i < v.size())
	v[i] = wr;
    }
  }

  for(int i=0; i < v.size();i++)
  {
    int rc = v[i].get(P);
    P.unmark(rc);
  }
  return v;
}

bool reg_heap::has_computation(int t, int r) const
{
  return computation_index_for_reg(t,r);
}

bool reg_heap::has_computation_(int t, int r) const
{
  return computation_index_for_reg_(t,r)>0;
}

int reg_heap::find_computation_for_reg(int t, int r) const
{
  if (not t)
    return token_roots[t].vm_relative[r];

  int rc = 0;

  while(true)
  {
    assert(token_is_used(t));
    rc = token_roots[t].vm_relative[r];
    if (rc < 0)
    {
      rc = 0;
      break;
    }
    else if (rc or t == root_token) break;
    t = parent_token(t);
    assert(t != -1);
  }

  return rc;
}

const computation& reg_heap::computation_for_reg_(int t, int r) const 
{ 
  int rc = computation_index_for_reg_(t,r);
  return computations.access_unused(rc);
}

computation& reg_heap::computation_for_reg_(int t, int r)
{ 
  int rc = computation_index_for_reg_(t,r);
  return computations.access_unused(rc);
}

int reg_heap::computation_index_for_reg_(int t, int r) const 
{
  return token_roots[t].vm_relative[r];
}

int reg_heap::result_for_reg(int t, int r) const 
{
  assert(not is_index_var(access(r).C.exp));
  if (access(r).type == reg::type_t::constant)
    return r;
  else
    return computation_result_for_reg(t,r);
}

int reg_heap::computation_result_for_reg(int t, int r) const 
{
  return computation_for_reg(t,r).result;
}

int reg_heap::computation_result_for_reg_(int t, int r) const 
{
  return computation_for_reg_(t,r).result;
}

void reg_heap::set_computation_result_for_reg(int t, int r1)
{
  assert(not t or is_root_token(t));

  int call = call_for_reg(t,r1);

  assert(call);

  int result = result_for_reg(t,call);

  assert(result);

  computation_for_reg(t,r1).result = result;

  // If R2 is WHNF then we are done
  if (access(call).type == reg::type_t::constant) return;

  // If R2 doesn't have a computation, add one to hold the called-by edge.
  assert(has_computation(t,call));

  int rc1 = computation_index_for_reg(t,r1);

  // Add a called-by edge to R2.
  computation_for_reg(t,call).called_by.push_back(computations.get_weak_ref(rc1));
}

void reg_heap::set_used_input(int t, int R1, int R2)
{
  assert(reg_is_changeable(R1));
  assert(reg_is_changeable(R2));

  assert(is_used(R1));
  assert(is_used(R2));

  assert(access(R1).C);
  assert(access(R2).C);

  assert(has_computation(t,R1));
  assert(has_computation(t,R2));
  assert(computation_result_for_reg(t,R2));

  // An index_var's result only changes if the thing the index-var points to also changes.
  // So, we may as well forbid using an index_var as an input.
  assert(access(R2).C.exp->head->type() != index_var_type);

  int rc1 = computation_index_for_reg(t,R1);
  int rc2 = computation_index_for_reg(t,R2);

  computations[rc1].used_inputs.push_back(rc2);
  computations[rc2].used_by.push_back(computations.get_weak_ref(rc1));

  assert(computation_is_used_by(rc1,rc2));
  assert(reg_is_used_by(t,R1,R2));
}

int count(const std::vector<int>& v, int I)
{
  int c = 0;
  for(int i: v)
    if (i == I)
      c++;
  return c;
}


void reg_heap::clear_used_inputs(int rc1)
{
  computations.access_unused(rc1).used_inputs.clear();
}

void reg_heap::clear_used_inputs_for_reg(int t, int R)
{
  int rc = computation_index_for_reg(t,R);
  if (rc > 0)
    clear_used_inputs(rc);
}

void reg_heap::set_call(int t, int R1, int R2)
{
  assert(reg_is_changeable(R1));
  // R2 might be of UNKNOWN changeableness

  // Check that R1 is legal
  assert(is_used(R1));

  // Check that R2 is legal
  assert(is_used(R2));

  // Only modify the call for the current context;
  assert(has_computation_(t,R1));

  // Don't override an *existing* call
  assert(not reg_has_call_(t,R1));

  // Check that we aren't overriding an existing *result*
  assert(not reg_has_result_(t,R1));

  // Set the call
  int rc1 = computation_index_for_reg_(t,R1);
  computations[rc1].call = R2;
}

void reg_heap::clear_call(int rc)
{
  computations.access_unused(rc).call = 0;
}

void reg_heap::clear_call_for_reg(int t, int R)
{
  int rc = computation_index_for_reg(t,R);
  if (rc > 0)
    clear_call( rc );
}

void reg_heap::set_C(int R, closure&& C)
{
  assert(C);
  assert(not is_a<expression>(C.exp));
  clear_C(R);

  access(R).C = std::move(C);
#ifndef NDEBUG
  for(int r: access(R).C.Env)
    assert(is_valid_address(r));
#endif
}

void reg_heap::clear_C(int R)
{
  access_unused(R).C.clear();
}

void reg_heap::set_reduction_result(int t, int R, closure&& result)
{
  assert( has_computation_(t,R) );

  // Check that there is no result we are overriding
  assert(not reg_has_result_(t,R) );

  // Check that there is no previous call we are overriding.
  assert(not reg_has_call_(t,R) );

  // if the result is NULL, just leave the result and call both unset.
  //  (this could happen if we set a parameter value to null.)
  if (not result) return;

  // If the value is a pre-existing reg_var, then call it.
  if (result.exp->head->type() == index_var_type)
  {
    int index = convert<const index_var>(result.exp->head)->index;

    int Q = result.lookup_in_env( index );
    
    assert(is_used(Q));
    
    set_call(t,R,Q);
  }
  // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the result and call it.
  else
  {
    int R2 = allocate();

    set_C(R2, std::move( result ) );
    set_call(t, R, R2);
  }
}

/// Update the value of a non-constant, non-computed index
void reg_heap::set_reg_value(int P, closure&& C, int token)
{
  assert(not is_dirty(token));
  assert(not children_of_token(token).size());
  assert(reg_is_changeable(P));

  if (not is_root_token(token) and token_roots[token].version == token_roots[parent_token(token)].version)
    token_roots[token].version--;

  // assert(not is_root_token and token_roots[token].version < token_roots[parent_token(token)].version) 

  // Check that this reg is indeed settable
  assert(is_modifiable(access(P).C.exp));

  const int mark_result = 1;
  const int mark_call_result = 2;
  const int mark_modified = 3;

  vector< int >& call_and_result_may_be_changed = get_scratch_list();
  vector< int >& result_may_be_changed = get_scratch_list();
  vector< int >& regs_to_re_evaluate = token_roots[token].regs_to_re_evaluate;

  // If we have a RELATIVE computation, we need to take care of its users new to this token.
  if (computation_index_for_reg_(token,P))
  {
    call_and_result_may_be_changed.push_back(P);
    computation_for_reg(token,P).temp = mark_modified;
    result_may_be_changed.push_back(P);
  }

  int i=0;
  int j=0;
  while(i < call_and_result_may_be_changed.size() or j < result_may_be_changed.size())
  {
    // First find all users or callers of regs where the result is out of date.
    find_callers_(token, token, j, result_may_be_changed, result_may_be_changed, mark_result);
    find_users_(token, token, j, result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    j = result_may_be_changed.size();

    // Second find all users or callers of regs where the result AND CALL are out of date.
    find_users_(token, token, i, call_and_result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    find_callers_(token, token, i, call_and_result_may_be_changed, result_may_be_changed, mark_result);
    i = call_and_result_may_be_changed.size();
  }

#ifndef NDEBUG
  for(int R: result_may_be_changed)
    assert(computation_for_reg(token,R).temp == mark_result or 
	   computation_for_reg(token,R).temp == mark_call_result or
	   computation_for_reg(token,R).temp == mark_modified
	   );

  for(int R: call_and_result_may_be_changed)
    assert(computation_for_reg(token,R).temp == mark_call_result or
	   computation_for_reg(token,R).temp == mark_modified
	   );
#endif

  //  std::cerr<<" result: "<<result_may_be_changed.size()<<"\n";

  // Clear the marks: 1a
  for(int R: result_may_be_changed)
  {
    assert(has_computation(token,R));

    //    assert(computation_result_for_reg(token,R) or reg_is_shared(token,R));

    auto& RC = computation_for_reg(token,R);

    if (RC.temp > mark_result) continue;

    assert(reg_has_call(token,R));

    RC.temp = -1;

    share_and_clear_result(token, R);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(R).re_evaluate)
      regs_to_re_evaluate.push_back(R);
  }

  // Clear the marks: 2a
  for(int R: call_and_result_may_be_changed)
  {
    assert(has_computation(token,R));

    // Put this back when we stop making spurious used_by edges
    //    assert(reg_has_call(token,R));

    auto& RC = computation_for_reg(token,R);

    if (RC.temp > mark_call_result) continue;

    assert(RC.temp == mark_call_result);

    RC.temp = -1;

    assert(not is_modifiable(access(R).C.exp));

    share_and_clear(token,R);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(R).re_evaluate)
      regs_to_re_evaluate.push_back(R);
  }

  if (has_computation_(token,P))
    computation_for_reg_(token,P).temp = -1;

  // Finally set the new value.
  token_roots[token].vm_relative.set_value(P,-1);
  add_shared_computation(token,P);
  assert(has_computation_(token,P));
  set_reduction_result(token, P, std::move(C) );

  release_scratch_list();
  release_scratch_list();
  assert(n_active_scratch_lists == 0);

  if (token == root_token)
  {
    if (regs_to_re_evaluate.size())
      mark_completely_dirty(token);
    for(int R: regs_to_re_evaluate)
      incremental_evaluate(R,token);
    regs_to_re_evaluate.clear();
  }
}

void swap_value(mapping& vm1, mapping& vm2, int r)
{
  int v1 = vm1[r];
  int v2 = vm2[r];
  vm1.replace_value(r,v2);
  vm2.replace_value(r,v1);
}

// Given mapping (m1,v1) followed by (m2,v2), compute a combined mapping for (m1,v1)+(m2,v2) -> (m2,v2)
// and a mapping (m1,v1)-(m2,v2)->(m1,v1) for things that now are unused.
void merge_split_mapping(mapping& vm1, mapping& vm2)
{
  if (vm1.modified().size() < vm2.modified().size())
  {
    for(int i=0;i<vm1.modified().size();)
    {
      int r = vm1.modified()[i];
      assert(vm1[r]);
      if (not vm2[r])
      {
	// transfer mapping from v1[r] -> v2[r]
	int rc = vm1.erase_value(r);
	vm2.add_value(r,rc);
      }
      else
	i++;
    }
  }
  else
  {
    for(int i=0;i<vm2.modified().size();)
    {
      int r = vm2.modified()[i];
      assert(vm2[r]);

      if (vm1[r])
      {
	swap_value(vm1, vm2, r);
	i++;
      }
      else
      {
	vm1.add_value(r,vm2[r]);
	vm2.erase_value(r);
      }
    }
    std::swap(vm1,vm2);
  }
}

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
void pivot_mapping(mapping& vm1, mapping& vm2)
{
  for(int i=0;i<vm2.modified().size();i++)
  {
    int r = vm2.modified()[i];
    assert(vm2[r]);

    int rc1 = vm1[r];
    int rc2 = vm2[r];

    // switch from root/0 => root/-
    if (rc1 == 0) rc1 = -1;

    // switch root positions
    std::swap(rc1,rc2);

    // switch from root/0 => root/-
    if (rc1 == -1) rc1 = 0;

    vm1.set_value(r,rc1);
    vm2.set_value(r,rc2);
  }
  std::swap(vm1,vm2);
}


void reg_heap::reroot_at(int t)
{
  if (is_root_token(t)) return;

  check_used_regs();

  int parent = parent_token(t);

  // 1. If this context isn't a direct child of the root, then make it one
  if (not is_root_token(parent))
    reroot_at(parent);

  // re-rooting to the parent context shouldn't release its token.
  assert(token_is_used(parent));
  // Now this context should be a direct child of the root
  assert(is_root_token(parent));

  // 2. Now invalidate regs in t that reference computations from parent
  invalidate_shared_regs(parent,t);

  assert(token_roots[t].version >= token_roots[parent].version);

  // 3. Now actually reroot.

  pivot_mapping(token_roots[parent].vm_relative, token_roots[t].vm_relative);

  token_roots[parent].parent = t;
  int index = remove_element(token_roots[parent].children, t);
  assert(index != -1);

  token_roots[t].parent = -1;
  token_roots[t].children.push_back(parent);
  root_token = t;

  for(int t2: token_roots[t].children)
    assert(token_roots[t2].version <= token_roots[t].version);

  assert(is_root_token(t));

  // re-evaluate all the regs that need to be up-to-date.
  if (token_roots[t].regs_to_re_evaluate.size())
    mark_completely_dirty(t);
  for(int R: token_roots[t].regs_to_re_evaluate)
    incremental_evaluate(R,t);
  token_roots[t].regs_to_re_evaluate.clear();

  // 4. Now, try to remove the parent if its unreferenced.
  try_release_token(parent);
}

void reg_heap::mark_completely_dirty(int t)
{
  int& version = token_roots[t].version;
  for(int t2:token_roots[t].children)
    version = std::max(version, token_roots[t2].version+1);
}

bool reg_heap::is_dirty(int t) const
{
  for(int t2:token_roots[t].children)
    if (token_roots[t].version > token_roots[t2].version)
      return true;
  return false;
}

// Note that a context can be completely dirty, w/o being dirty :-P
bool reg_heap::is_completely_dirty(int t) const
{
  for(int t2:token_roots[t].children)
    if (token_roots[t].version <= token_roots[t2].version)
      return false;
  return true;
}
  
// find regs in t2 that call values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_callers_(int t1, int t2, int start, const vector<int>& split, vector<int>& callers, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg_(t1,split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.called_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg_(t2,r2) != rc2) continue;

      // Skip this one if its been marked high enough already
      if (RC2.temp >= mark) continue;

      // There (usually) shouldn't be a back edge to r2 if r2 has no result.
      assert(RC2.result);

      RC2.temp = mark;
      assert(computation_index_for_reg_(t2,r2) == rc2);
      callers.push_back(r2);
    }
  }
}

// find regs in t2 that used values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_users_(int t1, int t2, int start, const vector<int>& split, vector<int>& users, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg_(t1, split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.used_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg_(t2,r2) != rc2) continue;

      // Skip this one if its been marked high enough already
      if (RC2.temp >= mark) continue;

      // There (usually) shouldn't be a back edge to r2 if r2 has no result.
      //      assert(RC2.result);

      RC2.temp = mark;
      assert(computation_index_for_reg_(t2,r2) == rc2);
      users.push_back(r2);
    }
  }
}

// find regs in t2 that call values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_callers(int t1, int t2, int start, const vector<int>& split, vector<int>& callers, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg(t1,split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.called_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg(t2,r2) != rc2) continue;

      // Skip this one if its been marked high enough already
      if (RC2.temp >= mark) continue;

      // There (usually) shouldn't be a back edge to r2 if r2 has no result.
      assert(RC2.result);

      RC2.temp = mark;
      assert(computation_index_for_reg(t2,r2) == rc2);
      callers.push_back(r2);
    }
  }
}

// find regs in t2 that used values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_users(int t1, int t2, int start, const vector<int>& split, vector<int>& users, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg(t1, split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.used_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg(t2,r2) != rc2) continue;

      // Skip this one if its been marked high enough already
      if (RC2.temp >= mark) continue;

      // There (usually) shouldn't be a back edge to r2 if r2 has no result.
      //      assert(RC2.result);

      RC2.temp = mark;
      assert(computation_index_for_reg(t2,r2) == rc2);
      users.push_back(r2);
    }
  }
}

void reg_heap::invalidate_shared_regs_(int t1, int t2)
{
  assert(t1 == parent_token(t2));

  if (token_roots[t1].version <= token_roots[t2].version) return;

  const int mark_result = 1;
  const int mark_call_result = 2;

  // find all regs in t2 that are not shared from t1
  vector<int> modified;
  for(int r: token_roots[t2].vm_relative.modified())
    if (token_roots[t1].vm_relative[r] > 0)
      modified.push_back(r);

  vector< int >& call_and_result_may_be_changed = get_scratch_list();
  vector< int >& result_may_be_changed = get_scratch_list();
  vector< int >& regs_to_re_evaluate = token_roots[t2].regs_to_re_evaluate;

  find_callers(t1, t2, 0, modified, result_may_be_changed, mark_result);
  find_users(t1, t2, 0, modified, call_and_result_may_be_changed, mark_call_result);

  int i=0;
  int j=0;
  while(i < call_and_result_may_be_changed.size() or j < result_may_be_changed.size())
  {
    // First find all users or callers of regs where the result is out of date.
    find_callers(t2, t2, j, result_may_be_changed, result_may_be_changed, mark_result);
    find_users(t2, t2, j, result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    j = result_may_be_changed.size();

    // Second find all users or callers of regs where the result AND CALL are out of date.
    find_users(t2, t2, i, call_and_result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    find_callers(t2, t2, i, call_and_result_may_be_changed, result_may_be_changed, mark_result);
    i = call_and_result_may_be_changed.size();
  }

  for(int r:result_may_be_changed)
  {
    auto& RC = computation_for_reg(t2,r);

    if (RC.temp > mark_result) continue;

    RC.temp = -1;

    share_and_clear_result(t2,r);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }

  for(int r:call_and_result_may_be_changed)
  {
    auto& RC = computation_for_reg(t2,r);

    if (RC.temp > mark_call_result) continue;

    RC.temp = -1;

    share_and_clear(t2,r);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }

  // find all regs in t2 that are not shared from t1.  Nothing needs to be done to these - they are already split.
  // Anything that uses these needs to be unshared.
  //  - The local version should be completely cleared.  We can remove the computation.
  //  - All children should have their computations removed also
  // Anything that calls these needs to be unshared.
  //  - The local version should preserve its uses and call, but its result should be cleared.
  //  - All children should be updated to use the new computation.

  // This is similar to set_reg_value, but not the same.
  // Should set_reg_value be able to consist of
  // (a) change the computation for some modifiables
  // (b) run invalidate_shared_regs?

  release_scratch_list();
  release_scratch_list();
  assert(n_active_scratch_lists == 0);

  // Mark this context as not having computations that need to be unshared
  assert(token_roots[t1].version >= token_roots[t2].version);

  // We can only do this if we know that t1 doesn't have an ancestor with the same version, I think.
  if (is_root_token(t1))
    token_roots[t2].version = token_roots[t1].version;
}

void reg_heap::invalidate_shared_regs(int t1, int t2)
{
  assert(t1 == parent_token(t2));

  if (token_roots[t1].version <= token_roots[t2].version) return;

  const int mark_result = 1;
  const int mark_call_result = 2;

  // find all regs in t2 that are not shared from t1
  vector<int> modified;
  for(int r: token_roots[t2].vm_relative.modified())
    if (token_roots[t1].vm_relative[r] > 0)
      modified.push_back(r);

  vector< int >& call_and_result_may_be_changed = get_scratch_list();
  vector< int >& result_may_be_changed = get_scratch_list();
  vector< int >& regs_to_re_evaluate = token_roots[t2].regs_to_re_evaluate;

  find_callers(t1, t2, 0, modified, result_may_be_changed, mark_result);
  find_users(t1, t2, 0, modified, call_and_result_may_be_changed, mark_call_result);

  int i=0;
  int j=0;
  while(i < call_and_result_may_be_changed.size() or j < result_may_be_changed.size())
  {
    // First find all users or callers of regs where the result is out of date.
    find_callers(t2, t2, j, result_may_be_changed, result_may_be_changed, mark_result);
    find_users(t2, t2, j, result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    j = result_may_be_changed.size();

    // Second find all users or callers of regs where the result AND CALL are out of date.
    find_users(t2, t2, i, call_and_result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    find_callers(t2, t2, i, call_and_result_may_be_changed, result_may_be_changed, mark_result);
    i = call_and_result_may_be_changed.size();
  }

  for(int r:result_may_be_changed)
  {
    auto& RC = computation_for_reg(t2,r);

    if (RC.temp > mark_result) continue;

    RC.temp = -1;

    share_and_clear_result(t2,r);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }

  for(int r:call_and_result_may_be_changed)
  {
    auto& RC = computation_for_reg(t2,r);

    if (RC.temp > mark_call_result) continue;

    RC.temp = -1;

    share_and_clear(t2,r);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }

  // find all regs in t2 that are not shared from t1.  Nothing needs to be done to these - they are already split.
  // Anything that uses these needs to be unshared.
  //  - The local version should be completely cleared.  We can remove the computation.
  //  - All children should have their computations removed also
  // Anything that calls these needs to be unshared.
  //  - The local version should preserve its uses and call, but its result should be cleared.
  //  - All children should be updated to use the new computation.

  // This is similar to set_reg_value, but not the same.
  // Should set_reg_value be able to consist of
  // (a) change the computation for some modifiables
  // (b) run invalidate_shared_regs?

  release_scratch_list();
  release_scratch_list();
  assert(n_active_scratch_lists == 0);

  // Mark this context as not having computations that need to be unshared
  assert(token_roots[t1].version >= token_roots[t2].version);

  // We can only do this if we know that t1 doesn't have an ancestor with the same version, I think.
  if (is_root_token(t1))
    token_roots[t2].version = token_roots[t1].version;
}

bool reg_heap::reg_is_shared_with_parent(int t, int r) const
{
  assert(t);
  if (is_root_token(t)) return false;
  int p = parent_token(t);
  return computation_index_for_reg(t,r) == computation_index_for_reg(p,r);
}

std::vector<int> reg_heap::used_regs_for_reg(int t, int r) const
{
  vector<int> U;
  if (not has_computation(t,r)) return U;

  for(int rc: computation_for_reg(t,r).used_inputs)
    U.push_back(computations[rc].source);

  return U;
}

void reg_heap::reclaim_used(int r)
{
  // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
  remove_from_used_list(r);

  for(int t=0;t<token_roots.size();t++)
  {
    if (not token_is_used(t)) continue;

    if (token_roots[t].vm_relative[r])
      token_roots[t].vm_relative.erase_value(r);
  }

  for(int t=0;t<token_roots.size();t++)
  {
    assert(not token_roots[t].vm_relative[r]);
  }

  clear_C(r);

  add_to_free_list(r);
}

template <typename T>
void insert_at_end(vector<int>& v, const T& t)
{
  v.insert(v.end(), t.begin(), t.end());
}

void reg_heap::get_roots(vector<int>& scan) const
{
  insert_at_end(scan, temp);
  insert_at_end(scan, heads);
  for(int j=0;j<parameters.size();j++)
    scan.push_back(parameters[j].second);
  for(const auto& i: identifiers)
    scan.push_back(i.second);
}

int reg_heap::push_temp_head()
{
  int R = allocate();

  temp.push_back(R);

  return R;
}

void reg_heap::pop_temp_head()
{
  temp.pop_back();
}

void reg_heap::get_more_memory()
{
  collect_garbage();
  base_pool_t::get_more_memory();
}

void reg_heap::expand_memory(int s)
{
  int old_size = size();
  for(int t=0;t<token_roots.size();t++)
    assert(token_roots[t].vm_relative.size() == old_size);

  base_pool_t::expand_memory(s);

  // Extend virtual mappings, with virtual_mapping[i] = 0;
  for(int t=0;t<token_roots.size();t++)
  {
    token_roots[t].vm_relative.resize(size());
    for(int i=old_size;i<size();i++)
      assert(token_roots[t].vm_relative[i] == 0);
  }
}

void reg_heap::trace_and_reclaim_unreachable()
{
  check_used_regs();

  vector<int>& tokens = get_scratch_list();

  vector<int>& scan1 = get_scratch_list();
  vector<int>& next_scan1 = get_scratch_list();
  vector<int>& scan2 = get_scratch_list();
  vector<int>& next_scan2 = get_scratch_list();

  //  assert(root_token != -1);
  tokens.push_back(root_token);

  get_roots(scan1);
  
  while (not scan1.empty() or not scan2.empty())
  {
    for(int r: scan1)
    {
      assert(not is_free(r));
      if (is_marked(r)) continue;
      
      set_mark(r);
      
      reg& R = access(r);
      
      // Count the references from E
      next_scan1.insert(next_scan1.end(), R.C.Env.begin(), R.C.Env.end());
      
      // Count all computations
      for(int t=0;t<get_n_tokens();t++)
      {
	if (not token_is_used(t)) continue;
	
	if (not has_computation(t,r)) continue;

	int rc = computation_index_for_reg(t,r);
	scan2.push_back(rc);
      }
    }
    std::swap(scan1,next_scan1);
    next_scan1.clear();

    for(int rc: scan2)
    {
      assert(not computations.is_free(rc));
      if (computations.is_marked(rc)) continue;
      
      computations.set_mark(rc);
      
      const computation& RC = computations[rc];
      
      // Count the reg that references us
      assert(RC.source);
      scan1.push_back(RC.source);
      
      // Count also the computation we call
      if (RC.call) 
	scan1.push_back(RC.call);
    }
    std::swap(scan2,next_scan2);
    next_scan2.clear();
  }

  check_used_regs();
  reclaim_unmarked();
  computations.reclaim_unmarked();
  check_used_regs();

  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
}

bool reg_heap::reg_is_changeable(int r) const
{
  return access(r).changeable;
}

/*
 * We would like to know if a reg E[i] ---t--> C[i] that is unreachable in t could
 * be reachable in a child context of t.
 *
 * Let's assume that new regs are only added into contexts where they are reachable
 * at the time.
 * 
 * Then
 * 1. If the reg was reachable when t was duplicated, then t will still be reachable in t.
 * 2. If the reg was unreachable when t was duplicated, then t will be unreach in 
      t & descendants.
 * 3. If the reg was 
 */

void reg_heap::collect_garbage()
{
  // Make sure weak references to anything freed here are invalidated.
  computations.inc_version();

#ifdef DEBUG_MACHINE
  std::cerr<<"***********Garbage Collection******************"<<std::endl;
  check_used_regs();
#endif
  assert(size() == n_used() + n_free() + n_null());

  trace_and_reclaim_unreachable();

#ifdef DEBUG_MACHINE
  cerr<<"Regs: "<<n_used()<<"/"<<size()<<endl;
  check_used_regs();
#endif
}

int reg_heap::get_unused_token()
{
  if (unused_tokens.empty())
  {
    unused_tokens.push_back(get_n_tokens());
    token_roots.push_back(graph_roots());
    token_roots.back().vm_relative.resize(size());
    for(int i=0;i<size();i++)
      assert(token_roots.back().vm_relative[i] == 0);
  }

  for(int i=0;i<token_roots.size();i++)
    assert(token_roots[i].vm_relative.size() == size());

  int t = unused_tokens.back();
  unused_tokens.pop_back();
  if (root_token == -1)
    root_token = t;

  assert(not token_is_used(t));

  token_roots[t].used = true;

  assert(token_roots[t].parent == -1);
  assert(token_roots[t].children.empty());
  assert(token_roots[t].vm_relative.empty());
  assert(not token_roots[t].referenced);

  token_roots[t].referenced = true;

  return t;
}

bool reg_heap::computation_is_called_by(int rc1, int rc2) const
{
  for(const auto& wr: computations[rc2].called_by)
    if (wr.get(computations) == rc1)
      return true;

  return false;
}

bool reg_heap::computation_is_used_by(int rc1, int rc2) const
{
  for(const auto& wr: computations[rc2].used_by)
    if (wr.get(computations) == rc1)
      return true;

  return false;
}

bool reg_heap::reg_is_used_by(int t, int r1, int r2) const
{
  int rc1 = computation_index_for_reg(t,r1);
  int rc2 = computation_index_for_reg(t,r2);

  return computation_is_used_by(rc1,rc2);
}

bool reg_heap::computation_is_referenced(int t,int rc) const
{
  assert(rc);
  int r = computations[rc].source;
  if (computation_index_for_reg_(t,r) == rc) return true;
  int p = parent_token(t);
  if (p != -1)
    return computation_is_referenced(p, rc);
  else
    return false;
}

void reg_heap::check_tokens() const
{
  for(int t=0;t<token_roots.size();t++)
    if (token_is_used(t))
    {
      assert(token_roots[t].referenced or token_roots[t].children.size() > 1);
      for(int t2: children_of_token(t))
	assert(token_roots[t].version >= token_roots[t2].version);
    }
}

void reg_heap::check_used_reg(int index) const
{
  for(int t=1;t<get_n_tokens();t++)
  {
    if (not token_is_used(t)) continue;

    if (not is_root_token(t) and token_roots[t].vm_relative[index] > 0 and token_roots[parent_token(t)].vm_relative[index] > 0)
      assert(token_roots[t].vm_relative[index] != token_roots[parent_token(t)].vm_relative[index]);

    if (access(index).type == reg::type_t::constant)
      assert(not has_computation_(t,index));

    if (not has_computation_(t, index)) continue;

    int call = call_for_reg_(t,index);
    int result = computation_result_for_reg_(t,index);

    if (result)
      assert(call);

    if (call and result == call)
      assert(access(call).type == reg::type_t::constant);

    if (call and result and access(call).type == reg::type_t::constant)
      assert(result == call);

    int index_c = computation_index_for_reg_(t,index);

    const computation& RC = computation_for_reg_(t,index);

    for(int rc2: RC.used_inputs)
    {
      // Used regs should have back-references to R
      assert( computation_is_used_by(index_c, rc2) );

      // Used computations should be mapped computation for the current token, if we are at the root
      int R2 = computations[rc2].source;
      assert(reg_is_changeable(R2));

      // The used computation should be referenced somewhere more root-ward
      // so that this computation can be invalidated, and the used computation won't be GC-ed.
      assert(is_modifiable(access(R2).C.exp) or computation_is_referenced(t,rc2));
      
      // Used computations should have results
      assert(computations[rc2].result);
    }

    if (t != root_token) continue;

    // Regs with results should have back-references from their call.
    if (result and access(call).type != reg::type_t::constant)
    {
      assert( has_computation(t,call) );
      int rc2 = computation_index_for_reg(t,call);
      assert( computation_is_called_by(index_c, rc2) );
    }

    // If we have a result, then our call should have a result
    if (result)
      assert(reg_has_result(t,call));
  }
}

void reg_heap::check_used_regs() const
{
  // check_used_regs
  for(auto r = begin(); r != end(); r++)
    check_used_reg( r.addr() );
}

int reg_heap::remove_shared_computation(int t, int r)
{
  if (not t or is_root_token(t))
    return token_roots[t].vm_relative.erase_value(r);
  else
    return token_roots[t].vm_relative.set_value(r,-1);
}

int reg_heap::new_computation_for_reg(int r) const
{
  int rc = computations.allocate();
  computations[rc].source = r;
  return rc;
}

int reg_heap::add_shared_computation(int t, int r)
{
  assert(token_roots[t].vm_relative[r] <= 0);

  int rc = new_computation_for_reg(r);

  token_roots[t].vm_relative.set_value(r, rc);

  return rc;
}

int reg_heap::share_and_clear(int t, int r)
{
  assert(t);
  int rc1 = computation_index_for_reg(t,r);
  assert(rc1);

  if (is_root_token(t))
  {
    assert(token_roots[t].vm_relative[r] == rc1);
    token_roots[t].vm_relative.erase_value(t);
  }
  else
  {
    if (token_roots[t].vm_relative[r] > 0)
      assert(token_roots[t].vm_relative[r] == rc1);

    token_roots[t].vm_relative.set_value(r,-1);
  }

  return rc1;
}

int reg_heap::replace_shared_computation(int t, int r)
{
  assert(t);
  int rc1 = computation_index_for_reg(t,r);
  assert(rc1);

  int rc2 = new_computation_for_reg(r);

  token_roots[t].vm_relative.set_value(r, rc2);
  
  return rc1;
}

int reg_heap::share_and_clear_result(int t, int r)
{
  assert(t);
  int rcA = replace_shared_computation(t,r);

  if (computations[rcA].call) 
    set_call(t,r,computations[rcA].call);
  for(int rc: computations[rcA].used_inputs)
  {
    int r2 = computations[rc].source;
    set_used_input(t,r,r2);
  }
  return rcA;
}

vector<int> reg_heap::find_all_regs_in_context_no_check(int t) const
{
  vector<int> unique;
  find_all_regs_in_context_no_check(t, unique);
  return unique;
}

vector<int> reg_heap::find_all_regs_in_context(int t) const
{
  vector<int> unique;
  find_all_regs_in_context(t, unique);
  return unique;
}

vector<int> reg_heap::find_all_used_regs_in_context(int t) const
{
  vector<int> unique;
  find_all_used_regs_in_context(t, unique);
  return unique;
}

void reg_heap::find_all_regs_in_context_no_check(int t, vector<int>& unique) const
{
  vector<int>& scan = get_scratch_list();

  get_roots(scan);

  find_all_regs_in_context_no_check(t,scan,unique);
}

void reg_heap::find_all_used_regs_in_context(int t, vector<int>& unique) const
{
  vector<int>& scan = get_scratch_list();

  get_roots(scan);

  find_all_regs_in_context_no_check(t,scan,unique);

#ifndef NDEBUG
  for(int R: unique)
    check_used_reg(R);
#endif
}

void reg_heap::find_all_regs_in_context_no_check(int t, vector<int>& scan, vector<int>& unique) const
{
  for(int i=0;i<scan.size();i++)
  {
    int r = scan[i];
    assert(is_used(r) or is_marked(r));
    if (is_marked(r)) continue;

    set_mark(r);
    unique.push_back(scan[i]);
  }

  for(int i=0;i<unique.size();i++)
  {
    int r = unique[i];
    assert(is_marked(r));

    const reg& R = access(r);
    for(int j:R.C.Env)
    {
      if (is_used(j) and not is_marked(j))
      {
	set_mark(j);
	unique.push_back(j);
      }
    }

    if (not has_computation(t,r)) continue;

    // Count also the references from the call
    if (reg_has_call(t,r))
    {
      int called_reg = call_for_reg(t,r);
      if (not is_marked(called_reg))
      {
	set_mark(called_reg);
	unique.push_back(called_reg);
      }
    }
  }

#ifndef NDEBUG
  for(int i=0;i<unique.size();i++)
    for(int j=0;j<i;j++)
      assert(unique[i] != unique[j]);
#endif

  for(int i=0;i<unique.size();i++)
    unmark(unique[i]);

  release_scratch_list();
}

// This routine is separate from the *_no_check variant because the
// checks don't hold in all cases.
void reg_heap::find_all_regs_in_context(int t, vector<int>& unique) const
{
  find_all_regs_in_context_no_check(t, unique);

#ifdef DEBUG_MACHINE
  for(int R: unique)
  {
    assert(reg_is_owned_by(R,t));
    check_used_reg(R);
  }
#endif
}

/// Remove the roots for the identifiers of graph t
void reg_heap::release_identifiers()
{
  identifiers.clear();
}

void reg_heap::try_release_token(int t)
{
  assert(token_is_used(t));

  // We shouldn't have any temporary heads still on the stack, here!
  // (This should be fast now, no longer proportional to the number of regs in context t.)
  // (But how fast is it?)
  assert(temp.empty());

  int n_children = token_roots[t].children.size();
  if (n_children > 1 or token_roots[t].referenced)
    return;

  int child_token = -1;
  if (n_children)
  {
    child_token = token_roots[t].children[0];

    // handle the case when we are trying to release the root
    if (is_root_token(t))
    {
      reroot_at(child_token);
      return;
    }

    merge_split_mapping(token_roots[t].vm_relative, token_roots[child_token].vm_relative);

    invalidate_shared_regs(t, child_token);
  }

  // mark token for this context unused
  token_roots[t].used = false;
  token_roots[t].children.clear();  
  unused_tokens.push_back(t);

  int parent = parent_token(t);
  token_roots[t].parent = -1;
  if (t == root_token)
  {
    assert(not n_children);
    root_token = -1;
  }

  // Any context must be either referenced or have more than 1 child context.
  if (parent != -1)
    assert(token_roots[parent].referenced or token_roots[parent].children.size() > 1);

  if (n_children == 1)
  {
    assert(not t != root_token);

    // make parent point to child
    if (parent != -1)
    {
      int index = replace_element(token_roots[parent].children, t, child_token);
      assert(index != -1);
    }

    // make child point to parent
    token_roots[child_token].parent = parent;
  }
  else if (parent != -1)
  {
    int index = remove_element(token_roots[parent].children, t);
    assert(index != -1);
  }

  // clear only the mappings that were actually updated here.
  token_roots[t].vm_relative.clear();

  // If we just released a terminal token, maybe it's parent is not terminal also.
  if (parent != -1)
    try_release_token(parent);

  // The -1 accounts for the unused token 0.
  if (token_roots.size() - unused_tokens.size() -1 > 0)
    assert(root_token != -1);

#ifdef DEBUG_MACHINE
  assert(token_roots[t].vm_relative.size() == size());
  for(int i=0;i<size();i++)
    assert(not token_roots[t].vm_relative[i]);

  check_used_regs();
#endif
}

bool reg_heap::is_terminal_token(int t) const
{
  assert(token_is_used(t));

  return token_roots[t].children.empty();
}

bool reg_heap::is_root_token(int t) const
{
  assert(root_token != -1);
  assert((t==root_token) == (token_roots[t].parent == -1));
  assert(token_is_used(t));

  return t == root_token;
}

int reg_heap::parent_token(int t) const
{
  return token_roots[t].parent;
}

const vector<int>& reg_heap::children_of_token(int t) const
{
  return token_roots[t].children;
}

int reg_heap::degree_of_token(int t) const
{
  int degree = children_of_token(t).size();
  if (not is_root_token(t))
    degree++;
  return degree;
}
  

void reg_heap::release_token(int t)
{
  check_tokens();

  token_roots[t].referenced = false;
  try_release_token(t);

  check_tokens();
}

bool reg_heap::token_is_used(int t) const
{
  return token_roots[t].used;
}

int reg_heap::copy_token(int t)
{
  check_tokens();

  check_used_regs();

  int t2 = get_unused_token();

  assert(temp.empty());

  token_roots[t2].triggers = token_roots[t].triggers;

  // set parent relationship
  token_roots[t2].parent = t;
  token_roots[t2].children.clear();

  token_roots[t].children.push_back(t2);

  token_roots[t2].version = token_roots[t].version;

  /*
    Only true for root token!
  for(int r: token_roots[t].modified)
    if (access(r).re_evaluate)
      assert(reg_has_result(t2,r));
  */

  /*
  // use all the same computations and result.
  token_roots[t2].modified = token_roots[t].modified;
  token_roots[t2].virtual_mapping = token_roots[t].virtual_mapping;

  for(int r: token_roots[t].modified)
  {
    assert(has_computation(t,r));
    assert(has_computation(t2,r));
  }
  */
  check_used_regs();

  check_tokens();

  return t2;
}

int reg_heap::switch_to_child_token(int t)
{
  int new_t = copy_token(t);
  token_roots[t].referenced = false;
  return new_t;
}

int reg_heap::add_identifier(const string& name)
{
  map<string,int>& identifiers = get_identifiers();

  // if there's already an 's', then complain
  if (identifiers.count(name))
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already an identifier with that name.";

  int R = allocate();

  identifiers[name] = R;
  return R;
}

reg_heap::reg_heap()
  :base_pool_t(1),
   computations(1),
   token_roots(1)
{ 
  //  computations.collect_garbage = [this](){collect_garbage();};
  computations.collect_garbage = [](){};
  computations.clear_references = [](int){};
  token_roots[0].vm_relative.resize(1);
  token_roots[0].used = true;
  token_roots[0].referenced = true;
}

#include "computation.H"

/// These are LAZY operation args! They don't evaluate arguments until they are evaluated by the operation (and then only once).
class RegOperationArgs: public OperationArgs
{
  const int R;

  reg_heap& M;

  const int t;

  int n_allocated;

  bool evaluate_changeable;

  int current_token() const {return t;}

  reg_heap& memory() {return M;}

  const closure& current_closure() const {return M[R].C;}

  const expression& get_E() const {return *current_closure().exp;}

  bool evaluate_changeables() const {return evaluate_changeable;}

  /// Evaluate the reg R2, record dependencies, and return the reg following call chains.
  int evaluate_reg_no_record(int R2, bool ec)
  {
    return M.incremental_evaluate(R2, ec?t:0);
  }

  /// Evaluate the reg R2, record a dependency on R2, and return the reg following call chains.
  int evaluate_reg_to_reg(int R2, bool ec)
  {
    // Compute the result, and follow index_var chains (which are not changeable).
    int R3 = M.incremental_evaluate(R2, ec?t:0);

    if (M.reg_is_changeable(R3) and evaluate_changeables())
    {
      // If R2 -> result was changeable, then R -> result will be changeable as well.
      M.access(R).changeable = true;

      // Note that although R2 is newly used, R3 might be already used if it was 
      // found from R2 through a non-changeable reg_var chain.
      M.set_used_input(t, R, R3);
    }

    return R3;
  }

public:

  int allocate(closure&& C)
  {
    int r = M.push_temp_head();
    M.set_C(r, std::move(C) );
    n_allocated++;
    return r;
  }

  RegOperationArgs* clone() const {return new RegOperationArgs(*this);}

  RegOperationArgs(int r, reg_heap& m, int T, bool ec)
    :R(r),M(m),t(T), n_allocated(0), evaluate_changeable(ec)
  { 
    // I think these should already be cleared.
    assert(M.computation_for_reg(t,R).used_inputs.empty());
  }

  ~RegOperationArgs()
  {
    for(int i=0;i<n_allocated;i++)
      M.pop_temp_head();
  }
};

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>&);
expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids);
expression_ref untranslate_vars(const expression_ref& E, const map<int,string>& ids);
map<int,string> get_constants(const reg_heap& C, int t);

  /*
   * incremental_eval R1
   *
   *   Note: index_var's never have a result, or call, and are never changeable.
   *   Note: only operations can have a call, and only if the operation uses values of changeable parameters.
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
   *      assert(not R1.changeable)
   *      assert(not R1.call)
   *      R3 = incremental_evaluate(R2)
   *      if (R3 != R2)
   *         R1.E = <R3>
   *      return R3
   *      <break>
   *
   *   If (R1.E is WHNF)
   *      R1.result = R1.E
   *      <break>
   *
   *   If R1.E = modifiable and no call
   *      Complain: modifiable should always have a call!
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
   *   assert(R1.result is not an index_var <*>)
   *   return R1
   */

// Perhaps rewrite the expression system to
// (a) DONE: Separate the head (object_ref) from the other args (expression_ref)
// (b) Make a constructor take some number of arguments.
// (c) Change the interpretation of closure constructors so that they are always C n n-1 ... 1 0.
//     I guess if we don't then we have to actually look into the constructor expression.
// (d) DONE: Remove Operation::evaluate( ) and just use lazy_evaluate( ).
// (e) Make translate_refs use only one names for refs that occur twice.
// (f) Make a pretty printer for expression_ref?

/// Evaluate R and look through reg_var chains to return the first reg that is NOT a reg_var.
/// The returned reg is guaranteed to be (a) in WHNF (a lambda or constructor) and (b) not a reg_var.
int reg_heap::incremental_evaluate(int R, int t)
{
  assert(not t or is_root_token(t));
  assert(not t or is_completely_dirty(t));
  assert(is_valid_address(R));
  assert(is_used(R));

#ifndef NDEBUG
  assert(not is_a<expression>(access(R).C.exp));
  if (t and reg_has_result(t,R))
  {
    expression_ref E = access_result_for_reg(t,R).exp;
    assert(is_WHNF(E));
    assert(not is_a<expression>(E));
    assert(not is_a<index_var>(E));
  }
  if (is_index_var(access(R).C.exp))
    assert(not reg_has_result(t,R));
  check_used_reg(R);
#endif

#ifndef NDEBUG
  //  if (not reg_has_result(t,R)) std::cerr<<"Statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

  while (1)
  {
    assert(access(R).C.exp);

#ifndef NDEBUG
    //    std::cerr<<"   statement: "<<R<<":   "<<access(R).E->print()<<std::endl;
#endif

    if (access(R).type == reg::type_t::constant) break;

    else if (reg_is_changeable(R))
    {
      // Changeable is a normal form, so we are done.
      if (not t) break;

      // We have a result, so we are done.
      if (reg_has_result(t,R)) break;

      // If we know what to call, then call it and use it to set the result
      if (reg_has_call(t,R))
      {
	// This should only be an Operation or a modifiable.
	assert(reg_is_changeable(R));
	
	// Only changeable regs have calls, and changeable regs are in normal form unless evaluate_changeable==true.
	assert(t);
	
	// Evaluate S, looking through unchangeable redirections
	int call = incremental_evaluate(call_for_reg(t,R), t);

	// If computation_for_reg(t,R).call can be evaluated to refer to S w/o moving through any changable operations, 
	// then it should be safe to change computation_for_reg(t,R).call to refer to S, even if R is changeable.
	if (call != call_for_reg(t,R))
	{
	  clear_call_for_reg(t,R);
	  set_call(t, R, call);
	}
	
	// R gets its result from S.
	set_computation_result_for_reg(t, R);
	break;
      }
    }

    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    expression_ref T;

    /*---------- Below here, there is no call, and no result. ------------*/

    if (access(R).C.exp->head->type() == index_var_type)
    {
      assert( not reg_is_changeable(R) );

      assert( not reg_has_result(t,R) );

      assert( not reg_has_call(t,R) );

      int index = assert_is_a<index_var>(access(R).C.exp)->index;

      int R2 = access(R).C.lookup_in_env( index );

      int R3 = incremental_evaluate(R2, t);

      // If we point to R3 through an intermediate index_var chain, then change us to point to the end
      if (R3 != R2)
	set_C(R, closure(index_var(0),{R3}));

      check_used_reg(R3);
      return R3;
    }

    // Check for WHNF *OR* heap variables
    else if (is_WHNF(access(R).C.exp))
    {
      access(R).type = reg::type_t::constant;
      if (has_computation(t,R))
	remove_shared_computation(t,R);
    }

#ifndef NDEBUG
    else if (is_a<Trim>(access(R).C.exp))
      std::abort();
    else if (access(R).C.exp->head->type() == parameter_type)
      std::abort();
#endif

    // If we are not evaluating changeable regs, then we shouldn't even get here.
    // A modifiable has a result that is not computed by reducing an expression.
    //       The result must be set.  Therefore, complain if the result is missing.
    else if (access(R).C.exp->head->type() == modifiable_type)
      throw myexception()<<"Reg "<<R<<": Modifiable '"<<access(R).C.exp<<"' with no result?! (Changeable = "<<reg_is_changeable(R)<<")";

    // Reduction: let expression
    else if (parse_indexed_let_expression(access(R).C.exp, bodies, T))
    {
      assert( not reg_is_changeable(R) );

      vector<int> local_env = access(R).C.Env;

      vector<int> new_heap_vars;
      for(int i=0;i<bodies.size();i++)
      {
	// FIXME - do we really want to add a new heap var to point to indirection nodes?
	// And, what would this mean, anyway?

	// Hmm... should this happen at all?  How?

	int V = push_temp_head();
	new_heap_vars.push_back( V );
	local_env.push_back( V );
      }
      
      set_C(R, get_trimmed({T, local_env}));

      // Substitute the new heap vars for the dummy vars in expression T and in the bodies
      for(int i=0;i<bodies.size();i++)
	set_C(new_heap_vars[i], get_trimmed({bodies[i],local_env}));

      assert( not reg_is_changeable(R) );

      // Remove the new heap vars from the list of temp heads in reverse order.
      for(int i=0;i<new_heap_vars.size(); i++)
	pop_temp_head();
      
      assert(not t or not reg_has_call(t,R) );
      assert(not t or not reg_has_result(t,R) );
    }
    
    // 3. Reduction: Operation (includes @, case, +, etc.)
    else
    {
      if (not has_computation(t,R))
	add_shared_computation(t,R);

      object_ptr<const Operation> O = assert_is_a<Operation>( access(R).C.exp );

      // Although the reg itself is not a modifiable, it will stay changeable if it ever computes a changeable result.
      // Therefore, we cannot do "assert(not computation_for_reg(t,R).changeable);" here.

#ifdef DEBUG_MACHINE
      string SS = "";
      SS = compact_graph_expression(*this, R, get_identifiers())->print();
      string SSS = untranslate_vars(deindexify(trim_unnormalize(access(R).C)),  
				    get_identifiers())->print();
      if (log_verbose)
	dot_graph_for_token(*this, t);
#endif

      try
      {
	RegOperationArgs Args(R, *this, t, t>0);
	closure result = (*O)(Args);
	
	// NOTE: While not all used_inputs are E-children, they SHOULD all be E-descendents.
	//       How could we assert that?
	
	// If the reduction doesn't depend on modifiable, then replace E with the result.
	if (not reg_is_changeable(R))
	{
	  // The old used_input slots are not invalid, which is OK since none of them are changeable.
	  assert(not reg_has_call(t,R) );
	  assert(not reg_has_result(t,R));
	  assert(computation_for_reg(t,R).used_inputs.empty());
	  //	  clear_used_inputs_for_reg(t,R);
	  set_C(R, std::move(result) );
	}
	// Otherwise, set the reduction result.
	else
	{
	  if (not t)
	  {
	    remove_shared_computation(t,R);
	    return R;
	  }
	  
	  bool result_is_index_var = result.exp->head->type() == index_var_type;

	  int r2=0;
	  if (result_is_index_var)
	  {
	    int index = convert<const index_var>(result.exp->head)->index;

	    r2 = result.lookup_in_env( index );
    
	    assert(is_used(r2));
	  }
	  else
	  {
	    r2 = push_temp_head();
	    set_C(r2, std::move(result) );
	  }
	  int r3 = incremental_evaluate(r2, t);

	  set_call(t, R, r3);
	  set_computation_result_for_reg(t, R);

	  if (not result_is_index_var)
	    pop_temp_head();
	}
	check_used_reg(R);
      }
      catch (myexception& e)
      {
	dot_graph_for_token(*this, t);

	string SS  = compact_graph_expression(*this, R, get_identifiers())->print();
	string SSS = unlet(untranslate_vars(
					    untranslate_vars(deindexify(trim_unnormalize(access(R).C)), get_identifiers()),
					    get_constants(*this,t)
					    )
			   )->print();
	std::ostringstream o;
	o<<"evaluating reg # "<<R<<": "<<SSS<<"\n\n";
	e.prepend(o.str());
	throw e;
      }
      catch (const std::exception& e)
      {
	std::cerr<<"evaluating reg # "<<R<<std::endl;
	dot_graph_for_token(*this, t);
	throw e;
      }

#ifdef DEBUG_MACHINE
      //      std::cerr<<"   + recomputing "<<SS<<"\n\n";
      std::cerr<<"   + Executing statement {"<<O<<"}:  "<<SS<<"\n\n";
#endif
    }
  }


#ifndef NDEBUG
  check_used_reg(R);
  assert(not is_a<index_var>(access(R).C.exp));
  if (reg_has_result(t,R))
  {
    expression_ref E = access_result_for_reg(t,R).exp;
    assert(not is_a<index_var>(E));
    assert(not is_a<expression>(E));
    assert(is_WHNF(E));
  }
#endif

  return R;
}

void reg_heap::release_scratch_list() const
{
  n_active_scratch_lists--;
}

vector<int>& reg_heap::get_scratch_list() const
{
  while(n_active_scratch_lists >= scratch_lists.size())
    scratch_lists.push_back( new Vector<int> );

  vector<int>& v = *scratch_lists[ n_active_scratch_lists++ ];

  v.clear();

  return v;
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

void discover_graph_vars(const reg_heap& H, int R, map<int,expression_ref>& names, const map<string, int>& id)
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

    bool escape_next = (s[i] == '\\') or (s[i] == '\n') or (s[i] == '"') or (s[i] == '<') or (s[i] == '>');

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
	return identifier(loc->second);
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

map<int,string> get_register_names(const map<string, int>& ids)
{
  map<int,string> ids2;
  for(const auto i:ids)
    ids2[i.second] = i.first;
  return ids2;
}

set<string> get_names(const map<string, int>& ids)
{
  set<string> names;
  for(const auto i:ids)
    names.insert(i.first);
  return names;
}

expression_ref untranslate_vars(const expression_ref& E, const map<string, int>& ids)
{
  return untranslate_vars(E, get_register_names(ids));
}

expression_ref compact_graph_expression(const reg_heap& C, int R, const map<string, int>& ids)
{
  return C[R].C.exp;

  map< int, expression_ref> names;
  for(const auto& id: ids)
  {
    int R = id.second;
    string name = id.first;
    names[R] = expression_ref(new identifier(name) );
  }
  discover_graph_vars(C, R, names, ids);

  return launchbury_unnormalize(names[R]);
}

map<int,string> get_constants(const reg_heap& C, int t)
{
  map<int,string> reg_names = get_register_names(C.get_identifiers());

  map<int,string> constants;

  vector<int> regs = C.find_all_used_regs_in_context(t);

  // Record some regs as being constants worthy of substituting into regs that reference them.
  for(int R: regs)
  {
    if (reg_names.count(R)) continue;

    if (is_index_var(C.access(R).C.exp)) continue;

    if (is_modifiable(C.access(R).C.exp)) continue;

    if (C.access(R).C.exp->size() == 0)
    {
      string name = C.access(R).C.exp->print();
      if (name.size() < 20)
	constants[R] = name;
    }
  }
  return constants;
}


void dot_graph_for_token(const reg_heap& C, int t)
{
  std::ofstream f("token.dot");
  dot_graph_for_token(C, t, f);
  f.close();
}

/* TODO - to make graph more readable:

   1. Handle indirection nodes, somehow.
      (a) First, check WHY we are getting indirection nodes.
      (b) Then Consider eliminating them somehow during garbage collection.

   2. Allow reduction result (call result) on the same level as redex.

 */

void dot_graph_for_token(const reg_heap& C, int t, std::ostream& o)
{
  const auto& ids = C.get_identifiers();

  map<int,string> reg_names = get_register_names(ids);

  const auto& params = C.get_parameters();
  for(const auto& p: params)
    reg_names[p.second] = p.first;

  map<string,string> simplify = get_simplified_names(get_names(ids));

  map<int,string> constants = get_constants(C, t);

  vector<int> regs = C.find_all_used_regs_in_context(t);

  o<<"digraph \"token"<<t<<"\" {\n";
  o<<"graph [ranksep=0.25, fontname=Arial,  nodesep=0.25, ranksep=0.5];\n";
  o<<"node [fontname=Arial, style=filled, height=0, width=0, shape=box];\n";
  o<<"edge [style=\"setlinewidth(2)\"];\n";
  for(int R:regs)
  {
    string name = "n" + convertToString(R);
    // node name
    o<<name<<" ";
    o<<"[";

    expression_ref F = C.access(R).C.exp;

    bool print_record = false;
    if (F->head->type() == operation_type or F->head->type() == constructor_type)
    {
      if (not is_a<Case>(F) and not is_a<Apply>(F))
      {
	print_record = true;
	o<<"shape = record, ";
      }
    }

    // node label = R/name: expression
    string label = convertToString(R);
    if (reg_names.count(R))
      label += "/" + reg_names[R];
    label += ": ";

    vector<int> targets;
    if (print_record)
    {
      label = escape(label);

      label += " |";
      label += escape(F->head->print());
      for(const expression_ref& E: F->sub)
      {
	int index = assert_is_a<index_var>(E)->index;
	int R2 = C.access(R).C.lookup_in_env( index );
	targets.push_back(R2);

	string reg_name = "<" + convertToString(R2) + ">";
	if (reg_names.count(R2))
	{
	  reg_name = reg_names[R2];
	  auto loc = simplify.find(reg_name);
	  if (loc != simplify.end())
	    reg_name = loc->second;
	}
	else if (constants.count(R2))
	  reg_name = constants[R2] + " " + reg_name;
	label += "| <" + convertToString(R2) + "> " + escape(reg_name) + " ";
      }
    }
    else if (F->head->type() == index_var_type)
    {
      int index = assert_is_a<index_var>(F)->index;

      int R2 = C.access(R).C.lookup_in_env( index );

      string reg_name = "<" + convertToString(R2) + ">";
      if (reg_names.count(R2))
      {
	  reg_name = reg_names[R2];
	  auto loc = simplify.find(reg_name);
	  if (loc != simplify.end())
	    reg_name = "<" + loc->second + ">";
      }
      else if (constants.count(R2))
	reg_name = constants[R2] + " " + reg_name;
      label += reg_name;
	
      //      expression_ref E = unlet(untranslate_vars(deindexify(trim_unnormalize(C.access(R).C)), reg_names));
      //      E = map_symbol_names(E, simplify);
      //      label += E->print();
      label = escape(wrap(label,40));
    }
    else
    {
      expression_ref E = unlet(untranslate_vars(untranslate_vars(deindexify(trim_unnormalize(C.access(R).C)), reg_names),constants));

      E = map_symbol_names(E, simplify);

      label += E->print();
      label = escape(wrap(label,40));
    }

    o<<"label = \""<<label<<"\"";
    if (C.access(R).re_evaluate)
      o<<",style=\"dashed,filled\",color=yellow";
    else if (C.reg_is_changeable(R))
      o<<",style=\"dashed,filled\",color=red";

    if (C.reg_is_changeable(R) and C.reg_has_computation_result(t,R))
      o<<",fillcolor=\"#007700\",fontcolor=white";
    else if (C.reg_is_changeable(R))
      o<<",fillcolor=\"#770000\",fontcolor=white";
    else if (C.access(R).C.exp->head->type() == index_var_type)
      o<<",fillcolor=\"#77bbbb\"";
    o<<"];\n";

    // out-edges
    if (print_record)
    {
      for(int R2: targets)
      {
	string name2 = "n" + convertToString(R2);
	bool used = false;
	for(int i: C.used_regs_for_reg(t,R))
	  if (i == R2) used = true;

	// Don't draw ref edges to things like fmap.
	if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	// Don't draw ref edges to things like fmap.
	if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	if (not used)
	  o<<name<<":<"<<R2<<">:s -> "<<name2<<":n;\n";
	else
	  o<<name<<":<"<<R2<<">:s -> "<<name2<<":n [color=\"#007777\"];\n";
      }
    }
    else
    {
      for(int R2: C.access(R).C.Env)
      {
	string name2 = "n" + convertToString(R2);
	bool used = false;
	for(int i: C.used_regs_for_reg(t,R))
	  if (i == R2) used = true;

	// Don't draw ref edges to things like fmap.
	if (reg_names.count(R2) and not C.reg_is_changeable(R2) and not used) continue;
	
	// Don't draw ref edges to things like fmap.
	if (constants.count(R2) and not C.reg_is_changeable(R2) and not used) continue;

	if (not used)
	  o<<name<<":s -> "<<name2<<":n;\n";
	else
	  o<<name<<":s -> "<<name2<<":n [color=\"#007777\"];\n";
      }
    }

    // call-edges
    // FIXME:Drawing - how can allow these to go to the right, but not above, if no ref edges?
    // FIXME:Drawing - doing :w and {rank=same; n -> n} makes the edge drawn over the node icon.
    if (C.reg_has_call(t,R))
    {
      string name2 = "n" + convertToString(C.call_for_reg(t,R));
      o<<name<<":e -> "<<name2<<":w ";
      o<<"[";
      o<<"color=\"#007700\"";
      o<<"];\n";
    }

    // used_inputs
    for(int R2: C.used_regs_for_reg(t,R))
    {
      bool is_ref_edge_also = false;
      for(int R3: C.access(R).C.Env)
	if (R2 == R3)
	  is_ref_edge_also = true;

      if (is_ref_edge_also) continue;

      string name2 = "n" + convertToString(R2);
      o<<name<<":s -> "<<name2<<":n ";
      o<<"[";
      o<<"color=\"#007777\"";
      o<<",style=dashed";
      o<<"];\n";
    }

  }
  o<<"}"<<std::endl;
}
