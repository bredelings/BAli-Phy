#include <iostream>
#include "graph_register.H"
#include "operations.H"
#include "program.H"
#include <algorithm>
#include "util.H"
#include "module.H"
#include "let-float.H"

using std::string;
using std::vector;
using std::map;
using std::pair;
using std::set;
using std::multiset;

using std::cerr;
using std::endl;

int total_reg_allocations = 0;
int total_step_allocations = 0;
int total_comp_allocations = 0;
int total_reroot = 0;
int total_reroot_one = 0;
int total_destroy_token = 0;
int total_release_knuckle = 0;
int total_set_reg_value = 0;
int total_get_reg_value = 0;
int total_get_reg_value_non_const = 0;
int total_get_reg_value_non_const_with_result = 0;
int total_invalidate = 0;
int total_steps_invalidated = 0;
int total_results_invalidated = 0;
int total_steps_scanned = 0;
int total_results_scanned = 0;
int total_context_pr = 0;
int total_tokens = 0;
int total_steps_pivoted = 0;
int total_results_pivoted = 0;

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
 * - value edges (computed by following call edges).
 * The called_by back edges indicate that a value is being used by another value that calls us.
 * Thus called_by edges need not be set when setting a call, but only when setting the value.
 */

/*
 * 1. get_reg_value( )... can we avoid re-rooting?
 *
 * 2. set_reg_value( ): speedup?
 *
 * 3. registering modifiables... can we just create a list inside reg_heap?
 *
 * 4. how could we *dynamically* handle modifiables
 *    - we need to make an MCMC move more them.
 *    - we need to incorporate them into the PDF
 */

/*
 * 1. [DONE] Make the root token into token 0.
 *
 * 2. [DONE] Remove the idea of an unchangable token.
 *
 * 3. [DONE] Make let into an operation.
 *
 * 4. [DONE] Remove t argument from computation_index_for_reg(int t, int r) 
 *
 * 5. Clean up back-edges to computations when computations are destroyed.
 *
 * 6. Move call and used_inputs into reduction
 *
 * 7. Make back-edges from reduction to computations that use it.
 *    - remove duplicate_computation( ).
 *    - HOW does this affect the invalidation algorithm??
 *
 * 8. 
 *
 */

/*
 * OK, so when we invalidate a modifiable, we also unref any local computations that
 * depend on this.  When we destroy a computation, we know that no computation can reference
 * its call -- or, in fact, any reg that the computation created.
 *
 * A. Therefore, we can do brute-force GC on the called reg: we scan all tokens and remove any 
 *    computations for the called reg.  This will lead to MORE regs being freed.  We therefore
 *    loop until no more regs (and thus computations) are being freed.
 */

bool null(const CacheList<int>::iterator& i)
{
  return (i == CacheList<int>::iterator());
}

template<typename V>
void truncate(V& v)
{
  (&v)->~V();
  new (&v) V;
}

expression_ref graph_normalize(const expression_ref& E)
{
  if (not E) return E;

  // 1. Var
  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E.size()) return E;
  
  // 2. Lambda
  if (E.head().is_a<lambda>())
  {
    assert(E.size() == 2);
    object_ptr<expression> V = E.as_expression().clone();
    V->sub[1] = graph_normalize(E.sub()[1]);

    if (V->sub[1].ptr() == E.sub()[1].ptr())
      return E;
    else
      return V;
  }

  // 6. Case
  if (E.head().is_a<Case>())
  {
    object_ptr<expression> V = E.as_expression().clone();

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
  if (E.head().is_a<constructor>() or E.head().is_a<Operation>())
  {
    int var_index = get_safe_binder_index(E);

    object_ptr<expression> E2 = E.as_expression().clone();

    // Actually we probably just need x[i] not to be free in E.sub()[i]
    vector<expression_ref> vars;
    vector<expression_ref> bodies;
    for(int i=0;i<E2->size();i++)
    {
      E2->sub[i] = graph_normalize(E.sub()[i]);

      if (not is_reglike(E2->sub[i]))
      {
	expression_ref var = dummy( var_index++ );

	// 1. Let-bind the argument expression
       	vars.push_back( var );
	bodies.push_back( E2->sub[i] );

	// 2. Replace the argument expression with the let var.
	E2->sub[i] = var;
      }
    }

    return let_expression(vars, bodies, object_ptr<const expression>(E2));
  }

  // 5. Let 
  if (E.head().is_a<let_obj>())
  {
    object_ptr<expression> V = E.as_expression().clone();

    // Normalize the object
    V->sub[0] = graph_normalize(V->sub[0]);

    const int L = (V->sub.size()-1)/2;

    // Just normalize the bodies, not the vars
    for(int i=0;i<L;i++)
      V->sub[2 + 2*i] = graph_normalize(V->sub[2 + 2*i]);

    return V;
  }

  throw myexception()<<"graph_normalize: I don't recognize expression '"+ E.print() + "'";
}


// Deleted: Fun_normalize( ).  Perhaps reintroduce later.
// Translate from language Basic to language Fun by introducing letrec expressions.  (arguments are already vars)
// See "From Natural Semantics to C: A Formal Derivation of two STG machines."
//      by Alberto de la Encina and Ricardo Pena.

void Step::clear()
{
  source_reg = -1;
  call = 0;
  truncate(used_inputs);
  num_created_regs = 0;

  // This should already be cleared.
  assert(flags.none());
}

void Step::check_cleared()
{
  assert(not call);
  assert(used_inputs.empty());
  assert(flags.none());
}

Step& Step::operator=(Step&& S) noexcept
{
  source_reg = S.source_reg;
  call = S.call;
  used_inputs  = std::move( S.used_inputs );
  num_created_regs  = S.num_created_regs;
  flags = S.flags;

  return *this;
}

Step::Step(Step&& S) noexcept
 :source_reg( S.source_reg),
  call ( S.call ),
  used_inputs ( std::move(S.used_inputs) ),
  num_created_regs ( S.num_created_regs ),
  flags ( S.flags )
{ }

void Result::clear()
{
  source_step = -1;
  source_reg = -1;
  value = 0;
  truncate(call_edge);
  truncate(used_by);
  truncate(called_by);

  // This should already be cleared.
  assert(flags.none());
}

void Result::check_cleared()
{
  assert(not value);
  assert(not call_edge.first);
  assert(called_by.empty());
  assert(used_by.empty());
  assert(flags.none());
}

Result& Result::operator=(Result&& R) noexcept
{
  value = R.value;
  source_step = R.source_step;
  source_reg = R.source_reg;
  call_edge = R.call_edge;
  used_by = std::move( R.used_by );
  called_by = std::move( R.called_by );
  flags = R.flags;

  return *this;
}

Result::Result(Result&& R) noexcept
:source_step(R.source_step),
  source_reg(R.source_reg),
  value (R.value), 
  call_edge (R.call_edge),
  used_by ( std::move( R.used_by) ),
  called_by ( std::move( R.called_by) ),
  flags ( R.flags )
{ }

reg& reg::operator=(reg&& R) noexcept
{
  C = std::move(R.C);

  re_evaluate = R.re_evaluate;

  type = R.type;

  n_heads = R.n_heads;

  return *this;
}

reg::reg(reg&& R) noexcept
:C( std::move(R.C) ),
  re_evaluate( R.re_evaluate ),
  type ( R.type ),
  n_heads( R.n_heads )
{ }

void reg::clear()
{
  assert(n_heads == 0);
  C.clear();
  re_evaluate = false;
  type = type_t::unknown;
}

void reg::check_cleared()
{
  assert(not C);
  assert(not re_evaluate);
  assert(type == type_t::unknown);
  assert(n_heads == 0);
}

bool mapping::has_value(int r) const {return operator[](r);}

void mapping::add_value(int r, int v) 
{
  assert(not has_value(r));
  assert(v);

  address A(v);
  A.index = delta_.size();

  values[r] = A;
  delta_.emplace_back(r,v);

  assert(delta_[values[r].index].first == r);
  assert(delta_[values[r].index].second == v);
}

int mapping::erase_value(int r)
{
  int v = values[r].value;

  // The reg should be mapped.
  assert(v);

  // Check correspondence between delta_ and values
  assert(delta_[values[r].index].first == r);

  // Lookup the position in delta_ where we mention r
  int index = values[r].index;

  auto back = delta_.back();
  int r2 = back.first;
  delta_.pop_back();

  // If we are deleting from the middle, move the last element to the middle
  if (index < delta_.size())
  {
    delta_[index] = back;
    values[r2].index = index;
    assert(delta_[values[r2].index].first == r2);
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
  delta_[values[r].index].second = v;
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
  for(auto p: delta_)
    values[p.first] = {};
  delta_.clear();
}

void mapping::resize(int s)
{
  values.resize(s);
  delta_.reserve(s);
}

int mapping::size() const
{
  return values.size();
}

bool mapping::empty() const
{
  return delta_.empty();
}

void reg_heap::register_probability(int r)
{
  mark_completely_dirty(root_token);
  r = incremental_evaluate(r).first;

  if (reg_is_constant(r))
  {
    log_double_t pr = access(r).C.exp.as_log_double();
    constant_pr *= pr;
  }
  else
  {
    assert(reg_is_changeable(r));

    int rc = result_index_for_reg(r);
    assert(rc > 0);

    probability_heads.push_back(r);

    inc_heads(r);

    prs_list.push_back(r);
  }
}

int reg_heap::register_probability(closure&& C)
{
  int r = allocate();
  total_reg_allocations++;
  set_C(r, std::move(C));
  register_probability(r);
  return r;
}

bool reg_heap::inc_probability_for_reg(int r)
{
  assert(reg_is_changeable(r));
  int rc = result_index_for_reg(r);

  if (rc > 0 and results[rc].flags.test(0)) return true; // already included

  incremental_evaluate(r);
  rc = result_index_for_reg(r);

  return inc_probability(rc);
}

void reg_heap::dec_probability_for_reg(int r)
{
  int rc = result_index_for_reg(r);

  if (rc > 0 and results[rc].flags.test(0))
    dec_probability(rc);
}

void reg_heap::dec_probability(int rc)
{
  assert(rc > 0);
  int r2 = results[rc].value;
  assert(r2 > 0);
  log_double_t pr = access(r2).C.exp.as_log_double();

  variable_pr /= pr;
  results[rc].flags.reset(0);

  int r = results[rc].source_reg;
  assert(reg_is_changeable(r));
  assert(access(r).n_heads > 0);
  prs_list.push_back(r);
}

double id(double x) {return x;}

log_double_t reg_heap::probability_for_context_diff(int c)
{
  reroot_at_context(c);
  unhandled_pr.log() = 0.0;

  // re-multiply all probabilities
  if (total_error > 1.0e-9)
  {
    for(int r: probability_heads)
    {
      int rc = result_index_for_reg(r);
      if (rc > 0 and results[rc].flags.test(0))
	dec_probability(rc);
    }
    // std::cerr<<"unwinding all prs: total_error = "<<total_error<<" variable_pr = "<<variable_pr<<"  error_pr = "<<error_pr<<"   variable_pr/error_pr = "<<variable_pr/error_pr<<std::endl;
    assert((variable_pr/error_pr).log() < 1.0e-6);
    assert(prs_list.size() == probability_heads.size());
    total_error = 0;
    variable_pr.log() = 0;
    error_pr.log() = 0;
  }

  if (not prs_list.empty())
  {
    mark_completely_dirty(root_token);

    int j=0;
    for(int i=0;i<prs_list.size();i++)
    {
      int r = prs_list[i];
      if (not inc_probability_for_reg(r))
	prs_list[j++] = r;
    }
    prs_list.resize(j);
  }

  return variable_pr * constant_pr * unhandled_pr / error_pr;
}

log_double_t reg_heap::probability_for_context(int c)
{
  total_context_pr++;

  log_double_t Pr = probability_for_context_diff(c);
  // std::cerr<<"A:   Pr1 = "<<Pr<<"   error = "<<total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<variable_pr<<"  unhandled = "<<unhandled_pr<<std::endl;

#ifndef NDEBUG  
  // log_double_t Pr2 = probability_for_context_full(c);
  // double diff = Pr.log() - Pr2.log();
  // std::cerr<<"B:diff = "<<diff<<"    Pr1 = "<<Pr<<"  Pr2 = "<<Pr2<<"   error = "<<total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<variable_pr<<"  unhandled = "<<unhandled_pr<<std::endl;
  //  assert(fabs(diff) < 1.0e-6);
#endif

  return Pr;
}

const vector<int>& reg_heap::random_modifiables() const
{
  return random_modifiables_;
}

int reg_heap::add_random_modifiable(int r)
{
  int i = random_modifiables_.size();
  random_modifiables_.push_back(r);
  inc_heads(r);
  return i;
}

bool reg_heap::parameter_is_modifiable(int index)
{
  int R = parameters[index].second;

  int R2 = incremental_evaluate_unchangeable(R);

  return is_modifiable(access(R2).C.exp);
}

int reg_heap::find_parameter_modifiable_reg(int index)
{
  assert(index >= 0);

  int R = parameters[index].second;

  int R2 = incremental_evaluate_unchangeable(R);

  if (R != R2)
  {
    dec_heads(R);
    inc_heads(R2);
    parameters[index].second = R2;
  }

#ifndef NDEBUG
  if (not is_modifiable(access(R2).C.exp))
    throw myexception()<<"Parameter is not a modifiable!  Instead its value is '"<<access(R2).C.exp<<"'";
#endif

  assert(R2>0);
  return R2;
}

const expression_ref reg_heap::get_parameter_range(int c, int p)
{
  return get_range_for_reg(c, find_parameter_modifiable_reg(p));
}

const expression_ref reg_heap::get_range_for_reg(int c, int r)
{
  if (access(r).C.Env.size() < 3)
    return {};

  int r2 = access(r).C.lookup_in_env(2);
  return get_reg_value_in_context(r2,c);
}

double reg_heap::get_rate_for_reg(int r)
{
  if (access(r).C.Env.size() < 3)
    return {};

  int r3 = access(r).C.lookup_in_env(0);
  r3 = incremental_evaluate_unchangeable(r3);
  return access(r3).C.exp.as_double();
}

const std::vector<int>& reg_heap::triggers() const {return tokens[root_token].triggers;}
      std::vector<int>& reg_heap::triggers()       {return tokens[root_token].triggers;}

int reg_heap::step_index_for_reg(int r) const 
{
  return tokens[root_token].vm_step[r];
}

int reg_heap::result_index_for_reg(int r) const 
{
  return tokens[root_token].vm_result[r];
}

const Step& reg_heap::step_for_reg(int r) const 
{ 
  int s = step_index_for_reg(r);
  return steps.access_unused(s);
}

Step& reg_heap::step_for_reg(int r)
{ 
  int s = step_index_for_reg(r);
  return steps.access_unused(s);
}

const Result& reg_heap::result_for_reg(int r) const 
{ 
  int rc = result_index_for_reg(r);
  return results.access_unused(rc);
}

Result& reg_heap::result_for_reg(int r)
{ 
  int rc = result_index_for_reg(r);
  return results.access_unused(rc);
}

const closure& reg_heap::access_value_for_reg(int R1) const
{
  int R2 = value_for_reg(R1);
  assert(R2);
  return access(R2).C;
}

bool reg_heap::reg_has_value(int r) const
{
  if (access(r).type == reg::type_t::constant)
    return true;
  else
    return reg_has_result_value(r);
}

bool reg_heap::reg_has_result_value(int r) const
{
  return has_result(r) and result_value_for_reg(r);
}

bool reg_heap::reg_has_call(int r) const
{
  return has_step(r) and call_for_reg(r);
}

int reg_heap::call_for_reg(int r) const
{
  return step_for_reg(r).call;
}

bool reg_heap::reg_has_value_(int t, int r) const
{
  if (access(r).type == reg::type_t::constant)
    return true;
  else
    return reg_has_result_value_(t,r);
}

bool reg_heap::reg_has_result_value_(int t, int r) const
{
  return has_result_(t,r) and result_value_for_reg_(t,r);
}

bool reg_heap::reg_has_call_(int t, int r) const
{
  return has_result_(t,r) and call_for_reg_(t,r);
}

int reg_heap::call_for_reg_(int t, int r) const
{
  return step_for_reg_(t,r).call;
}

bool reg_heap::has_step(int r) const
{
  return step_index_for_reg(r)>0;
}

bool reg_heap::has_result(int r) const
{
  return result_index_for_reg(r)>0;
}

bool reg_heap::has_step_(int t, int r) const
{
  return step_index_for_reg_(t,r)>0;
}

bool reg_heap::has_result_(int t, int r) const
{
  return result_index_for_reg_(t,r)>0;
}

const Step& reg_heap::step_for_reg_(int t, int r) const 
{ 
  int rc = step_index_for_reg_(t,r);
  return steps.access_unused(rc);
}

Step& reg_heap::step_for_reg_(int t, int r)
{ 
  int rc = step_index_for_reg_(t,r);
  return steps.access_unused(rc);
}

const Result& reg_heap::result_for_reg_(int t, int r) const 
{ 
  int rc = result_index_for_reg_(t,r);
  return results.access_unused(rc);
}

Result& reg_heap::result_for_reg_(int t, int r)
{ 
  int rc = result_index_for_reg_(t,r);
  return results.access_unused(rc);
}

int reg_heap::step_index_for_reg_(int t, int r) const 
{
  return tokens[t].vm_step[r];
}

int reg_heap::result_index_for_reg_(int t, int r) const 
{
  return tokens[t].vm_result[r];
}

int reg_heap::value_for_reg(int r) const 
{
  assert(not access(r).C.exp.is_index_var());
  if (access(r).type == reg::type_t::changeable)
    return result_value_for_reg(r);
  else
  {
    assert(access(r).type == reg::type_t::constant);
    return r;
  }
}

int reg_heap::result_value_for_reg(int r) const 
{
  return result_for_reg(r).value;
}

int reg_heap::result_value_for_reg_(int t, int r) const 
{
  return result_for_reg_(t,r).value;
}

void reg_heap::set_result_value_for_reg(int r1)
{
  int call = call_for_reg(r1);

  assert(call);

  int value = value_for_reg(call);

  assert(value);

  int rc1 = result_index_for_reg(r1);
  if (rc1 <= 0)
    rc1 = add_shared_result(root_token, r1, step_index_for_reg(r1));
  assert(rc1 > 0);
  auto& RC1 = results[rc1];
  RC1.value = value;

  // If R2 is WHNF then we are done
  if (access(call).type == reg::type_t::constant) return;

  // If R2 doesn't have a result, add one to hold the called-by edge.
  assert(has_result(call));

  // Add a called-by edge to R2.
  int rc2 = result_index_for_reg(call);
  auto back_edge = results[rc2].called_by.push_back(rc1);
  RC1.call_edge.first = rc2;
  RC1.call_edge.second = back_edge;
}

void reg_heap::set_used_input(int s1, int R2)
{
  assert(reg_is_changeable(R2));

  assert(is_used(R2));

  assert(access(R2).C);

  assert(has_result(R2));
  assert(result_value_for_reg(R2));

  // An index_var's value only changes if the thing the index-var points to also changes.
  // So, we may as well forbid using an index_var as an input.
  assert(access(R2).C.exp.head().type() != index_var_type);

  int rc2 = result_index_for_reg(R2);

  auto back_edge = results[rc2].used_by.push_back(s1);
  steps[s1].used_inputs.push_back({rc2,back_edge});

  assert(result_is_used_by(s1,rc2));
}

int count(const std::vector<int>& v, int I)
{
  int c = 0;
  for(int i: v)
    if (i == I)
      c++;
  return c;
}

void reg_heap::set_call(int R1, int R2)
{
  assert(reg_is_changeable(R1));
  // R2 might be of UNKNOWN changeableness

  // Check that R1 is legal
  assert(is_used(R1));

  // Check that R2 is legal
  assert(is_used(R2));

  // Only modify the call for the current context;
  assert(has_step(R1));

  // Don't override an *existing* call
  assert(not reg_has_call(R1));

  // Check that we aren't overriding an existing *value*
  assert(not reg_has_value(R1));

  // Set the call
  step_for_reg(R1).call = R2;
}

void reg_heap::destroy_all_computations_in_token(int t)
{
  // Remove use back-edges
  auto delta_step = tokens[t].delta_step();
  auto delta_result = tokens[t].delta_result();

  for(auto p: delta_step)
  {
//    int r = p.first;
    int s = p.second;
    if (s > 0)
      clear_back_edges_for_step(s);
  }

  // Remove call back-edges
  for(auto p: delta_result)
  {
//    int r = p.first;
    int rc = p.second;
    if (rc > 0)
      clear_back_edges_for_result(rc);
  }

  for(auto p: delta_step)
  {
//    int r = p.first;
    int s = p.second;
    if (s > 0)
      steps.reclaim_used(s);
  }
  tokens[t].vm_step.clear();

  for(auto p: delta_result)
  {
//    int r = p.first;
    int rc = p.second;
    if (rc > 0)
      results.reclaim_used(rc);
  }
  tokens[t].vm_result.clear();
}

void reg_heap::clear_call(int s)
{
  steps.access_unused(s).call = 0;
}

void reg_heap::clear_call_for_reg(int R)
{
  int s = step_index_for_reg(R);
  if (s > 0)
    clear_call( s );
}

void reg_heap::set_C(int R, closure&& C)
{
  assert(C);
  assert(not C.exp.head().is_a<expression>());
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

void reg_heap::mark_reg_created_by_step(int r, int s)
{
  assert(r > 0);
  assert(s > 0);
  steps[s].num_created_regs++;
}

int reg_heap::create_reg_from_step(int s)
{
  total_reg_allocations++;
  int r = allocate();
  mark_reg_created_by_step(r,s);
  return r;
}


// If we replace a computation at P that is newly defined in this token,
// there may be computations that call or use it that are also newly
// defined in this token.  Such computations must be cleared, because they
// do not use a value defined in a previous token, and so would not be detected
// as invalidate by invalidate_shared_regs( ), which can only detect computations
// as invalidate if they use a computation valid in a parent context.
//
// As a value, every computation that we invalidate is going to be newly defined
// in the current context.  Other computations can be invalidated later.

/// Update the value of a non-constant, non-computed index
void reg_heap::set_reg_value(int R, closure&& value, int t)
{
  total_set_reg_value++;
  assert(not is_dirty(t));
  assert(not children_of_token(t).size());
  assert(reg_is_changeable(R));

  if (not is_root_token(t) and tokens[t].version == tokens[parent_token(t)].version)
    tokens[t].version--;

  // assert(not is_root_token and tokens[t].version < tokens[parent_token(t)].version) 

  // Check that this reg is indeed settable
  assert(is_modifiable(access(R).C.exp));

  assert(not is_root_token(t));

  // Finally set the new value.
  int s = add_shared_step(t,R);

  assert(not children_of_token(t).size());

  // if the value is NULL, just leave the value and call both unset.
  //  (this could happen if we set a parameter value to null.)
  if (not value) return;

  // If the value is a pre-existing reg_var, then call it.
  if (value.exp.head().type() == index_var_type)
  {
    int index = value.exp.as_index_var();

    int Q = value.lookup_in_env( index );

    assert(is_used(Q));

    // Set the call
    steps[s].call = Q;
  }
  // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the value and call it.
  else
  {
    int R2 = create_reg_from_step(s);

    // clear 'reg created' edge from s to old call.
    steps[s].num_created_regs = 0;
    steps[s].call = R2;

    // Set the call
    set_C(R2, std::move( value ) );
  }
  clear_result(t,R);

#if DEBUG_MACHINE >= 2
  check_used_regs();
  check_tokens();
#endif
}

void reg_heap::set_shared_value(int r, int v)
{
  // add a new computation
  int step = add_shared_step(root_token, r);
  add_shared_result(root_token, r, step);

  // set the value
  set_call(r, v);
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
void reg_heap::merge_split_mapping(int t1, int t2)
{
  merge_split_step_mapping(t1, t2);
  merge_split_relative_mapping(t1, t2);
}

void reg_heap::merge_split_step_mapping(int t1, int t2)
{
    auto& vm1 = tokens[t1].vm_step;
    auto& vm2 = tokens[t2].vm_step;

    auto delta_step1 = vm1.delta();
    auto delta_step2 = vm2.delta();

    for(auto p: delta_step2)
    {
	int r = p.first;
	prog_temp[r] = 1;
    }

    for(int i=0;i<delta_step1.size();)
    {
	int r = delta_step1[i].first;
	int v = delta_step1[i].second;

	if (not prog_temp[r])
	{
	    delta_step2.emplace_back(r,v);
	    vm2.add_value(r,v);

	    if (i < delta_step1.size())
		delta_step1[i] = delta_step1.back();
	    delta_step1.pop_back();
	    vm1.erase_value(r);
	}
	else
	    i++;
    }

    for(auto p: delta_step2)
    {
	int r = p.first;
	prog_temp[r] = 0;
    }
}

// Given mapping (m1,v1) followed by (m2,v2), compute a combined mapping for (m1,v1)+(m2,v2) -> (m2,v2)
// and a mapping (m1,v1)-(m2,v2)->(m1,v1) for things that now are unused.
void reg_heap::merge_split_relative_mapping(int t1, int t2)
{
    auto& vm1 = tokens[t1].vm_result;
    auto& vm2 = tokens[t2].vm_result;

    auto delta_result1 = vm1.delta();
    auto delta_result2 = vm2.delta();

    for(auto p: delta_result2)
    {
	int r = p.first;
	prog_temp[r] = 1;
    }

    for(int i=0;i<delta_result1.size();)
    {
	int r = delta_result1[i].first;
	int v = delta_result1[i].second;

	if (not prog_temp[r])
	{
	    delta_result2.emplace_back(r,v);
	    vm2.add_value(r,v);

	    if (i < delta_result1.size())
		delta_result1[i] = delta_result1.back();
	    delta_result1.pop_back();
	    vm1.erase_value(r);
	}
	else
	    i++;
    }

    for(auto p: delta_result2)
    {
	int r = p.first;
	prog_temp[r] = 0;
    }
}

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
void reg_heap::pivot_step_mapping(int t1, int t2)
{
  auto& vm1 = tokens[t1].vm_step;
  auto& vm2 = tokens[t2].vm_step;

  total_steps_pivoted += vm2.delta().size();
  
  for(int i=0;i<vm2.delta().size();i++)
  {
    int r = vm2.delta()[i].first;
    assert(vm2[r]);

    int s1 = vm1[r];
    int s2 = vm2[r];

    // switch from root/0 => root/-
    if (s1 == 0) s1 = -1;

    // switch root positions
    std::swap(s1,s2);

    // switch from root/0 => root/-
    if (s1 == -1) s1 = 0;

    vm1.set_value(r,s1);
    vm2.set_value(r,s2);
  }
  std::swap(vm1,vm2);
}

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
void reg_heap::pivot_relative_mapping(int t1, int t2)
{
  auto& vm1 = tokens[t1].vm_result;
  auto& vm2 = tokens[t2].vm_result;

  total_results_pivoted += vm2.delta().size();

  for(int i=0;i<vm2.delta().size();i++)
  {
    int r = vm2.delta()[i].first;
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

void reg_heap::reroot_at_context(int c)
{
  // 1. Bail if we are already at the root.
  int t = token_for_context(c);
  if (is_root_token(t)) return;

  total_reroot++;
  
  // 2. Get the tokens on the path to the root.
  boost::container::small_vector<int,10> path;
  path.push_back(token_for_context(c));
  while(true)
  {
    int parent = tokens[path.back()].parent;
    if (parent != -1)
      path.push_back(parent);
    else
      break;
  }

  // 3. Get the tokens on the path to the root.
  for(int i=int(path.size())-2; i>=0; i--)
    reroot_at(path[i]);

  // 4. Clean up old root token if it became a tip, and remote intermediate knuckles
  int old_root = path.back();
  for(int t2 = old_root; t2 != root_token ; )
  {
    int p = tokens[t2].parent;

    if (not tokens[t2].is_referenced())
    {
      if (tokens[t2].children.empty())
	release_tip_token(t2);
      else if (tokens[t2].children.size() == 1 and t2 != old_root)
	release_knuckle_token(t2);
    }

    t2 = p;
  }
}

void reg_heap::reroot_at(int t)
{
  assert(not is_root_token(t) and is_root_token(tokens[t].parent));

#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  // 1. If this context isn't a direct child of the root, then make it one
  if (not is_root_token(parent_token(t)))
    reroot_at(parent_token(t));

  // re-rooting to the parent context shouldn't release its token.
  int parent = parent_token(t);
  assert(is_root_token(parent));

  // 2. Unshare regs
  unshare_regs(t);

  // 3. Change the relative mappings
  pivot_step_mapping(parent, t);
  pivot_relative_mapping(parent, t);

  // 4. Alter the inheritance tree
  tokens[parent].parent = t;
  int index = remove_element(tokens[parent].children, t);
  assert(index != -1);

  tokens[t].parent = -1;
  tokens[t].children.push_back(parent);

  root_token = t;
  assert(is_root_token(t));

  // 5. Remove probabilities for invalidated regs from the current probability

  for(auto p: tokens[parent].vm_result.delta())
  {
    int r = p.first;
    int rc = p.second;  
    if (rc > 0 and results[rc].flags.test(0))
      dec_probability(rc);
  }

  total_reroot_one++;
  
  assert(tokens[parent].version == tokens[t].version);

  for(int t2: tokens[t].children)
    assert(tokens[t2].version <= tokens[t].version);

  assert(is_root_token(t));

  // 6. re-evaluate all the regs that need to be up-to-date.
  if (tokens[t].regs_to_re_evaluate.size())
    mark_completely_dirty(t);
  for(int R: tokens[t].regs_to_re_evaluate)
    incremental_evaluate(R);
  tokens[t].regs_to_re_evaluate.clear();
}

/*
 * If parent token's version is greater than its child, this means that there could
 * be computations in the parent that are shared into the child that should not be.
 *
 * This occurs EITHER if we perform computation in the parent, OR of we alter a modifiable
 * value in the child.  Therefore, we increase the root version (mark_completely_dirty)
 * before executing in the root token, and decrease the child version when changing its 
 * modifiable values.
 *
 * Computations that are improperly shared into the child have dependencies on computations
 * in the parent context even though these computations are overridden in the child context.
 * We detect and invalidate such computations in invalidate_shared_regs( ).
 */

void reg_heap::mark_completely_dirty(int t)
{
  int& version = tokens[t].version;
  for(int t2:tokens[t].children)
    version = std::max(version, tokens[t2].version+1);
}

bool reg_heap::is_dirty(int t) const
{
  for(int t2:tokens[t].children)
    if (tokens[t].version > tokens[t2].version)
      return true;
  return false;
}

vector<pair<int,int>> reg_heap::Token::delta_result() const
{
  return vm_result.delta();
}

vector<pair<int,int>> reg_heap::Token::delta_step() const
{
  return vm_step.delta();
}


// Note that a context can be completely dirty, w/o being dirty :-P
bool reg_heap::is_completely_dirty(int t) const
{
  for(int t2:tokens[t].children)
    if (tokens[t].version <= tokens[t2].version)
      return false;
  return true;
}
  
void reg_heap::unshare_regs(int t)
{
  assert(is_root_token(parent_token(t)));
  assert(tokens[root_token].version >= tokens[t].version);

  if (tokens[root_token].version <= tokens[t].version) return;

#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif

  total_invalidate++;
  
  // find all regs in t that are not shared from the root
  auto delta_result = tokens[t].delta_result();
  auto delta_step = tokens[t].delta_step();
  
  int n_delta_result0 = delta_result.size();
  int n_delta_step0 = delta_step.size();
  
  for(const auto& p: delta_result)
  {
    int r = p.first;
    prog_temp[r] |= 1;
  }

  for(const auto& p: delta_step)
  {
    int r = p.first;
    prog_temp[r] |= 2;
    assert(prog_temp[r] == 3);
  }

  auto& vm_result = tokens[t].vm_result;
  auto& vm_step = tokens[t].vm_step;
  auto& regs_to_re_evaluate = tokens[t].regs_to_re_evaluate;

  for(int i=0;i<delta_result.size();i++)
  {
    int r = delta_result[i].first;

//    int result = result_index_for_reg(r);

    if (not has_result(r)) continue;

    const auto& Result = result_for_reg(r);

    for(int res2: Result.called_by)
    {
      const auto& Result2 = results[res2];
      int r2 = Result2.source_reg;

      // This result is already unshared
      if (prog_temp[r2] != 0) continue;

      prog_temp[r2] = 1;
      delta_result.emplace_back(r2,-1);
    }

    for(int s2: Result.used_by)
    {
      auto& S2 = steps[s2];
      int r2 = S2.source_reg;

      // This step is already unshared
      if (prog_temp[r2] == 3) continue;

      if (prog_temp[r2] == 0)
	delta_result.emplace_back(r2,-1);

      prog_temp[r2] = 3;
      delta_step.emplace_back(r2,-1);
      vm_step.add_value(r2,-1);
    }
  }

  for(int i=n_delta_result0;i<delta_result.size();i++)
  {
    int r = delta_result[i].first;
    vm_result.add_value(r,-1);
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }
  
  for(const auto& p: delta_result)
  {
    int r = p.first;
    prog_temp[r] = 0;
  }

  total_results_invalidated += (delta_result.size() - n_delta_result0);
  total_steps_invalidated += (delta_step.size() - n_delta_step0);

  total_results_scanned += delta_result.size();
  total_steps_scanned += delta_step.size();

  tokens[t].version = tokens[root_token].version;
  
#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif
}


std::vector<int> reg_heap::used_regs_for_reg(int r) const
{
  vector<int> U;
  if (not has_step(r)) return U;

  for(const auto& rcp: step_for_reg(r).used_inputs)
    U.push_back(results[rcp.first].source_reg);

  return U;
}

void reg_heap::reclaim_used(int r)
{
#ifndef NDEBUG  
  for(int t=0;t<tokens.size();t++)
  {
    assert(not tokens[t].vm_result[r]);
    assert(not tokens[t].vm_step[r]);
  }
#endif

  pool<reg>::reclaim_used(r);
}

template <typename T>
void insert_at_end(vector<int>& v, const T& t)
{
  v.insert(v.end(), t.begin(), t.end());
}

void reg_heap::get_roots(vector<int>& scan, bool keep_identifiers) const
{
  insert_at_end(scan, stack); // inc_heads = yes
  insert_at_end(scan, temp); // yes
  insert_at_end(scan, heads); // yes
  insert_at_end(scan, probability_heads); // yes
  insert_at_end(scan, random_modifiables_); // yes
  insert_at_end(scan, transition_kernels_); // yes
  for(int j=0;j<parameters.size();j++) // yes
    scan.push_back(parameters[j].second);
  if (keep_identifiers)
    for(const auto& i: identifiers) // no
      scan.push_back(i.second);
}

int reg_heap::inc_heads(int R)
{
  assert( access(R).n_heads >= 0);
  access(R).n_heads++;
  return access(R).n_heads;
}

int reg_heap::dec_heads(int R)
{
  assert( access(R).n_heads >= 0);
  access(R).n_heads--;
  assert( access(R).n_heads >= 0);
  return access(R).n_heads;
}

int reg_heap::set_head(int index, int R2)
{
  int R1 = heads[index];

  inc_heads(R2);

  heads[index] = R2;

  dec_heads(R1);

  return R1;
}

int reg_heap::allocate_head()
{
  int R = allocate();
  total_reg_allocations++;

  heads.push_back(R);

  inc_heads(R);

  return R;
}

int reg_heap::push_temp_head()
{
  int R = allocate();
  total_reg_allocations++;

  temp.push_back(R);

  inc_heads(R);

  return R;
}

int reg_heap::push_temp_head(int R)
{
  temp.push_back(R);

  inc_heads(R);

  return R;
}

void reg_heap::pop_temp_head()
{
  int R = temp.back();

  dec_heads(R);

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
  for(int t=0;t<tokens.size();t++)
  {
    assert(tokens[t].vm_step.size() == old_size);
    assert(tokens[t].vm_result.size() == old_size);
  }

  base_pool_t::expand_memory(s);

  // Extend virtual mappings, with virtual_mapping[i] = 0;
  for(int t=0;t<tokens.size();t++)
  {
    tokens[t].vm_step.resize(size());
    tokens[t].vm_result.resize(size());
    for(int i=old_size;i<size();i++)
    {
      assert(tokens[t].vm_step[i] == 0);
      assert(tokens[t].vm_result[i] == 0);
    }
  }

  // Extend program
  prog_steps.resize(size());
  prog_results.resize(size());
  prog_temp.resize(size());
  for(int i=old_size;i<size();i++)
  {
    assert(prog_steps[i] == 0);
    assert(prog_results[i] == 0);
    assert(prog_temp[i] == 0);
  }
}

bool reg_heap::reg_is_constant(int r) const
{
  return access(r).type == reg::type_t::constant;
}

bool reg_heap::reg_is_changeable(int r) const
{
  return access(r).type == reg::type_t::changeable;
}

void reg_heap::make_reg_changeable(int r)
{
  assert( access(r).type == reg::type_t::changeable or access(r).type == reg::type_t::unknown );

  access(r).type = reg::type_t::changeable;
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

int reg_heap::get_unused_token()
{
  if (unused_tokens.empty())
  {
    unused_tokens.push_back(get_n_tokens());
    tokens.push_back(Token());
    tokens.back().vm_step.resize(size());
    tokens.back().vm_result.resize(size());
    for(int i=0;i<size();i++)
    {
      assert(tokens.back().vm_step[i] == 0);
      assert(tokens.back().vm_result[i] == 0);
    }
    total_tokens = tokens.size();
  }

#ifndef NDEBUG
  for(int i=0;i<tokens.size();i++)
  {
    assert(tokens[i].vm_step.size() == size());
    assert(tokens[i].vm_result.size() == size());
  }
#endif

  int t = unused_tokens.back();
  unused_tokens.pop_back();

  assert(not token_is_used(t));

  tokens[t].used = true;

  if (root_token == -1)
  {
    assert(tokens.size() - unused_tokens.size() == 1);
    root_token = t;
  }
  else
    assert(tokens.size() - unused_tokens.size() > 1);

  assert(tokens[t].parent == -1);
  assert(tokens[t].children.empty());
  assert(tokens[t].vm_step.empty());
  assert(tokens[t].vm_result.empty());
  assert(not tokens[t].is_referenced());

  return t;
}

bool reg_heap::result_is_called_by(int rc1, int rc2) const
{
  for(int rc: results[rc2].called_by)
    if (rc == rc1)
      return true;

  return false;
}

bool reg_heap::result_is_used_by(int s1, int rc2) const
{
  for(int s: results[rc2].used_by)
    if (s == s1)
      return true;

  return false;
}

bool reg_heap::reg_is_used_by(int r1, int r2) const
{
  int s1 = step_index_for_reg(r1);
  int rc2 = result_index_for_reg(r2);

  return result_is_used_by(s1,rc2);
}

bool reg_heap::step_is_referenced(int t,int s) const
{
  assert(s);
  int r = steps[s].source_reg;
  if (step_index_for_reg_(t,r) == s) return true;
  int p = parent_token(t);
  if (p != -1)
    return step_is_referenced(p, s);
  else
    return false;
}

bool reg_heap::result_is_referenced(int t,int rc) const
{
  assert(rc);
  int r = results[rc].source_reg;
  if (result_index_for_reg_(t,r) == rc) return true;
  int p = parent_token(t);
  if (p != -1)
    return result_is_referenced(p, rc);
  else
    return false;
}

void reg_heap::check_tokens() const
{
#ifndef NDEBUG
  for(int c=0;c<get_n_contexts();c++)
  {
    int t = token_for_context(c);
    if (t >= 0)
    {
      assert(tokens[t].is_referenced());
      assert(tokens[t].used);
    }
  }

  for(int t=0;t<tokens.size();t++)
    if (token_is_used(t))
    {
      assert(tokens[t].is_referenced() or tokens[t].children.size() >= 1);
      for(int t2: children_of_token(t))
	assert(tokens[t].version >= tokens[t2].version);
    }
#endif
}

void reg_heap::check_used_reg(int r) const
{
  for(int t=0;t<get_n_tokens();t++)
  {
    if (not token_is_used(t)) continue;

    if (is_root_token(t))
    {
      assert(tokens[t].vm_step[r] != -1);
      assert(tokens[t].vm_result[r] != -1);
    }

    if (not is_root_token(t))
    {
      int p = parent_token(t);

      // If there are + values in adjacent contexts, they should not the be same value
      if (tokens[t].vm_step[r] > 0 and tokens[p].vm_step[r] > 0)
	assert(tokens[t].vm_step[r] != tokens[p].vm_step[r]);

      if (tokens[t].vm_result[r] > 0 and tokens[p].vm_result[r] > 0)
	assert(tokens[t].vm_result[r] != tokens[p].vm_result[r]);

      // If we have a new step in a token, we should not share the result from out parent.
      // This would hold for all tokens, except that in the root token "0" means "-" and not "="
      if (tokens[t].vm_step[r] > 0)
	assert(tokens[t].vm_result[r] != 0);
    }

    if (access(r).type == reg::type_t::constant)
      assert(not has_result_(t,r));

    // Any checks for result, but no step?

    // Below this we have a step.
    if (not has_step_(t, r)) continue;
    int call = call_for_reg_(t,r);
    int r_s = step_index_for_reg_(t,r);
    int r_r = result_index_for_reg_(t,r);

    // If we have a new step, we cannot share a result.  (In the root token 0 means 'unshare', not 'share')
    assert(is_root_token(t) or r_r != 0);

    for(const auto& rcp2: steps[r_s].used_inputs)
    {
      int rc2 = rcp2.first;

      // Used regs should have back-references to R
      assert( result_is_used_by(r_s, rc2) );

      // Used computations should be mapped computation for the current token, if we are at the root
      int R2 = results[rc2].source_reg;
      assert(reg_is_changeable(R2));

      // The used result should be referenced somewhere more root-ward
      // so that this result can be invalidated, and the used result won't be GC-ed.
      assert(is_modifiable(access(R2).C.exp) or result_is_referenced(t,rc2));
      
      // Used results should have values
      assert(results[rc2].value);
    }


    // Below this we have a result.
    if (r_r <= 0) continue;

    assert(results[r_r].source_step == r_s);
    int value = result_value_for_reg_(t,r);

    if (results[r_r].flags.test(0))
      assert(is_root_token(t));

    if (value)
      assert(call);

    if (call and value == call)
      assert(access(call).type == reg::type_t::constant);

    if (call and value and access(call).type == reg::type_t::constant)
      assert(value == call);

    if (t != root_token) continue;

    // Regs with values should have back-references from their call.
    if (value and access(call).type != reg::type_t::constant)
    {
      assert( has_result(call) );
      int rc2 = result_index_for_reg(call);
      assert( result_is_called_by(r_r, rc2) );
    }

    // If we have a value, then our call should have a value
    if (value)
      assert(reg_has_value(call));
  }
}

void reg_heap::check_used_regs() const
{
  // check_used_regs
  for(auto r = begin(); r != end(); r++)
    check_used_reg( r.addr() );
}

// This routine should only be called by other routines.  It is not safe to call directly.
int reg_heap::remove_shared_step(int t, int r)
{
  if (is_root_token(t))
  {
    if (tokens[t].vm_step[r])
      return tokens[t].vm_step.erase_value(r);
    else
      return 0;
  }
  else
    return tokens[t].vm_step.set_value(r,-1);
}

// This routine should only be called by other routines.  It is not safe to call directly.
int reg_heap::remove_shared_result(int t, int r)
{
  if (is_root_token(t))
  {
    if (tokens[t].vm_result[r])
      return tokens[t].vm_result.erase_value(r);
    else
      return 0;
  }
  else
    return tokens[t].vm_result.set_value(r,-1);
}

/// Add a shared step at (t,r) -- assuming there isn't one already
int reg_heap::add_shared_step(int t, int r)
{
  assert(tokens[t].vm_step[r] <= 0);

  // 1. Get a new computation
  int s = steps.allocate();
  total_step_allocations++;
  
  // 2. Set the source of the computation
  steps[s].source_reg = r;

  // 3. Link it in to the mapping
  tokens[t].vm_step.set_value(r, s);

#if DEBUG_MACHINE >= 3
  check_used_reg(r);
#endif

  return s;
}

/// Add a shared result at (t,r) -- assuming there isn't one already
int reg_heap::add_shared_result(int t, int r, int s)
{
  assert(tokens[t].vm_result[r] <= 0);
  // There should already be a step, if there is a result
  assert(tokens[t].vm_step[r] > 0);

  // 1. Get a new result
  int rc = results.allocate();
  total_comp_allocations++;
  
  // 2. Set the source of the result
  results[rc].source_step = s;
  results[rc].source_reg = r;

  // 3. Link it in to the mapping
  tokens[t].vm_result.set_value(r, rc);

#if DEBUG_MACHINE >= 3
  check_used_reg(r);
#endif

  return rc;
}

void reg_heap::check_back_edges_cleared_for_step(int s)
{
  for(auto& rcp: steps.access_unused(s).used_inputs)
    assert(null(rcp.second));
}

void reg_heap::check_back_edges_cleared_for_result(int rc)
{
  assert(null(results.access_unused(rc).call_edge.second));
}

void reg_heap::clear_back_edges_for_reg(int /*r*/)
{
}

void reg_heap::clear_back_edges_for_step(int s)
{
  assert(s > 0);
  for(auto& rcp: steps[s].used_inputs)
  {
    results[rcp.first].used_by.erase(rcp.second);
    rcp.second = {};
  }
}

void reg_heap::clear_back_edges_for_result(int rc)
{
  assert(rc > 0);
  // FIXME! If there is a value, set, there should be a call_edge
  // FIXME! Should we unmap all values with no .. value/call_edge?
  int call = results[rc].call_edge.first;
  if (call)
  {
    assert(results[rc].value);
    auto back_edge = results[rc].call_edge.second;
    results[call].called_by.erase(back_edge);
    results[rc].call_edge = {};
  }
}

void reg_heap::clear_step(int t, int r)
{
  assert(not has_result_(t,r));
  int s = remove_shared_step(t,r);
  
  if (s > 0)
  {
#ifndef NDEBUG
    check_back_edges_cleared_for_step(s);
#endif
    steps.reclaim_used(s);
  }
}

void reg_heap::clear_result(int t, int r)
{
  int rc = remove_shared_result(t,r);
  
  if (rc > 0)
  {
#ifndef NDEBUG
    check_back_edges_cleared_for_result(rc);
#endif
    results.reclaim_used(rc);
  }
}

void reg_heap::release_tip_token(int t)
{
  assert(tokens[t].children.empty());
  assert(not tokens[t].is_referenced());

  total_destroy_token++;

  // 1. Clear flags of results in the root token before destroying the root token!
  if (is_root_token(t))
    for(auto p: tokens[root_token].vm_result.delta())
    {
      int rc = p.second;
      if (rc > 0 and results[rc].flags.test(0))
	dec_probability(rc);
    }

  // 2. Destroy computations in the token (this is an optimization)
  destroy_all_computations_in_token(t);

  // 3. Adjust the token tree
  int parent = parent_token(t);

  unused_tokens.push_back(t);
  
  if (parent != -1)
  {
    // mark token for this context unused
    assert(not is_root_token(t));
    assert(tokens.size() - unused_tokens.size() > 0);

    int index = remove_element(tokens[parent_token(t)].children, t);
    assert(index != -1);
    tokens[t].parent = -1;
  }
  else
  {
    assert(is_root_token(t));
    root_token = -1;
    assert(tokens.size() - unused_tokens.size() == 0);
  }

  // 4. Set the token to unused
  tokens[t].used = false;

  assert(tokens[t].vm_step.size() == size());
  assert(tokens[t].vm_result.size() == size());

  // 5. Make sure the token is empty
#ifdef DEBUG_MACHINE
  for(int i=0;i<size();i++)
  {
    assert(not tokens[t].vm_step[i]);
    assert(not tokens[t].vm_result[i]);
  }
#endif
}

void reg_heap::capture_parent_token(int t2)
{
  int t1 = parent_token(t2);
  assert(t1 != -1);

  int parent = parent_token(t1);
  assert(parent != -1);

  // make parent point to t2 instead of t1
  int index = replace_element(tokens[parent].children, t1, t2);
  assert(index != -1);

  // connect t2 to the parent and to t1
  tokens[t2].parent = parent;
  tokens[t2].children.push_back(t1);

  // token t1 is now a leaf token
  tokens[t1].parent = t2;
  index = remove_element(tokens[t1].children, t2);
  assert(index != -1);
}

void reg_heap::release_knuckle_token(int t)
{
  assert(token_is_used(t));
  assert(not tokens[t].is_referenced());
  assert(tokens[t].children.size() == 1);

  int child_token = tokens[t].children[0];

  if (is_root_token(t))
  {
    reroot_at(child_token);
    release_tip_token(t);
    return;
  }

  total_release_knuckle++;
  
  merge_split_mapping(t, child_token);

  capture_parent_token(child_token);

  assert(tokens[t].version <= tokens[child_token].version);

  release_tip_token(t);
}

void reg_heap::release_tip_token_and_ancestors(int t)
{
  assert(token_is_used(t));
  assert(tokens[t].children.empty());
  assert(not tokens[t].is_referenced());

  while(t != -1 and (not tokens[t].is_referenced()) and tokens[t].children.empty())
  {
    int parent = parent_token(t);

    // clear only the mappings that were actually updated here.
    release_tip_token(t);

    t = parent;
  }
}

bool reg_heap::is_terminal_token(int t) const
{
  assert(token_is_used(t));

  return tokens[t].children.empty();
}

bool reg_heap::is_root_token(int t) const
{
  assert(token_is_used(t));
  assert((t==root_token) == (tokens[t].parent == -1));

  return t == root_token;
}

int reg_heap::parent_token(int t) const
{
  return tokens[t].parent;
}

const vector<int>& reg_heap::children_of_token(int t) const
{
  return tokens[t].children;
}

int reg_heap::degree_of_token(int t) const
{
  int degree = children_of_token(t).size();
  if (not is_root_token(t))
    degree++;
  return degree;
}
  

bool reg_heap::token_is_used(int t) const
{
  assert(t >= 0 and t < tokens.size());
  return tokens[t].used;
}

int reg_heap::make_child_token(int t)
{
#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  assert(tokens[t].used);

  int t2 = get_unused_token();

  // assert(temp.empty());

  tokens[t2].triggers = tokens[t].triggers;

  // set parent relationship
  tokens[t2].parent = t;
  tokens[t2].children.clear();

  tokens[t].children.push_back(t2);

  tokens[t2].version = tokens[t].version;

  /*
    Only true for root token!
  for(int r: tokens[t].modified)
    if (access(r).re_evaluate)
      assert(reg_has_value(t2,r));
  */

  /*
  // use all the same computations and value.
  tokens[t2].modified = tokens[t].modified;
  tokens[t2].virtual_mapping = tokens[t].virtual_mapping;

  for(int r: tokens[t].modified)
  {
    assert(has_computation(t,r));
    assert(has_computation(t2,r));
  }
  */
#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  return t2;
}

int reg_heap::switch_to_child_token(int c)
{
  check_tokens();

  int t1 = token_for_context(c);
  int t2 = make_child_token(t1);
  unset_token_for_context(c);
  set_token_for_context(c,t2);

  check_tokens();

  return t2;
}

int interchange(int x, int t1, int t2)
{
  if (x == t1)
    return t2;
  else if (x == t2)
    return t1;
  else
    return x;
}

int reg_heap::get_n_contexts() const
{
  return token_for_context_.size();
}

int reg_heap::token_for_context(int c) const
{
  assert(c >= 0);
  return token_for_context_[c];
}

int reg_heap::unset_token_for_context(int c)
{
  int t = token_for_context(c);
  assert(t != -1);
  assert(tokens[t].is_referenced());

  token_for_context_[c] = -1;
  tokens[t].n_context_refs--;
  assert(tokens[t].n_context_refs >= 0);

  return t;
}

void reg_heap::set_token_for_context(int c, int t)
{
  assert(token_for_context(c) == -1);
  token_for_context_[c] = t;
  assert(tokens[t].n_context_refs >= 0);
  tokens[t].n_context_refs++;
  assert(tokens[t].is_referenced());
}

int reg_heap::copy_context(int c)
{
  check_tokens();

  int t = token_for_context(c);
  int c2 = get_new_context();
  set_token_for_context(c2,t);

  check_tokens();
  return c2;
}

int reg_heap::get_new_context()
{
  // Add an unused context if we are missing one
  if (unused_contexts.empty())
  {
    unused_contexts.push_back(get_n_contexts());
    token_for_context_.push_back(-1);
  }

  // Get a new context index and check it has no token
  int c = unused_contexts.back();
  unused_contexts.pop_back();
  assert(token_for_context(c) == -1);

  return c;
}

int reg_heap::get_unused_context()
{
  int c = get_new_context();
  
  set_token_for_context(c, get_unused_token());

  check_tokens();

  return c;
}

void reg_heap::release_context(int c)
{
  // release the reference to the token
  check_tokens();

  int t = unset_token_for_context(c);

  if ((not tokens[t].is_referenced()) and tokens[t].children.size() == 0)
    release_tip_token_and_ancestors(t);

  // Mark the context as unused
  token_for_context_[c] = -1;
  unused_contexts.push_back(c);

  check_tokens();
}

std::vector<int>& reg_heap::triggers_for_context(int c)
{
  reroot_at_context(c);
  return triggers();
}

bool reg_heap::reg_is_fully_up_to_date_in_context(int R, int c)
{
  reroot_at_context(c);
  return reg_is_fully_up_to_date(R);
}

bool reg_heap::reg_is_fully_up_to_date(int R) const
{
  // 1. Handle index_var nodes!
  int type = access(R).C.exp.head().type();
  if (type == index_var_type)
  {
    assert( not reg_is_changeable(R) );

    assert( not reg_has_value(R) );

    assert( not reg_has_call(R) );

    int index = access(R).C.exp.as_index_var();

    int R2 = access(R).C.lookup_in_env( index );

    return reg_is_fully_up_to_date(R2);
  }

  // 2. If we've never been evaluated OR we're not constant and have no value, then return false;
  if (not reg_has_value(R)) return false;

  const closure& value = access_value_for_reg(R);

  // NOTE! value cannot be an index_var.
  const expression_ref& E = value.exp;

  // Therefore, if the value is atomic, then R is up-to-date.
  if (not E.size()) return true;

  // If the value is a lambda function, then R is up-to-date.
  if (E.head().type() != constructor_type) return true;

  // If we get here, this had better be a constructor!
  assert(E.head().is_a<constructor>());

  // Check each component that is a index_var to see if its out of date.
  for(int i=0;i<E.size();i++)
  {
    int R2 = value.lookup_in_env( E.sub()[i].as_index_var() );
    
    if (not reg_is_fully_up_to_date(R2)) return false;
  }

  // All the components must be fully up-to-date, so R is fully up-to-date.
  return true;
}

const expression_ref& reg_heap::get_parameter_value_in_context(int p, int c)
{
  int& R = parameters[p].second;

  return get_reg_value_in_context(R, c);
}

const expression_ref& reg_heap::get_reg_value_in_context(int& R, int c)
{
  total_get_reg_value++;
  if (access(R).type == reg::type_t::constant) return access(R).C.exp;

  total_get_reg_value_non_const++;
  reroot_at_context(c);

  if (has_result(R))
  {
    total_get_reg_value_non_const_with_result++;
    int R2 = result_value_for_reg(R);
    if (R2) return access(R2).C.exp;
  }

  // If the value needs to be computed (e.g. its a call expression) then compute it.
  auto p = incremental_evaluate_in_context(R,c);
  R = p.first;
  int value = p.second;

  return access(value).C.exp;
}

void reg_heap::set_reg_value_in_context(int P, closure&& C, int c)
{
  int t = switch_to_child_token(c);

  set_reg_value(P, std::move(C), t);
}

pair<int,int> reg_heap::incremental_evaluate_in_context(int R, int c)
{
#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif

  reroot_at_context(c);
  mark_completely_dirty(root_token);
  auto p = incremental_evaluate(R);

#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif

  return p;
}

const closure& reg_heap::lazy_evaluate(int& R)
{
  mark_completely_dirty(root_token);
  auto p = incremental_evaluate(R);
  R = p.first;
  int value = p.second;
  return access(value).C;
}

const closure& reg_heap::lazy_evaluate(int& R, int c)
{
  auto p = incremental_evaluate_in_context(R,c);
  R = p.first;
  int value = p.second;
  return access(value).C;
}

const closure& reg_heap::lazy_evaluate_head(int index, int c)
{
  int R1 = heads[index];
  auto p = incremental_evaluate_in_context(R1,c);
  int R2 = p.first;
  int value = p.second;
  if (R2 != R1)
    set_head(index, R2);

  return access(value).C;
}

const closure& reg_heap::lazy_evaluate_unchangeable(int& R)
{
  R = incremental_evaluate_unchangeable(R);
  return access(R).C;
}

int reg_heap::get_modifiable_value_in_context(int R, int c)
{
  assert( access(R).C.exp.head().type() == modifiable_type);
  assert( reg_is_changeable(R) );

  reroot_at_context(c);

  return call_for_reg(R);
}

int reg_heap::add_identifier(const string& name)
{
  // if there's already an 's', then complain
  if (identifiers.count(name))
    throw myexception()<<"Cannot add identifier '"<<name<<"': there is already an identifier with that name.";

  int R = allocate();
  total_reg_allocations++;

  identifiers[name] = R;
  return R;
}

reg_heap::reg_heap(const module_loader& L)
  :base_pool_t(1),
   steps(1),
   results(1),
   P(new Program),
   loader(L)
{ 
  //  results.collect_garbage = [this](){collect_garbage();};
  steps.collect_garbage = [](){};
  results.collect_garbage = [](){};

#ifndef NDEBUG
  steps.clear_references = [this](int s){check_back_edges_cleared_for_step(s);};
  results.clear_references = [this](int rc){check_back_edges_cleared_for_step(rc);};
#endif
  steps.clear_references = [this](int){};
  results.clear_references = [this](int){};
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

closure let_float(closure&& C)
{
  C.exp = let_float(expression_ref(C.exp));
  C.exp = do_optimize_DCE(C.exp);
  return C;
}

closure graph_normalize(closure&& C)
{
  C.exp = graph_normalize(expression_ref(C.exp));
  return C;
}

closure indexify(closure&& C)
{
  C.exp = indexify(expression_ref(C.exp));
  return C;
}

closure trim_normalize(closure&& C)
{
  C.exp = trim_normalize(expression_ref(C.exp));
  return C;
}

closure resolve_refs(const vector<Module>& P, closure&& C)
{
  C.exp = resolve_refs(P, C.exp);
  return C;
}

closure reg_heap::preprocess(const closure& C)
{
  assert(C.exp);
  assert(let_float(C.exp).print() == let_float(let_float(C.exp)).print());
  //  return trim_normalize( indexify( Fun_normalize( graph_normalize( let_float( translate_refs( closure(C) ) ) ) ) ) );
  return trim_normalize( indexify( graph_normalize( let_float( translate_refs( resolve_refs(*P, closure(C) ) ) ) ) ) );
}

expression_ref reg_heap::translate_refs(const expression_ref& E, closure::Env_t& Env)
{
  int reg = -1;

  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (E.is_a<parameter>())
  {
    string name = E.as_<parameter>().parameter_name;
    string qualified_name = name;

    int param_index = find_parameter(qualified_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<name<<"') in expression!";

    reg = parameters[param_index].second;
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (E.is_a<identifier>())
  {
    string name = E.as_<identifier>().name;
    string qualified_name = name;
    assert(is_qualified_symbol(qualified_name) or is_haskell_builtin_con_name(qualified_name));
    auto loc = identifiers.find( qualified_name );
    if (loc == identifiers.end())
    {
      if (is_haskell_builtin_con_name(name))
      {
	symbol_info S = Module::lookup_builtin_symbol(name);
	add_identifier(S.name);
      
	// get the root for each identifier
	loc = identifiers.find(S.name);
	assert(loc != identifiers.end());
	
	int R = loc->second;
	
	assert(R != -1);
	set_C(R, preprocess(S.body) );
      }
      else
	throw myexception()<<"Can't translate undefined identifier '"<<name<<"' in expression!";
    }

    reg = loc->second;
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (E.is_a<reg_var>())
    reg = E.as_<reg_var>().target;

  if (reg != -1)
  {
    int index = Env.size();
    Env.insert(Env.begin(), reg);

    return index_var(index);
  }

  // Other constants have no parts, and don't need to be translated
  if (not E.size()) return E;

  // Translate the parts of the expression
  object_ptr<expression> V = E.as_expression().clone();
  for(int i=0;i<V->size();i++)
    V->sub[i] = translate_refs(V->sub[i], Env);

  return V;
}

closure reg_heap::translate_refs(closure&& C)
{
  closure C2 = C;
  C2.exp = translate_refs(C2.exp, C2.Env);
  return C2;
}

int reg_heap::find_parameter(const string& s) const
{
  for(int i=0;i<parameters.size();i++)
    if (parameters[i].first == s)
      return i;

  return -1;
}

const vector<int>& reg_heap::transition_kernels() const
{
  return transition_kernels_;
}

int reg_heap::add_transition_kernel(int r)
{
  int i = transition_kernels_.size();
  transition_kernels_.push_back(r);
  inc_heads(r);
  return i;
}

int reg_heap::add_parameter(const string& full_name)
{
  assert(full_name.size() != 0);

  // 1. Check that we don't already have a parameter with that name
  for(const auto& parameter: parameters)
    if (parameter.first == full_name)
      throw myexception()<<"A parameter with name '"<<full_name<<"' already exists - cannot add another one.";

  // 2. Allocate space for the parameter
  int r = allocate();
  parameters.push_back( {full_name, r} );
  inc_heads(r);

  // 3. Set its value to new_modifiable
  expression_ref E = identifier("new_modifiable");
  E = (identifier("unsafePerformIO"), E);
  E = (identifier("evaluate"),-1,E);

  set_C(r, preprocess( E ) );


  return r;
}
