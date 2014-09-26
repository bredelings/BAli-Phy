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
 * 5. Move call and used_inputs into computation_info
 *
 * 6. Make back-edges from computation_info to computations that use it.
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

template<typename T>
void truncate(vector<T>& v)
{
  vector<T> v2;
  v.swap(v2);
}

expression_ref graph_normalize(const expression_ref& E)
{
  if (not E) return E;

  // 1. Var
  // 5. (partial) Literal constant.  Treat as 0-arg constructor.
  if (not E.size()) return E;
  
  // 2. Lambda
  object_ptr<const lambda> L = is_a<lambda>(E);
  if (L)
  {
    assert(E.size() == 2);
    object_ptr<expression> V = E.clone_expression();
    V->sub[1] = graph_normalize(E.sub()[1]);

    if (V->sub[1].ptr() == E.sub()[1].ptr())
      return E;
    else
      return V;
  }

  // 6. Case
  object_ptr<const Case> IsCase = E.is_a<Case>();
  if (IsCase)
  {
    object_ptr<expression> V = E.clone_expression();

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

    object_ptr<expression> E2 = E.clone_expression();

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
	bodies.push_back( E.sub()[i] );

	// 2. Replace the argument expression with the let var.
	E2->sub[i] = var;
      }
    }

    return let_expression(vars, bodies, object_ptr<const expression>(E2));
  }

  // 5. Let 
  if (object_ptr<const let_obj> Let = is_a<let_obj>(E))
  {
    object_ptr<expression> V = E.clone_expression();

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

void computation::clear()
{
  source_token = -1;
  source_reg = -1;
  result = 0;
  call = 0;
  truncate(used_inputs);
  truncate(used_by);
  truncate(called_by);

  // This should already be cleared.
  assert(temp == -1);
  assert(flags == 0);
}

void computation::check_cleared()
{
  assert(not result);
  assert(not call);
  assert(used_inputs.empty());
  assert(called_by.empty());
  assert(used_by.empty());
  assert(temp == -1);
  assert(flags == 0);
}

computation& computation::operator=(computation&& R) noexcept
{
  result = R.result;
  source_token = R.source_token;
  source_reg = R.source_reg;
  call = R.call;
  used_inputs  = std::move( R.used_inputs );
  used_by = std::move( R.used_by );
  called_by = std::move( R.called_by );
  temp = R.temp;
  flags = R.flags;

  return *this;
}

computation::computation(computation&& R) noexcept
:source_token(R.source_token),
  source_reg(R.source_reg),
  result (R.result), 
  call ( R.call ),
  used_inputs ( std::move(R.used_inputs) ),
  used_by ( std::move( R.used_by) ),
  called_by ( std::move( R.called_by) ),
  temp ( R.temp ),
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

void reg_heap::register_probability(int r)
{
  mark_completely_dirty(root_token);
  r = incremental_evaluate(r);

  if (reg_is_constant(r))
  {
    log_double_t pr = *convert<const Log_Double>(access(r).C.exp.head().get());
    constant_pr *= pr;
  }
  else
  {
    assert(reg_is_changeable(r));

    int rc = computation_index_for_reg(r);
    assert(rc > 0);

    probability_heads.push_back(r);

    inc_heads(r);

    prs_list.push_back(r);
  }
}

int reg_heap::register_probability(closure&& C)
{
  int r = allocate();
  set_C(r, std::move(C));
  register_probability(r);
  return r;
}

bool reg_heap::inc_probability_for_reg(int r)
{
  assert(reg_is_changeable(r));
  int rc = computation_index_for_reg(r);

  if (rc > 0 and computations[rc].flags) return true; // already included

  incremental_evaluate(r);
  rc = computation_index_for_reg(r);

  return inc_probability(rc);
}

void reg_heap::dec_probability_for_reg(int r)
{
  int rc = computation_index_for_reg(r);

  if (rc > 0 and computations[rc].flags)
    dec_probability(rc);
}

bool reg_heap::inc_probability(int rc)
{
  assert(rc > 0);
  int r2 = computations[rc].result;
  assert(r2 > 0);
  log_double_t pr1 = *convert<const Log_Double>(access(r2).C.exp.head().get());
  log_double_t pr2 = pr1 / error_pr;

  log_double_t new_total = variable_pr * pr2;
  double error1 = ((new_total / variable_pr) / pr2).log();
  double error2 = ((new_total / pr2) / variable_pr).log();

  if (std::abs(error1) > 1.0e-8 or std::abs(error2) > 1.0e-8)
  {
    unhandled_pr *= pr1;
    return false;
  }

  total_error += std::abs(error1) + std::abs(error2);
  if (std::abs(error1) > std::abs(error2))
    error_pr.log() = error1;
  else
    error_pr.log() = error2;
  variable_pr = new_total;
  computations[rc].flags = 1;
  return true;
}

void reg_heap::dec_probability(int rc)
{
  assert(rc > 0);
  int r2 = computations[rc].result;
  assert(r2 > 0);
  log_double_t pr = *convert<const Log_Double>(access(r2).C.exp.head().get());

  variable_pr /= pr;
  computations[rc].flags = 0;

  int r = computations[rc].source_reg;
  assert(reg_is_changeable(r));
  assert(access(r).n_heads > 0);
  prs_list.push_back(r);
}

double id(double x) {return x;}

log_double_t reg_heap::probability_for_context_full(int c)
{
  /*
    This version doesn't really change the amount of time in incremental_evaluate.
    However, it drastically increases the amount of time spent in reg_has_result( 30% ),
    get_reg_value_in_context( 13% ), and probability_for_context( 3% ).

    With those removed, this could be comparable, or even faster.
  */

  double log_pr = 0.0;
  double C = 0.0;
  for(int r: probability_heads)
  {
    const auto& x = get_reg_value_in_context(r, c);
    log_double_t X = *convert<const Log_Double>(x.get());
    double y = X.log() - C;
    double t = log_pr + y;
    C = (t - log_pr) - y;
    log_pr = t;
  }
  log_double_t Pr;
  Pr.log() = log_pr;
  return Pr;
}

log_double_t reg_heap::probability_for_context_diff(int c)
{
  reroot_at_context(c);
  unhandled_pr.log() = 0.0;

  // re-multiply all probabilities
  if (total_error > 1.0e-9)
  {
    for(int r: probability_heads)
    {
      int rc = tokens[root_token].vm_relative[r];
      if (rc > 0 and computations[rc].flags)
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
  log_double_t Pr = probability_for_context_diff(c);
  // std::cerr<<"A:   Pr1 = "<<Pr<<"   error = "<<total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<variable_pr<<"  unhandled = "<<unhandled_pr<<std::endl;

#ifndef NDEBUG  
  log_double_t Pr2 = probability_for_context_full(c);
  double diff = Pr.log() - Pr2.log();
  // std::cerr<<"B:diff = "<<diff<<"    Pr1 = "<<Pr<<"  Pr2 = "<<Pr2<<"   error = "<<total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<variable_pr<<"  unhandled = "<<unhandled_pr<<std::endl;
  assert(fabs(diff) < 1.0e-6);
#endif

  return Pr;
}

const vector<int>& reg_heap::random_modifiables() const
{
  return random_modifiables_;
}

int reg_heap::add_random_modifiable(int index)
{
  int i = random_modifiables_.size();
  random_modifiables_.push_back(index);
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
    parameters[index].second = R2;

#ifndef NDEBUG
  if (not is_modifiable(access(R2).C.exp))
    throw myexception()<<"Parameter is not a modifiable!  Instead its value is '"<<access(R2).C.exp<<"'";
#endif

  assert(R2>0);
  return R2;
}

object_ref reg_heap::get_parameter_range(int c, int p)
{
  return get_range_for_reg(c, find_parameter_modifiable_reg(p));
}

object_ref reg_heap::get_range_for_reg(int c, int r)
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
  return *convert<const Double>(access(r3).C.exp.head());
}

const std::vector<int>& reg_heap::triggers() const {return tokens[root_token].triggers;}
      std::vector<int>& reg_heap::triggers()       {return tokens[root_token].triggers;}

int reg_heap::computation_index_for_reg(int r) const 
{
  return tokens[root_token].vm_relative[r];
}

const computation& reg_heap::computation_for_reg(int r) const 
{ 
  int rc = computation_index_for_reg(r);
  return computations.access_unused(rc);
}

computation& reg_heap::computation_for_reg(int r)
{ 
  int rc = computation_index_for_reg(r);
  return computations.access_unused(rc);
}

const closure& reg_heap::access_result_for_reg(int R1) const
{
  int R2 = result_for_reg(R1);
  assert(R2);
  return access(R2).C;
}

bool reg_heap::reg_has_result(int r) const
{
  if (access(r).type == reg::type_t::constant)
    return true;
  else
    return reg_has_computation_result(r);
}

bool reg_heap::reg_has_computation_result(int r) const
{
  return has_computation(r) and computation_result_for_reg(r);
}

bool reg_heap::reg_has_call(int r) const
{
  return has_computation(r) and call_for_reg(r);
}

int reg_heap::call_for_reg(int r) const
{
  return computation_for_reg(r).call;
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

bool reg_heap::has_computation(int r) const
{
  return computation_index_for_reg(r)>0;
}

bool reg_heap::has_computation_(int t, int r) const
{
  return computation_index_for_reg_(t,r)>0;
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
  return tokens[t].vm_relative[r];
}

int reg_heap::result_for_reg(int r) const 
{
  assert(not is_index_var(access(r).C.exp));
  if (access(r).type == reg::type_t::changeable)
    return computation_result_for_reg(r);
  else
  {
    assert(access(r).type == reg::type_t::constant);
    return r;
  }
}

int reg_heap::computation_result_for_reg(int r) const 
{
  return computation_for_reg(r).result;
}

int reg_heap::computation_result_for_reg_(int t, int r) const 
{
  return computation_for_reg_(t,r).result;
}

void reg_heap::set_computation_result_for_reg(int r1)
{
  int call = call_for_reg(r1);

  assert(call);

  int result = result_for_reg(call);

  assert(result);

  computation_for_reg(r1).result = result;

  // If R2 is WHNF then we are done
  if (access(call).type == reg::type_t::constant) return;

  // If R2 doesn't have a computation, add one to hold the called-by edge.
  assert(has_computation(call));

  int rc1 = computation_index_for_reg(r1);

  // Add a called-by edge to R2.
  computation_for_reg(call).called_by.push_back(computations.get_weak_ref(rc1));
}

void reg_heap::set_used_input(int R1, int R2)
{
  assert(reg_is_changeable(R1));
  assert(reg_is_changeable(R2));

  assert(is_used(R1));
  assert(is_used(R2));

  assert(access(R1).C);
  assert(access(R2).C);

  assert(has_computation(R1));
  assert(has_computation(R2));
  assert(computation_result_for_reg(R2));

  // An index_var's result only changes if the thing the index-var points to also changes.
  // So, we may as well forbid using an index_var as an input.
  assert(access(R2).C.exp.head()->type() != index_var_type);

  int rc1 = computation_index_for_reg(R1);
  int rc2 = computation_index_for_reg(R2);

  computations[rc1].used_inputs.push_back(rc2);
  computations[rc2].used_by.push_back(computations.get_weak_ref(rc1));

  assert(computation_is_used_by(rc1,rc2));
  assert(reg_is_used_by(R1,R2));
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
  assert(has_computation(R1));

  // Don't override an *existing* call
  assert(not reg_has_call(R1));

  // Check that we aren't overriding an existing *result*
  assert(not reg_has_result(R1));

  // Set the call
  int rc1 = computation_index_for_reg(R1);
  computations[rc1].call = R2;
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

void reg_heap::destroy_all_computations_in_token(int t)
{
  computations.inc_version();
  for(int r: tokens[t].vm_relative.modified())
  {
    int rc = tokens[t].vm_relative[r];
    if (rc > 0)
      computations.reclaim_used(rc);
  }
  tokens[t].vm_relative.clear();
}

void reg_heap::clear_call(int rc)
{
  computations.access_unused(rc).call = 0;
}

void reg_heap::clear_call_for_reg(int R)
{
  int rc = computation_index_for_reg(R);
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
  if (result.exp.head()->type() == index_var_type)
  {
    int index = convert<const index_var>(result.exp.head())->index;

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

// If we replace a computation at P that is newly defined in this token,
// there may be computations that call or use it that are also newly
// defined in this token.  Such computations must be cleared, because they
// do not use a value defined in a previous token, and so would not be detected
// as invalidate by invalidate_shared_regs( ), which can only detect computations
// as invalidate if they use a computation valid in a parent context.
//
// As a result, every computation that we invalidate is going to be newly defined
// in the current context.  Other computations can be invalidated later.

/// Update the value of a non-constant, non-computed index
void reg_heap::set_reg_value(int P, closure&& C, int token)
{
  assert(not is_dirty(token));
  assert(not children_of_token(token).size());
  assert(reg_is_changeable(P));

  if (not is_root_token(token) and tokens[token].version == tokens[parent_token(token)].version)
    tokens[token].version--;

  // assert(not is_root_token and tokens[token].version < tokens[parent_token(token)].version) 

  // Check that this reg is indeed settable
  assert(is_modifiable(access(P).C.exp));

  const int mark_result = 1;
  const int mark_call_result = 2;
  const int mark_modified = 3;

  vector< int >& call_and_result_may_be_changed = get_scratch_list();
  vector< int >& result_may_be_changed = get_scratch_list();
  vector< int >& regs_to_re_evaluate = tokens[token].regs_to_re_evaluate;

  // If we have a RELATIVE computation, we need to take care of its users new to this token.
  if (has_computation_(token,P))
  {
    call_and_result_may_be_changed.push_back(P);
    computation_for_reg_(token,P).temp = mark_modified;
    result_may_be_changed.push_back(P);
  }

  int i=0;
  int j=0;
  while(i < call_and_result_may_be_changed.size() or j < result_may_be_changed.size())
  {
    // First find all users or callers of regs where the result is out of date.
    find_callers(token, token, j, result_may_be_changed, result_may_be_changed, mark_result);
    find_users(token, token, j, result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    j = result_may_be_changed.size();

    // Second find all users or callers of regs where the result AND CALL are out of date.
    find_users(token, token, i, call_and_result_may_be_changed, call_and_result_may_be_changed, mark_call_result);
    find_callers(token, token, i, call_and_result_may_be_changed, result_may_be_changed, mark_result);
    i = call_and_result_may_be_changed.size();
  }

#ifndef NDEBUG
  for(int R: result_may_be_changed)
    assert(computation_for_reg_(token,R).temp == mark_result or 
	   computation_for_reg_(token,R).temp == mark_call_result or
	   computation_for_reg_(token,R).temp == mark_modified
	   );

  for(int R: call_and_result_may_be_changed)
    assert(computation_for_reg_(token,R).temp == mark_call_result or
	   computation_for_reg_(token,R).temp == mark_modified
	   );
#endif

  //  std::cerr<<" result: "<<result_may_be_changed.size()<<"\n";

  if (token == root_token)
  {
    for(int r: result_may_be_changed)
      dec_probability_for_reg(r);
    for(int r: call_and_result_may_be_changed)
      dec_probability_for_reg(r);
  }

  // Clear the marks: 1a
  for(int R: result_may_be_changed)
  {
    assert(has_computation_(token,R));

    //    assert(computation_result_for_reg(token,R) or reg_is_shared(token,R));

    auto& RC = computation_for_reg_(token,R);

    if (RC.temp > mark_result) continue;

    assert(reg_has_call_(token,R));

    RC.temp = -1;

    computation_for_reg_(token,R).result = 0;

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(R).re_evaluate)
      regs_to_re_evaluate.push_back(R);
  }

  computations.inc_version();
  // Clear the marks: 2a
  for(int R: call_and_result_may_be_changed)
  {
    assert(has_computation_(token,R));

    // Put this back when we stop making spurious used_by edges
    //    assert(reg_has_call(token,R));

    auto& RC = computation_for_reg_(token,R);

    if (RC.temp > mark_call_result) continue;

    assert(RC.temp == mark_call_result);

    RC.temp = -1;

    assert(not is_modifiable(access(R).C.exp));

    clear_computation(token,R);

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(R).re_evaluate)
      regs_to_re_evaluate.push_back(R);
  }

  if (has_computation_(token,P))
  {
    computation_for_reg_(token,P).temp = -1;
    clear_computation(token,P);
  }

  // Finally set the new value.
  add_shared_computation(token,P);
  set_reduction_result(token, P, std::move(C) );

  release_scratch_list();
  release_scratch_list();
  assert(n_active_scratch_lists == 0);

  if (token == root_token)
  {
    if (regs_to_re_evaluate.size())
      mark_completely_dirty(token);
    for(int R: regs_to_re_evaluate)
      incremental_evaluate(R);
    regs_to_re_evaluate.clear();
  }
#if DEBUG_MACHINE >= 2
  check_used_regs();
  check_tokens();
#endif
}

void reg_heap::set_shared_value(int r, int v)
{
  // add a new computation
  add_shared_computation(root_token, r);

  // set the value
  set_call(root_token, r, v);
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
bool reg_heap::merge_split_mapping(int t1, int t2)
{
  auto& vm1 = tokens[t1].vm_relative;
  auto& vm2 = tokens[t2].vm_relative;
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
	if (rc > 0)
	{
	  assert(computations[rc].source_token == t1);
	  computations[rc].source_token = t2;
	}
      }
      else
	i++;
    }
    return false;
  }
  else
  {
    for(int i=0;i<vm2.modified().size();)
    {
      int r = vm2.modified()[i];
      assert(vm2[r]);

      if (vm1[r])
      {
	int rc1 = vm1[r];
	int rc2 = vm2[r];
	vm1.replace_value(r,rc2);
	vm2.replace_value(r,rc1);
	if (rc1 > 0)
	{
	  assert(computations[rc1].source_token == t1);
	  computations[rc1].source_token = t2;
	}
	if (rc2 > 0)
	{
	  assert(computations[rc2].source_token == t2);
	  computations[rc2].source_token = t1;
	}

	i++;
      }
      else
      {
	int rc = vm2[r];
	vm1.add_value(r,rc);
	vm2.erase_value(r);
	if (rc > 0)
	{
	  assert(computations[rc].source_token == t2);
	  computations[rc].source_token = t1;
	}
      }
    }
    return true;
  }
}

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
void reg_heap::pivot_mapping(int t1, int t2)
{
  auto& vm1 = tokens[t1].vm_relative;
  auto& vm2 = tokens[t2].vm_relative;
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
    if (rc1 > 0)
    {
      assert(computations[rc1].source_token == t2);
      computations[rc1].source_token = t1;
    }
    vm2.set_value(r,rc2);
    if (rc2 > 0)
    {
      assert(computations[rc2].source_token == t1);
      computations[rc2].source_token = t2;
    }
  }
}

void reg_heap::reroot_at_context(int c)
{
  reroot_at(token_for_context(c));
}

void reg_heap::reroot_at(int t)
{
  if (is_root_token(t)) return;

#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  // 1. If this context isn't a direct child of the root, then make it one
  if (not is_root_token(parent_token(t)))
    reroot_at(parent_token(t));

  // re-rooting to the parent context shouldn't release its token.
  int parent = parent_token(t);
  assert(parent == root_token);

  // 2. Change the relative mappings
  pivot_mapping(parent, t);
  swap_tokens(parent,t);
  std::swap(parent,t);

  // 3. Alter the inheritance tree
  tokens[parent].parent = t;
  int index = remove_element(tokens[parent].children, t);
  assert(index != -1);

  tokens[t].parent = -1;
  tokens[t].children.push_back(parent);

  assert(t == root_token);

  // 4. Invalidate regs in t that reference(d) computations from parent
  assert(tokens[parent].version >= tokens[t].version);

  for(int r: tokens[parent].vm_relative.modified())
  {
    int rc = tokens[parent].vm_relative[r];
    if (rc > 0 and computations[rc].flags)
      dec_probability(rc);
  }

  invalidate_shared_regs(parent,t);

  // Mark this context as not having computations that need to be unshared
  tokens[t].version = tokens[parent].version;

  assert(tokens[t].version >= tokens[parent].version);

  for(int t2: tokens[t].children)
    assert(tokens[t2].version <= tokens[t].version);

  assert(is_root_token(t));

  // 5. re-evaluate all the regs that need to be up-to-date.
  if (tokens[t].regs_to_re_evaluate.size())
    mark_completely_dirty(t);
  for(int R: tokens[t].regs_to_re_evaluate)
    incremental_evaluate(R);
  tokens[t].regs_to_re_evaluate.clear();

  // 6. Now, try to remove the parent if its unreferenced.
  try_release_token(parent);
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

// Note that a context can be completely dirty, w/o being dirty :-P
bool reg_heap::is_completely_dirty(int t) const
{
  for(int t2:tokens[t].children)
    if (tokens[t].version <= tokens[t2].version)
      return false;
  return true;
}
  
// find regs in t2 that call values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_callers(int t1, int t2, int start, const vector<int>& split, vector<int>& callers, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg_(t1,split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.called_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source_reg;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg_(t2,r2) != rc2) continue;

      // Skip this one if its been marked high enough already
      if (RC2.temp >= mark) continue;

      // If the computation has no result, then its called-by edge is out-of-date
      if (not RC2.result) continue;

      // There (usually) shouldn't be a back edge to r2 if r2 has no result.
      // assert(RC2.result);

      RC2.temp = mark;
      assert(computation_index_for_reg_(t2,r2) == rc2);
      callers.push_back(r2);
    }
  }
}

// find regs in t2 that used values only active in t1.  We look at regs in split, and append results to callers
void reg_heap::find_users(int t1, int t2, int start, const vector<int>& split, vector<int>& users, int mark)
{
  for(int i=start;i<split.size();i++)
  {
    auto& RC1 = computation_for_reg_(t1, split[i]);

    // Look at computations in t2 that call the old value in t1.
    for(const auto& wrc2: clean_weak_refs(RC1.used_by, computations))
    {
      int rc2 = wrc2.get(computations);

      computation& RC2 = computations[rc2];
      int r2 = RC2.source_reg;

      // If this computation is not used in t2, we don't need to unshare it.
      if (computation_index_for_reg_(t2,r2) != rc2) continue;

      assert(not is_modifiable(access(r2).C.exp));

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

void reg_heap::invalidate_shared_regs(int t1, int t2)
{
  //  assert(t2 == parent_token(t1));
  assert(tokens[t1].version >= tokens[t2].version);

  if (tokens[t1].version <= tokens[t2].version) return;

  const int mark_result = 1;
  const int mark_call_result = 2;

  // find all regs in t2 that are not shared from t1
  vector<int>& modified = get_scratch_list();
  for(int r: tokens[t1].vm_relative.modified())
    if (tokens[t1].vm_relative[r] > 0)
      modified.push_back(r);

  vector< int >& call_and_result_may_be_changed = get_scratch_list();
  vector< int >& result_may_be_changed = get_scratch_list();
  vector< int >& regs_to_re_evaluate = tokens[t2].regs_to_re_evaluate;

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

  if (t2 == root_token)
  {
    for(int r: result_may_be_changed)
      dec_probability_for_reg(r);
    for(int r: call_and_result_may_be_changed)
      dec_probability_for_reg(r);
  }

  for(int r:result_may_be_changed)
  {
    int rc1 = computation_index_for_reg_(t2,r);
    auto& RC = computations[rc1];

    if (RC.temp > mark_result) continue;

    RC.temp = -1;

    if (not computation_index_for_reg_(t1,r))
    {
      // Move rc1 from t2 -> t1.
      move_computation(t2, t1, r);

      // Make a new computation in t2.
      int rc2 = add_shared_computation(t2,r);

      // Copy the computation-step part, but not the result
      duplicate_computation(rc1,rc2);
    }
    else
      RC.result = 0;

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);
  }

  for(int r:call_and_result_may_be_changed)
  {
    int rc2 = computation_index_for_reg_(t2,r);
    auto& RC = computations[rc2];

    assert(not is_modifiable(access(r).C.exp));

    if (RC.temp > mark_call_result) continue;

    RC.temp = -1;

    if (not computation_index_for_reg_(t1,r))
      move_computation(t2, t1, r);
    else
      clear_computation(t2, r);

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
  release_scratch_list();
  assert(n_active_scratch_lists == 0);
}

std::vector<int> reg_heap::used_regs_for_reg(int r) const
{
  vector<int> U;
  if (not has_computation(r)) return U;

  for(int rc: computation_for_reg(r).used_inputs)
    U.push_back(computations[rc].source_reg);

  return U;
}

void reg_heap::reclaim_used(int r)
{
  // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
  remove_from_used_list(r);

  for(int t=0;t<tokens.size();t++)
  {
    if (not token_is_used(t)) continue;

    if (tokens[t].vm_relative[r])
      tokens[t].vm_relative.erase_value(r);
  }

  for(int t=0;t<tokens.size();t++)
  {
    assert(not tokens[t].vm_relative[r]);
  }

  clear_C(r);

  add_to_free_list(r);
}

template <typename T>
void insert_at_end(vector<int>& v, const T& t)
{
  v.insert(v.end(), t.begin(), t.end());
}

void reg_heap::get_roots(vector<int>& scan, bool keep_identifiers) const
{
  insert_at_end(scan, temp);
  insert_at_end(scan, heads);
  insert_at_end(scan, probability_heads);
  insert_at_end(scan, random_modifiables_);
  insert_at_end(scan, transition_kernels_);
  for(int j=0;j<parameters.size();j++)
    scan.push_back(parameters[j].second);
  if (keep_identifiers)
    for(const auto& i: identifiers)
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

  heads.push_back(R);

  inc_heads(R);

  return R;
}

int reg_heap::push_temp_head()
{
  int R = allocate();

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
    assert(tokens[t].vm_relative.size() == old_size);

  base_pool_t::expand_memory(s);

  // Extend virtual mappings, with virtual_mapping[i] = 0;
  for(int t=0;t<tokens.size();t++)
  {
    tokens[t].vm_relative.resize(size());
    for(int i=old_size;i<size();i++)
      assert(tokens[t].vm_relative[i] == 0);
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
    tokens.back().vm_relative.resize(size());
    for(int i=0;i<size();i++)
      assert(tokens.back().vm_relative[i] == 0);
  }

  for(int i=0;i<tokens.size();i++)
    assert(tokens[i].vm_relative.size() == size());

  int t = unused_tokens.back();
  unused_tokens.pop_back();

  assert(not token_is_used(t));

  tokens[t].used = true;

  assert(tokens[t].parent == -1);
  assert(tokens[t].children.empty());
  assert(tokens[t].vm_relative.empty());
  assert(not tokens[t].referenced);

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

bool reg_heap::reg_is_used_by(int r1, int r2) const
{
  int rc1 = computation_index_for_reg(r1);
  int rc2 = computation_index_for_reg(r2);

  return computation_is_used_by(rc1,rc2);
}

bool reg_heap::computation_is_referenced(int t,int rc) const
{
  assert(rc);
  int r = computations[rc].source_reg;
  if (computation_index_for_reg_(t,r) == rc) return true;
  int p = parent_token(t);
  if (p != -1)
    return computation_is_referenced(p, rc);
  else
    return false;
}

void reg_heap::check_tokens() const
{
  for(int c=0;c<get_n_contexts();c++)
  {
    int t = token_for_context(c);
    if (t >= 0)
    {
      assert(tokens[t].referenced);
      assert(tokens[t].used);
    }
  }

  for(int t=0;t<tokens.size();t++)
    if (token_is_used(t))
    {
      assert(tokens[t].referenced or tokens[t].children.size() > 1);
      for(int t2: children_of_token(t))
	assert(tokens[t].version >= tokens[t2].version);
    }

}

void reg_heap::check_used_reg(int index) const
{
  for(int t=0;t<get_n_tokens();t++)
  {
    if (not token_is_used(t)) continue;

    if (is_root_token(t))
      assert(tokens[t].vm_relative[index] != -1);

    if (not is_root_token(t) and tokens[t].vm_relative[index] > 0 and tokens[parent_token(t)].vm_relative[index] > 0)
      assert(tokens[t].vm_relative[index] != tokens[parent_token(t)].vm_relative[index]);

    if (access(index).type == reg::type_t::constant)
      assert(not has_computation_(t,index));

    if (not has_computation_(t, index)) continue;

    int call = call_for_reg_(t,index);
    int result = computation_result_for_reg_(t,index);

    if (computations[computation_index_for_reg_(t,index)].flags)
      assert(is_root_token(t));

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
      int R2 = computations[rc2].source_reg;
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
      assert( has_computation(call) );
      int rc2 = computation_index_for_reg(call);
      assert( computation_is_called_by(index_c, rc2) );
    }

    // If we have a result, then our call should have a result
    if (result)
      assert(reg_has_result(call));
  }
}

void reg_heap::check_used_regs() const
{
  // check_used_regs
  for(auto r = begin(); r != end(); r++)
    check_used_reg( r.addr() );
}

// This routine should only be called by other routines.  It is not safe to call directly.
int reg_heap::remove_shared_computation(int t, int r)
{
  if (is_root_token(t))
    return tokens[t].vm_relative.erase_value(r);
  else
    return tokens[t].vm_relative.set_value(r,-1);
}

int reg_heap::move_computation(int t1, int t2, int r)
{
  int rc = computation_index_for_reg_(t1, r);
  computations[rc].source_token = t2;
  tokens[t2].vm_relative.add_value(r, rc);

  remove_shared_computation(t1,r);
  return rc;
}

void reg_heap::duplicate_computation(int rc1, int rc2) const
{
  assert(not computations[rc2].call);
  computations[rc2].call = computations[rc1].call;
  computations[rc2].used_inputs = computations[rc1].used_inputs;

  // set back-edges for used inputs
  for(int rcu: computations[rc2].used_inputs)
    computations[rcu].used_by.push_back(computations.get_weak_ref(rc2));
}

/// Add a shared computation at (t,r) -- assuming there isn't one already
int reg_heap::add_shared_computation(int t, int r)
{
  assert(tokens[t].vm_relative[r] <= 0);

  // 1. Get a new computation
  int rc = computations.allocate();

  // 2. Set the source of the computation
  computations[rc].source_token = t;
  computations[rc].source_reg = r;

  // 3. Link it in to the mapping
  tokens[t].vm_relative.set_value(r, rc);

#if DEBUG_MACHINE >= 3
  check_used_reg(r);
#endif

  return rc;
}

void reg_heap::clear_computation(int t, int r)
{
  int rc = remove_shared_computation(t,r);

  if (rc > 0)
  {
    computations.inc_version();
    computations.reclaim_used(rc);
  }
}

void reg_heap::release_child_token(int t)
{
  // clear flags of computations in the root token before destroying the root token!
  if (t == root_token)
    for(int r: tokens[root_token].vm_relative.modified())
    {
      int rc = tokens[root_token].vm_relative[r];
      if (rc > 0 and computations[rc].flags)
	dec_probability(rc);
    }

  destroy_all_computations_in_token(t);

  int parent = parent_token(t);

  assert(tokens[t].children.empty());
  assert(not tokens[t].referenced);

  tokens[t].used = false;
  
  if (parent != -1)
  {
    // mark token for this context unused
    int index = remove_element(tokens[parent_token(t)].children, t);
    assert(index != -1);
    tokens[t].parent = -1;
  }

  unused_tokens.push_back(t);
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

void reg_heap::try_release_token(int t)
{
  assert(token_is_used(t));

  // We shouldn't have any temporary heads still on the stack, here!
  // (This should be fast now, no longer proportional to the number of regs in context t.)
  // (But how fast is it?)

  // FIXME: we can have temp heads here from performing an IO action from outside...
  //  assert(temp.empty());

  int parent = parent_token(t);

  // Check invariant that any context must be either referenced or have more than 1 child context.
  if (parent != -1)
    assert(tokens[parent].referenced or tokens[parent].children.size() > 1);

  int n_children = tokens[t].children.size();
  if (n_children > 1 or tokens[t].referenced)
    return;

  if (n_children)
  {
    int child_token = tokens[t].children[0];

    // handle the case when we are trying to release the root
    if (is_root_token(t))
    {
      reroot_at(child_token);
      return;
    }

    if (merge_split_mapping(t, child_token))
    {
      swap_tokens(t, child_token);
      std::swap(t, child_token);
    }

    capture_parent_token(child_token);

    invalidate_shared_regs(t, child_token);
  }

  // clear only the mappings that were actually updated here.
  release_child_token(t);

  // If we just released a terminal token, maybe it's parent is not terminal also.
  if (parent != -1)
    try_release_token(parent);

  // The -1 accounts for the unused token 0.
  if (tokens.size() - unused_tokens.size() -1 > 0)
    assert(root_token != -1);

#ifdef DEBUG_MACHINE
  assert(tokens[t].vm_relative.size() == size());
  for(int i=0;i<size();i++)
    assert(not tokens[t].vm_relative[i]);

  check_used_regs();
#endif
}

bool reg_heap::is_terminal_token(int t) const
{
  assert(token_is_used(t));

  return tokens[t].children.empty();
}

bool reg_heap::is_root_token(int t) const
{
  assert((t==root_token) == (tokens[t].parent == -1));
  assert(token_is_used(t));

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
  return tokens[t].used;
}

int reg_heap::copy_token(int t)
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
      assert(reg_has_result(t2,r));
  */

  /*
  // use all the same computations and result.
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
  int t2 = copy_token(t1);
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

void reg_heap::swap_tokens(int t1, int t2)
{
  // 1. Check that tokens are active parent and child
  assert(tokens[t1].used);
  assert(tokens[t2].used);
  assert(t1 == tokens[t2].parent);

  // 2. Switch the contexts of the two tokens
  for(int& t: token_for_context_)
    t = interchange(t, t1, t2);

  if (tokens[t1].parent == t2)
    std::swap(t1,t2);

  // 3. Switch parent relationships
  std::swap(tokens[t1].children, tokens[t2].children);
  std::swap(tokens[t1].parent, tokens[t2].parent);

  if (tokens[t1].parent == t1)
  {
    tokens[t1].parent = t2;

    replace_element(tokens[t2].children, t2, t1);
  }

  {
    for(int t: tokens[t1].children)
      tokens[t].parent = t1;

    for(int t: tokens[t2].children)
      tokens[t].parent = t2;

    int p1 = tokens[t1].parent;
    if (p1 != -1)
      replace_element(tokens[p1].children, t2, t1);

    int p2 = tokens[t2].parent;
    if (p2 != -1)
      replace_element(tokens[p2].children, t1, t2);
  }

  // 4. Switch other fields
  std::swap(tokens[t1].triggers, tokens[t2].triggers);
  std::swap(tokens[t1].referenced, tokens[t2].referenced);
  std::swap(tokens[t1].version, tokens[t2].version);
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
  assert(tokens[t].referenced);

  token_for_context_[c] = -1;
  tokens[t].referenced = false;

  return t;
}

void reg_heap::set_token_for_context(int c, int t)
{
  assert(token_for_context(c) == -1);
  token_for_context_[c] = t;
  assert(not tokens[t].referenced);
  tokens[t].referenced = true;
}

int reg_heap::copy_context(int c)
{
  check_tokens();

  int t1 = token_for_context(c);

  int c2 = get_new_context();
  int t2 = copy_token(t1);
  set_token_for_context(c2,t2);

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

  try_release_token(t);

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
  int type = access(R).C.exp.head()->type();
  if (type == index_var_type)
  {
    assert( not reg_is_changeable(R) );

    assert( not reg_has_result(R) );

    assert( not reg_has_call(R) );

    int index = assert_is_a<index_var>(access(R).C.exp)->index;

    int R2 = access(R).C.lookup_in_env( index );

    return reg_is_fully_up_to_date(R2);
  }

  // 2. If we've never been evaluated OR we're not constant and have no result, then return false;
  if (not reg_has_result(R)) return false;

  const closure& result = access_result_for_reg(R);

  // NOTE! result cannot be an index_var.
  const expression_ref& E = result.exp;

  // Therefore, if the result is atomic, then R is up-to-date.
  if (not E.size()) return true;

  // If the result is a lambda function, then R is up-to-date.
  if (E.head()->type() != constructor_type) return true;

  // If we get here, this had better be a constructor!
  assert(is_a<constructor>(E));

  // Check each component that is a index_var to see if its out of date.
  for(int i=0;i<E.size();i++)
  {
    // assert_cast
    object_ptr<const index_var> V = assert_is_a<index_var>(E.sub()[i]);
    int R2 = result.lookup_in_env( V->index );
    
    if (not reg_is_fully_up_to_date(R2)) return false;
  }

  // All the components must be fully up-to-date, so R is fully up-to-date.
  return true;
}

object_ref reg_heap::get_parameter_value_in_context(int p, int c)
{
  int& R = parameters[p].second;

  return get_reg_value_in_context(R, c);
}

const object_ptr<const Object>& reg_heap::get_reg_value_in_context(int& R, int c)
{
  if (access(R).type == reg::type_t::constant) return access(R).C.exp.head();

  reroot_at_context(c);

  if (has_computation(R))
  {
    int R2 = computation_result_for_reg(R);
    if (R2) return access(R2).C.exp.head();
  }

  // If there's no result AND there's no call, then the result simply hasn't be set, so return NULL.
  if (is_modifiable(access(R).C.exp) and not reg_has_call(R)) return object_ref();

  // If the value needs to be computed (e.g. its a call expression) then compute it.
  R = incremental_evaluate_in_context(R,c);

  return access_result_for_reg(R).exp.head();
}

void reg_heap::set_reg_value_in_context(int P, closure&& C, int c)
{
  int t = token_for_context(c);
  if (not children_of_token(t).empty())
    t = switch_to_child_token(c);

  set_reg_value(P, std::move(C), t);
}

int reg_heap::incremental_evaluate_in_context(int R, int c)
{
#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif

  reroot_at_context(c);
  mark_completely_dirty(root_token);
  R = incremental_evaluate(R);

#if DEBUG_MACHINE >= 2
  check_used_regs();
#endif

  return R;
}

const closure& reg_heap::lazy_evaluate(int& R)
{
  mark_completely_dirty(root_token);
  R = incremental_evaluate(R);
  return access_result_for_reg(R);
}

const closure& reg_heap::lazy_evaluate(int& R, int c)
{
  R = incremental_evaluate_in_context(R,c);
  return access_result_for_reg(R);
}

const closure& reg_heap::lazy_evaluate_head(int index, int c)
{
  int R1 = heads[index];
  int R2 = incremental_evaluate_in_context(R1,c);
  if (R2 != R1)
    set_head(index, R2);

  return access_result_for_reg(R2);
}

const closure& reg_heap::lazy_evaluate_unchangeable(int& R)
{
  R = incremental_evaluate_unchangeable(R);
  return access(R).C;
}

int reg_heap::get_modifiable_value_in_context(int R, int c)
{
  assert( access(R).C.exp.head()->type() == modifiable_type);
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

  identifiers[name] = R;
  return R;
}

reg_heap::reg_heap()
  :base_pool_t(1),
   computations(1),
   P(new Program)
{ 
  //  computations.collect_garbage = [this](){collect_garbage();};
  computations.collect_garbage = [](){};
  computations.clear_references = [](int){};
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

expression_ref reg_heap::translate_refs(const expression_ref& E, vector<int>& Env)
{
  int reg = -1;

  // Replace parameters with the appropriate reg_var: of value parameter( )
  if (object_ptr<const parameter> p = is_a<parameter>(E))
  {
    string qualified_name = p->parameter_name;

    int param_index = find_parameter(qualified_name);
    
    if (param_index == -1)
      throw myexception()<<"Can't translate undefined parameter '"<<qualified_name<<"' ('"<<p->parameter_name<<"') in expression!";

    reg = parameters[param_index].second;
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const identifier> V = is_a<identifier>(E))
  {
    string qualified_name = V->name;
    assert(is_qualified_symbol(qualified_name) or is_haskell_builtin_con_name(qualified_name));
    auto loc = identifiers.find( qualified_name );
    if (loc == identifiers.end())
    {
      if (is_haskell_builtin_con_name(V->name))
      {
	symbol_info S = Module::lookup_builtin_symbol(V->name);
	add_identifier(S.name);
      
	// get the root for each identifier
	loc = identifiers.find(S.name);
	assert(loc != identifiers.end());
	
	int R = loc->second;
	
	assert(R != -1);
	set_C(R, preprocess(S.body) );
      }
      else
	throw myexception()<<"Can't translate undefined identifier '"<<V->name<<"' in expression!";
    }

    reg = loc->second;
  }

  // Replace parameters with the appropriate reg_var: of value whatever
  if (object_ptr<const reg_var> RV = is_a<reg_var>(E))
    reg = RV->target;

  if (reg != -1)
  {
    int index = Env.size();
    Env.insert(Env.begin(), reg);

    return new index_var(index);
  }

  // Other constants have no parts, and don't need to be translated
  if (not E.size()) return E;

  // Translate the parts of the expression
  object_ptr<expression> V = E.clone_expression();
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
  return i;
}
