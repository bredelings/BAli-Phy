#include <iostream>
#include <algorithm>
#include "util.H"
#include "graph_register.H"
#include "computation/expression/expression.H"
#include "computation/expression/var.H"

using std::string;
using std::vector;
using std::pair;

using std::cerr;
using std::endl;

using boost::optional;

long total_reg_allocations = 0;
long total_step_allocations = 0;
long total_comp_allocations = 0;
long total_set_reg_value = 0;
long total_get_reg_value = 0;
long total_get_reg_value_non_const = 0;
long total_get_reg_value_non_const_with_result = 0;
long total_context_pr = 0;
long total_tokens = 0;
long max_version = 0;

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

void Step::clear()
{
    source_reg = -1;
    call = 0;
    truncate(used_inputs);
    assert(created_regs.empty());

    // This should already be cleared.
    assert(flags.none());
}

void Step::check_cleared()
{
    assert(not call);
    assert(used_inputs.empty());
    assert(created_regs.empty());
    assert(flags.none());
}

Step& Step::operator=(Step&& S) noexcept
{
    source_reg = S.source_reg;
    call = S.call;
    used_inputs  = std::move( S.used_inputs );
    created_regs  = std::move( S.created_regs );
    flags = S.flags;

    return *this;
}

Step::Step(Step&& S) noexcept
:source_reg( S.source_reg),
			 call ( S.call ),
			 used_inputs ( std::move(S.used_inputs) ),
			 created_regs ( std::move(S.created_regs) ),
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

    type = R.type;

    n_heads = R.n_heads;

    created_by = std::move(R.created_by);

    return *this;
}

reg::reg(reg&& R) noexcept
:C( std::move(R.C) ),
			 type ( R.type ),
			 n_heads( R.n_heads ),
			 created_by( std::move(R.created_by) )
{ }

void reg::clear()
{
    assert(n_heads == 0);
    C.clear();
    type = type_t::unknown;
}

void reg::check_cleared()
{
    assert(not C);
    assert(type == type_t::unknown);
    assert(n_heads == 0);
    assert(created_by.first == 0);
    assert(created_by.second == 0);
}

void mapping::add_value(int r, int v) 
{
    assert(v);

    delta_.emplace_back(r,v);
}

int mapping::erase_value_at(int index)
{
    auto back = delta_.back();
    delta_.pop_back();

    // If we are deleting from the middle, move the last element to the middle
    if (index < delta_.size())
	delta_[index] = back;

    return back.second;
}

void mapping::clear()
{
    delta_.clear();
}

void mapping::resize(int s)
{
    delta_.reserve(s);
}

bool mapping::empty() const
{
    return delta_.empty();
}

boost::optional<int> reg_heap::creator_of_reg(int r) const
{
    int s = regs[r].created_by.first;
    assert(s >= 0);
    if (s == 0)
	return boost::none;
    else
	return s;
}

bool reg_heap::reg_is_contingent(int r) const
{
    return (bool)creator_of_reg(r);
}

bool reg_heap::step_exists_in_root(int s) const
{
    assert(s > 0);
    int r = steps[s].source_reg;
    assert(r > 0 and r < size());
    return prog_steps[r] == s;
}

bool reg_heap::reg_exists(int r) const
{
    auto s = creator_of_reg(r);
    if (not s)
	return true;
    else
	return step_exists_in_root(*s);
}

size_t reg_heap::size() const
{
    assert(regs.size() == prog_steps.size());
    assert(regs.size() == prog_results.size());
    assert(regs.size() == prog_temp.size());
    return regs.size();
}

void reg_heap::register_probability(int r)
{
    mark_completely_dirty(root_token);
    r = incremental_evaluate(r).first;

    if (reg_is_constant(r))
    {
	log_double_t pr = regs.access(r).C.exp.as_log_double();
	constant_pr *= pr;
    }
    else
    {
	assert(reg_is_changeable(r));

	int rc = result_index_for_reg(r);
	assert(rc > 0);

	probability_heads.push_back(r);

	prs_list.push_back(r);
    }
}

int reg_heap::register_probability(closure&& C)
{
    assert(not C.exp.head().is_a<expression>());

    int r = allocate();
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
    log_double_t pr = regs.access(r2).C.exp.as_log_double();

    // This value has already by included, so take it out unconditionally.
    prior.data.value -= pr.log();

    results[rc].flags.reset(0);

    int r = results[rc].source_reg;
    assert(reg_is_changeable(r));
    prs_list.push_back(r);
}

double id(double x) {return x;}

log_double_t reg_heap::probability_for_context_diff(int c)
{
    reroot_at_context(c);
    prior.reset_unhandled();

    // re-multiply all probabilities
    if (prior.data.total_error > 1.0e-9)
    {
	for(int r: probability_heads)
	{
	    int rc = result_index_for_reg(r);
	    if (rc > 0 and results[rc].flags.test(0))
		dec_probability(rc);
	}
	// std::cerr<<"unwinding all prs: total_error = "<<prior.data.total_error<<" variable_pr = "<<prior.data.value<<"  error_pr = "<<prior.data.delta<<"   variable_pr/error_pr = "<<prior.data.value - prior.data.delta<<std::endl;
	assert(std::abs(prior.data.value - prior.data.delta) < 1.0e-6);
	assert(prs_list.size() == probability_heads.size());
	prior.reset();
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

    return constant_pr * log_double_t(prior);
}

log_double_t reg_heap::probability_for_context(int c)
{
    total_context_pr++;

    log_double_t Pr = probability_for_context_diff(c);
    // std::cerr<<"A:   Pr1 = "<<Pr<<"   error = "<<prior.data.total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<prior.data.value<<"  unhandled = "<<prior.data.unhandled<<std::endl;

#ifndef NDEBUG  
    // log_double_t Pr2 = probability_for_context_full(c);
    // double diff = Pr.log() - Pr2.log();
    // std::cerr<<"B:diff = "<<diff<<"    Pr1 = "<<Pr<<"  Pr2 = "<<Pr2<<"   error = "<<prior.data.total_error<<"  constant_pr = "<<constant_pr<<"  variable_pr = "<<prior.data.value<<"  unhandled = "<<prior.data.unhandled<<std::endl;
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
    return i;
}

optional<int> reg_heap::parameter_is_modifiable_reg(int index)
{
    int& R = parameters[index].second;

    if (find_modifiable_reg(R))
	return R;
    else
	return boost::none;
}

int reg_heap::parameter_as_modifiable_reg(int index)
{
    auto R = parameter_is_modifiable_reg(index);
    if (R)
	return *R;
    else
	throw myexception()<<"Parameter '"<<parameters[index].first<<"' is not modifiable!";
}

optional<int> reg_heap::compute_expression_is_modifiable_reg(int index)
{
    int& H = heads[index];

    if (find_modifiable_reg(H))
	return H;
    else
	return boost::none;
}

int reg_heap::compute_expression_as_modifiable_reg(int index)
{
    auto R = compute_expression_is_modifiable_reg(index);
    if (R)
	return *R;
    else
	throw myexception()<<"Compute expression '"<<index<<"' is not modifiable!";
}

bool reg_heap::find_modifiable_reg(int& R)
{
    // Note: here we always update R
    R = incremental_evaluate_unchangeable(R);

    return is_modifiable(regs.access(R).C.exp);
}

const expression_ref reg_heap::get_parameter_range(int c, int p)
{
    return get_range_for_reg(c, parameter_as_modifiable_reg(p));
}

const expression_ref reg_heap::get_range_for_reg(int c, int r)
{
    if (regs.access(r).C.Env.size() < 3)
	return {};

    int r2 = regs.access(r).C.lookup_in_env(2);
    return get_reg_value_in_context(r2,c);
}

double reg_heap::get_rate_for_reg(int r)
{
    if (regs.access(r).C.Env.size() < 3)
	return {};

    int r3 = regs.access(r).C.lookup_in_env(0);
    r3 = incremental_evaluate_unchangeable(r3);
    return regs.access(r3).C.exp.as_double();
}

int reg_heap::step_index_for_reg(int r) const 
{
    assert(prog_steps[r] > 0);
    return prog_steps[r];
}

int reg_heap::result_index_for_reg(int r) const 
{
    assert(prog_results[r] > 0);
    return prog_results[r];
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
    return regs.access(R2).C;
}

bool reg_heap::reg_has_value(int r) const
{
    if (regs.access(r).type == reg::type_t::constant)
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

bool reg_heap::has_step(int r) const
{
    return step_index_for_reg(r)>0;
}

bool reg_heap::has_result(int r) const
{
    return result_index_for_reg(r)>0;
}

int reg_heap::value_for_reg(int r) const 
{
    assert(not regs.access(r).C.exp.is_index_var());
    if (regs.access(r).type == reg::type_t::changeable)
	return result_value_for_reg(r);
    else
    {
	assert(regs.access(r).type == reg::type_t::constant);
	return r;
    }
}

int reg_heap::result_value_for_reg(int r) const 
{
    return result_for_reg(r).value;
}

void reg_heap::set_result_value_for_reg(int r1)
{
    int call = call_for_reg(r1);

    assert(call);

    int value = value_for_reg(call);

    assert(value);

    int rc1 = result_index_for_reg(r1);
    if (rc1 < 0)
	rc1 = add_shared_result(r1, step_index_for_reg(r1));
    assert(rc1 > 0);
    auto& RC1 = results[rc1];
    RC1.value = value;

    // If R2 is WHNF then we are done
    if (regs.access(call).type == reg::type_t::constant) return;

    // If R2 doesn't have a result, add one to hold the called-by edge.
    assert(has_result(call));

    // Add a called-by edge to R2.
    int rc2 = result_index_for_reg(call);
    int back_index = results[rc2].called_by.size();
    results[rc2].called_by.push_back(rc1);
    RC1.call_edge = {rc2, back_index};
}

void reg_heap::set_used_input(int s1, int R2)
{
    assert(reg_is_changeable(R2));

    assert(regs.is_used(R2));

    assert(regs.access(R2).C);

    assert(has_result(R2));
    assert(result_value_for_reg(R2));

    // An index_var's value only changes if the thing the index-var points to also changes.
    // So, we may as well forbid using an index_var as an input.
    assert(regs.access(R2).C.exp.head().type() != index_var_type);

    int rc2 = result_index_for_reg(R2);

    int back_index = results[rc2].used_by.size();
    int forw_index = steps[s1].used_inputs.size();
    results[rc2].used_by.push_back({s1,forw_index});
    steps[s1].used_inputs.push_back({rc2,back_index});

    assert(result_is_used_by(s1,rc2));
}

void reg_heap::set_call(int R1, int R2)
{
    assert(reg_is_changeable(R1));
    // R2 might be of UNKNOWN changeableness

    // Check that R1 is legal
    assert(regs.is_used(R1));

    // Check that R2 is legal
    assert(regs.is_used(R2));

    // Only modify the call for the current context;
    assert(has_step(R1));

    // Don't override an *existing* call
    assert(not reg_has_call(R1));

    // Check that we aren't overriding an existing *value*
    assert(not reg_has_value(R1));

    // Set the call
    step_for_reg(R1).call = R2;
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

    regs.access(R).C = std::move(C);
#ifndef NDEBUG
    for(int r: regs.access(R).C.Env)
	assert(regs.is_valid_address(r));
#endif
}

void reg_heap::clear_C(int R)
{
    truncate(regs.access_unused(R).C);
}

void reg_heap::mark_reg_created_by_step(int r, int s)
{
    assert(r > 0);
    assert(s > 0);

    int index = steps[s].created_regs.size();
    steps[s].created_regs.push_back(r);
    assert(regs.access(r).created_by.first == 0);
    assert(regs.access(r).created_by.second == 0);
    regs.access(r).created_by = {s,index};
}

int reg_heap::allocate()
{
    total_reg_allocations++;
    return regs.allocate();
}

int reg_heap::allocate_reg_from_step(int s)
{
    int r = allocate();
    mark_reg_created_by_step(r,s);
    assert(not has_step(r));
    return r;
}

int reg_heap::allocate_reg_from_step(int s, closure&& C)
{
    int r = allocate_reg_from_step(s);
    set_C(r, std::move(C));
    return r;
}

int reg_heap::allocate_reg_from_step_in_token(int s, int t)
{
    int r = allocate_reg_from_step(s);
    tokens[t].vm_result.add_value(r, non_computed_index);
    tokens[t].vm_step.add_value(r, non_computed_index);
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
    assert(is_modifiable(regs.access(R).C.exp));

    assert(not is_root_token(t));

    // Finally set the new value.
    int s = get_shared_step(R);

    assert(tokens[t].vm_step.empty());
    tokens[t].vm_step.add_value(R,s);

    assert(tokens[t].vm_result.empty());
    tokens[t].vm_result.add_value(R, non_computed_index);

    assert(not children_of_token(t).size());

    // if the value is NULL, just leave the value and call both unset.
    //  (this could happen if we set a parameter value to null.)
    if (not value) return;

    // If the value is a pre-existing reg_var, then call it.
    if (value.exp.head().type() == index_var_type)
    {
	int index = value.exp.as_index_var();

	int Q = value.lookup_in_env( index );

	assert(regs.is_used(Q));

	// Set the call
	steps[s].call = Q;
    }
    // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the value and call it.
    else
    {
	int R2 = allocate_reg_from_step_in_token(s,t);

	// clear 'reg created' edge from s to old call.
	steps[s].call = R2;

	// Set the call
	set_C(R2, std::move( value ) );
    }

#if DEBUG_MACHINE >= 2
    check_used_regs();
    check_tokens();
#endif
}

void reg_heap::set_shared_value(int r, int v)
{
    // add a new computation
    int step = add_shared_step(r);
    add_shared_result(r, step);

    // set the value
    set_call(r, v);
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
    auto& version = tokens[t].version;
    for(int t2:tokens[t].children)
	version = std::max(version, tokens[t2].version+1);
    max_version = std::max(version, max_version);
}

bool reg_heap::is_dirty(int t) const
{
    for(int t2:tokens[t].children)
	if (tokens[t].version > tokens[t2].version)
	    return true;
    return false;
}

const vector<pair<int,int>>& reg_heap::Token::delta_result() const
{
    return vm_result.delta();
}

const vector<pair<int,int>>& reg_heap::Token::delta_step() const
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
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
    assert(not regs.access(r).created_by.first);
    assert(not regs.access(r).created_by.second);
    assert(not has_step(r));
  
    regs.reclaim_used(r);
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

    for(const auto& C: closure_stack)
	for(int r: C.Env)
	    scan.push_back(r);

    for(int j=0;j<parameters.size();j++) // yes
	scan.push_back(parameters[j].second);
    if (keep_identifiers)
	for(const auto& i: identifiers) // no
	    scan.push_back(i.second);
}

int reg_heap::set_head(int index, int R2)
{
    int R1 = heads[index];

    heads[index] = R2;

    return R1;
}

int reg_heap::set_head(int index, closure&& C)
{
    int R = allocate();

    set_head(index, R);

    set_C(R, std::move(C) );

    return R;
}

int reg_heap::allocate_head(closure&& C)
{
    int R = allocate();

    heads.push_back(R);

    set_C(R, std::move(C));

    return R;
}

int reg_heap::push_temp_head()
{
    int R = allocate();

    temp.push_back(R);

    return R;
}

int reg_heap::push_temp_head(closure&& C)
{
    int R = push_temp_head();

    set_C(R, std::move(C));

    return R;
}

void reg_heap::pop_temp_head()
{
//    int R = temp.back();

    temp.pop_back();
}

void reg_heap::resize(int s)
{
    assert(regs.size() == s);

    auto old_size = prog_steps.size();
    // Extend program.  Use regs.size() instead of size()
    prog_steps.resize(regs.size());
    prog_results.resize(regs.size());
    prog_temp.resize(regs.size());

    // Now we can use size() again.
    for(auto i=old_size;i<size();i++)
    {
	prog_steps[i] = non_computed_index;
	prog_results[i] = non_computed_index;

	assert(prog_steps[i] == non_computed_index);
	assert(prog_results[i] == non_computed_index);
	assert(prog_temp[i].none());
    }
}

void reg_heap::get_more_memory()
{
    collect_garbage();
    // This calls the resize( ) callback
    regs.get_more_memory();
}

bool reg_heap::reg_is_constant(int r) const
{
    return regs.access(r).type == reg::type_t::constant;
}

bool reg_heap::reg_is_changeable(int r) const
{
    return regs.access(r).type == reg::type_t::changeable;
}

void reg_heap::make_reg_changeable(int r)
{
    assert( regs.access(r).type == reg::type_t::changeable or regs.access(r).type == reg::type_t::unknown );

    regs.access(r).type = reg::type_t::changeable;
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
    for(auto& s: results[rc2].used_by)
	if (s.first == s1)
	    return true;

    return false;
}

bool reg_heap::reg_is_used_by(int r1, int r2) const
{
    int s1 = step_index_for_reg(r1);
    int rc2 = result_index_for_reg(r2);

    return result_is_used_by(s1,rc2);
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

void reg_heap::check_used_regs_in_token(int t) const
{
    assert(token_is_used(t));

    for(auto p: tokens[t].delta_result())
    {
	int r = p.first;
	assert(not prog_temp[r].test(0));
	prog_temp[r].set(0);

	// No results for constant regs
	assert(regs.access(r).type != reg::type_t::constant);
	int rc = p.second;
	if (rc > 0)
	{
	    assert(not steps.is_free(results[rc].source_step));
	    int call = results[rc].call_edge.first;
	    if (call > 0)
		assert(not results.is_free(call));
	}
    }
    for(auto p: tokens[t].delta_step())
    {
	int r = p.first;
	assert(not prog_temp[r].test(1));
	prog_temp[r].set(1);

	// If the step is unshared, the result must be unshared as well: this allows us to just walk unshared results.
	assert(prog_temp[r].test(0) and prog_temp[r].test(1));
	// No steps for constant regs
	assert(regs.access(r).type != reg::type_t::constant);
    }

    // FIXME - nonlocal. The same result/step are not set in multiple places!

    for(auto p: tokens[t].delta_step())
    {
	int r_s = p.second;
	if (r_s < 0) continue;
	
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
            // FIXME - nonlocal.  assert(is_modifiable(regs.access(R2).C.exp) or result_is_referenced(t,rc2));
      
	    // Used results should have values
	    assert(results[rc2].value);
	}
    }

    for(auto p: tokens[t].delta_result())
    {
	int r = p.first;
	int r_r = p.second;
	if (r_r < 0) continue;

	int r_s = results[r_r].source_step;
	int call = steps[r_s].call;
	
	assert(steps[r_s].source_reg == r);
	//FIXME! Check that source_step is in same token, for same reg
	int value = results[r_r].value;

	if (results[r_r].flags.test(0))
	    assert(is_root_token(t));

	if (value)
	    assert(call);

	if (call and value == call)
	    assert(regs.access(call).type == reg::type_t::constant);

	if (call and value and regs.access(call).type == reg::type_t::constant)
	    assert(value == call);

	if (t != root_token) continue;

	// Regs with values should have back-references from their call.
	if (value and regs.access(call).type != reg::type_t::constant)
	{
	    assert( has_result(call) );
	    int rc2 = result_index_for_reg(call);
	    assert( result_is_called_by(r_r, rc2) );
	}

	// If we have a value, then our call should have a value
	if (value)
	    assert(reg_has_value(call));
    }

    for(auto p: tokens[t].delta_result())
    {
	prog_temp[p.first].reset(0);
	prog_temp[p.first].reset(1);
    }

    for(auto p: tokens[t].delta_step())
    {
	prog_temp[p.first].reset(0);
	prog_temp[p.first].reset(1);
    }
}

void reg_heap::check_used_regs() const
{
    assert(tokens[root_token].vm_step.empty());
    assert(tokens[root_token].vm_result.empty());

    for(int t=0; t< tokens.size(); t++)
	if (token_is_used(t))
	    check_used_regs_in_token(t);

    // Check results that are not mapped
    for(const auto& result: results)
    {
	assert(not steps.is_free(result.source_step));
	int call = result.call_edge.first;
	if (call > 0)
	    assert(not results.is_free(call));
    }
}

int reg_heap::get_shared_step(int r)
{
    // 1. Get a new computation
    int s = steps.allocate();
    total_step_allocations++;
  
    // 2. Set the source of the computation
    steps[s].source_reg = r;

    assert(s > 0);
    
    return s;
}

/// Add a shared step at (t,r) -- assuming there isn't one already
int reg_heap::add_shared_step(int r)
{
    assert(not has_step(r));

    // Allocate a step
    int s = get_shared_step(r);

    // Link it in to the mapping
    prog_steps[r] = s;

    assert(s > 0);

    return s;
}

int reg_heap::get_shared_result(int r, int s)
{
    // 1. Get a new result
    int rc = results.allocate();
    total_comp_allocations++;
  
    // 2. Set the source of the result
    results[rc].source_step = s;
    results[rc].source_reg = r;

    assert(rc > 0);

    return rc;
}

/// Add a shared result at (t,r) -- assuming there isn't one already
int reg_heap::add_shared_result(int r, int s)
{
    assert(not has_result(r));
    // There should already be a step, if there is a result
    assert(has_step(r));

    // Get a result
    int rc = get_shared_result(r,s);

    // Link it in to the mapping
    prog_results[r] = rc;

    assert(rc > 0);

    return rc;
}

void reg_heap::check_back_edges_cleared_for_step(int s)
{
    for(auto& rcp: steps.access_unused(s).used_inputs)
	assert(rcp.second == 0);
    for(auto& r: steps.access_unused(s).created_regs)
    {
	auto& created_by = regs.access(r).created_by;
	assert(created_by.first == 0);
	assert(created_by.second == 0);
    }
}

void reg_heap::check_back_edges_cleared_for_result(int rc)
{
    assert(results.access_unused(rc).call_edge.second == 0);
}

void reg_heap::clear_back_edges_for_reg(int r)
{
    assert(r > 0);
    auto& created_by = regs.access(r).created_by;
    int s = created_by.first;
    if (s > 0)
    {
	auto& backward = steps[s].created_regs;
	int j = created_by.second;
	assert(0 <= j and j < backward.size());

	// Clear the forward edge.
	created_by = {0, 0};

	// Move the last element to the hole, and adjust index of correspond forward edge.
	if (j + 1 < backward.size())
	{
	    backward[j] = backward.back();
	    auto& forward2 = regs.access(backward[j]);
	    forward2.created_by.second = j;

	    assert(regs.access(backward[j]).created_by.second == j);
	}
	backward.pop_back();
    }
}

void reg_heap::clear_back_edges_for_step(int s)
{
    assert(s > 0);
    auto& forward1 = steps[s].used_inputs;
    for(int i=0; i<forward1.size(); i++)
    {
	auto& backward = results[forward1[i].first].used_by;
	assert(0 <= i and i < forward1.size());
	int j = forward1[i].second;
	assert(0 <= j and j < backward.size());

	forward1[i] = {0,0};

	if (j+1 < backward.size())
	{
	    // erase the backward edge by moving another backward edge on top of it.
	    backward[j] = backward.back();
	    // adjust the forward edge for that backward edge
	    int i2 = backward[j].second;
	    auto& forward2 = steps[backward[j].first].used_inputs;
	    assert(0 <= i2 and i2 < forward2.size());
	    forward2[i2].second = j;

	    assert(steps[backward[j].first].used_inputs[backward[j].second].second == j);
	    assert(results[forward2[i2].first].used_by[forward2[i2].second].second == i2);
	}

	backward.pop_back();
    }
    for(auto& r: steps[s].created_regs)
	regs.access(r).created_by = {0,{}};
    steps[s].created_regs.clear();
}

void reg_heap::clear_back_edges_for_result(int rc)
{
    assert(rc > 0);
    // FIXME! If there is a value, set, there should be a call_edge
    //        Should we unmap all values with no .. value/call_edge?
    int call = results[rc].call_edge.first;
    if (call)
    {
	assert(results[rc].value);

	auto& backward = results[call].called_by;
	int j = results[rc].call_edge.second;
	assert(0 <= j and j < backward.size());

	// Clear the forward edge.
	results[rc].call_edge = {0, 0};

	// Move the last element to the hole, and adjust index of correspond forward edge.
	if (j+1 < backward.size())
	{
	    backward[j] = backward.back();
	    auto& forward2 = results[backward[j]];
	    forward2.call_edge.second = j;

	    assert(results[backward[j]].call_edge.second == j);
	}
	backward.pop_back();
    }
}

void reg_heap::clear_step(int r)
{
    assert(not has_result(r));
    int s = prog_steps[r];
    prog_steps[r] = non_computed_index;
  
    if (s > 0)
    {
#ifndef NDEBUG
	check_back_edges_cleared_for_step(s);
#endif
	steps.reclaim_used(s);
    }
}

void reg_heap::clear_result(int r)
{
    int rc = prog_results[r];
    prog_results[r] = non_computed_index;

    if (rc > 0)
    {
#ifndef NDEBUG
	check_back_edges_cleared_for_result(rc);
#endif
	results.reclaim_used(rc);
    }
}

const expression_ref& reg_heap::get_parameter_value_in_context(int p, int c)
{
    int& R = parameters[p].second;

    return get_reg_value_in_context(R, c);
}

const expression_ref& reg_heap::get_reg_value_in_context(int& R, int c)
{
    total_get_reg_value++;
    if (regs.access(R).type == reg::type_t::constant) return regs.access(R).C.exp;

    total_get_reg_value_non_const++;
    reroot_at_context(c);

    if (has_result(R))
    {
	total_get_reg_value_non_const_with_result++;
	int R2 = result_value_for_reg(R);
	if (R2) return regs.access(R2).C.exp;
    }

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    auto p = incremental_evaluate_in_context(R,c);
    R = p.first;
    int value = p.second;

    return regs.access(value).C.exp;
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
    return regs.access(value).C;
}

const closure& reg_heap::lazy_evaluate(int& R, int c)
{
    auto p = incremental_evaluate_in_context(R,c);
    R = p.first;
    int value = p.second;
    return regs.access(value).C;
}

const closure& reg_heap::lazy_evaluate_head(int index, int c)
{
    int R1 = heads[index];
    auto p = incremental_evaluate_in_context(R1,c);
    int R2 = p.first;
    int value = p.second;
    if (R2 != R1)
	set_head(index, R2);

    return regs.access(value).C;
}

const closure& reg_heap::lazy_evaluate_unchangeable(int& R)
{
    R = incremental_evaluate_unchangeable(R);
    return regs.access(R).C;
}

int reg_heap::get_modifiable_value_in_context(int R, int c)
{
    assert( regs.access(R).C.exp.head().type() == modifiable_type);
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

reg_heap::reg_heap(const std::shared_ptr<module_loader>& L)
    :regs(1,[this](int s){resize(s);}),
     steps(1),
     results(1),
     P(new Program(L)),
     prog_steps(1,non_existant_index),
     prog_results(1, non_existant_index),
     prog_temp(1)
{
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

optional<int> reg_heap::maybe_find_parameter(const string& s) const
{
    for(int i=0;i<parameters.size();i++)
	if (parameters[i].first == s)
	    return i;

    return boost::none;
}

int reg_heap::find_parameter(const string& s) const
{
    auto index = maybe_find_parameter(s);
    if (not index)
	throw myexception()<<"Can't find parameter '"<<s<<"'!";
    return *index;
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

int reg_heap::add_modifiable_parameter(const string& full_name)
{
    expression_ref E = var("Parameters.new_modifiable");
    E = {var("Prelude.unsafePerformIO"), E};
    E = {var("Parameters.evaluate"),-1,E};

    return add_parameter(full_name, E);
}

int reg_heap::add_parameter(const string& full_name, const expression_ref& E)
{
    // 1. Allocate space for the expression
    int r = allocate();
    set_C(r, preprocess( E ) );

    add_parameter(full_name, r);

    return r;
}

void reg_heap::add_parameter(const string& full_name, int r)
{
    assert(full_name.size() != 0);

    // 1. Check that we don't already have a parameter with that name
    for(const auto& parameter: parameters)
	if (parameter.first == full_name)
	    throw myexception()<<"A parameter with name '"<<full_name<<"' already exists - cannot add another one.";

    // 2. Allocate space for the parameter
    parameters.push_back( {full_name, r} );
}
