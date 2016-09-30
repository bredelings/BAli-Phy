#include <iostream>
#include "graph_register.H"
#include <algorithm>
#include "util.H"

using std::string;
using std::vector;
using std::pair;

using std::cerr;
using std::endl;

int total_reg_allocations = 0;
int total_step_allocations = 0;
int total_comp_allocations = 0;
int total_set_reg_value = 0;
int total_get_reg_value = 0;
int total_get_reg_value_non_const = 0;
int total_get_reg_value_non_const_with_result = 0;
int total_context_pr = 0;
int total_tokens = 0;

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

    re_evaluate = R.re_evaluate;

    type = R.type;

    n_heads = R.n_heads;

    created_by = std::move(R.created_by);

    return *this;
}

reg::reg(reg&& R) noexcept
:C( std::move(R.C) ),
			 re_evaluate( R.re_evaluate ),
			 type ( R.type ),
			 n_heads( R.n_heads ),
			 created_by( std::move(R.created_by) )
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
    assert(created_by.first == 0);
    assert(null(created_by.second));
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
    return prog_steps[r];
}

int reg_heap::result_index_for_reg(int r) const 
{
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

void reg_heap::set_result_value_for_reg(int r1)
{
    int call = call_for_reg(r1);

    assert(call);

    int value = value_for_reg(call);

    assert(value);

    int rc1 = result_index_for_reg(r1);
    if (rc1 <= 0)
	rc1 = add_shared_result(r1, step_index_for_reg(r1));
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
    steps[s].created_regs.push_front(r);
    assert(access(r).created_by.first == 0);
    assert(null(access(r).created_by.second));
    access(r).created_by = {s,steps[s].created_regs.begin()};
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
    int s = get_shared_step(R);

    assert(tokens[t].vm_step.empty());
    tokens[t].vm_step.add_value(R,s);

    assert(tokens[t].vm_result.empty());
    tokens[t].vm_result.add_value(R,-1);

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
    assert(not access(r).created_by.first);
    assert(null(access(r).created_by.second));
  
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
	assert(prog_steps.size() == old_size);
	assert(prog_results.size() == old_size);
	assert(prog_temp.size() == old_size);
    }

    base_pool_t::expand_memory(s);

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
	assert((prog_temp[r]&1) == 0);
	prog_temp[r] |= 1;
	if (is_root_token(t)) assert(p.second != -1);
	// No results for constant regs
	assert(access(r).type != reg::type_t::constant);
    }
    for(auto p: tokens[t].delta_step())
    {
	int r = p.first;
	assert((prog_temp[r]&2) == 0);
	prog_temp[r] |= 2;
	if (is_root_token(t)) assert(p.second != -1);
	// If the step is set, the result better be set as well.
	assert(prog_temp[r] == 3);
	// No steps for constant regs
	assert(access(r).type != reg::type_t::constant);
    }

    // FIXME - nonlocal. The same result/step are not set in multiple places!

    for(auto p: tokens[t].delta_step())
    {
	int r_s = p.second;
	if (r_s <= 0) continue;
	
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
            // FIXME - nonlocal.  assert(is_modifiable(access(R2).C.exp) or result_is_referenced(t,rc2));
      
	    // Used results should have values
	    assert(results[rc2].value);
	}
    }

    for(auto p: tokens[t].delta_result())
    {
	int r = p.first;
	int r_r = p.second;
	if (r_r <= 0) continue;

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

    for(auto p: tokens[t].delta_result())
	prog_temp[p.first] = 0;

    for(auto p: tokens[t].delta_step())
	prog_temp[p.first] = 0;

}

void reg_heap::check_used_regs() const
{
    assert(tokens[root_token].vm_step.empty());
    assert(tokens[root_token].vm_result.empty());

    for(auto c: prog_temp)
	assert(not c);

    for(int t=0; t< tokens.size(); t++)
	if (token_is_used(t))
	    check_used_regs_in_token(t);

    for(auto c: prog_temp)
	assert(not c);
}

int reg_heap::get_shared_step(int r)
{
    // 1. Get a new computation
    int s = steps.allocate();
    total_step_allocations++;
  
    // 2. Set the source of the computation
    steps[s].source_reg = r;

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

    return rc;
}

void reg_heap::check_back_edges_cleared_for_step(int s)
{
    for(auto& rcp: steps.access_unused(s).used_inputs)
	assert(null(rcp.second));
    for(auto& r: steps.access_unused(s).created_regs)
    {
	auto& created_by = access(r).created_by;
	assert(created_by.first == 0);
	assert(null(created_by.second));
    }
}

void reg_heap::check_back_edges_cleared_for_result(int rc)
{
    assert(null(results.access_unused(rc).call_edge.second));
}

void reg_heap::clear_back_edges_for_reg(int r)
{
    assert(r > 0);
    auto& created_by = access(r).created_by;
    int s = created_by.first;
    if (s > 0)
    {
	steps[s].created_regs.erase(created_by.second);
	created_by.first = 0;
	created_by.second = {};
    }
}

void reg_heap::clear_back_edges_for_step(int s)
{
    assert(s > 0);
    for(auto& rcp: steps[s].used_inputs)
    {
	results[rcp.first].used_by.erase(rcp.second);
	rcp.second = {};
    }
    for(auto& r: steps[s].created_regs)
	access(r).created_by = {0,{}};
    steps[s].created_regs.clear();
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
	results[rc].call_edge = {0,{}};
    }
}

void reg_heap::clear_step(int r)
{
    assert(not has_result(r));
    int s = prog_steps[r];
    prog_steps[r] = 0;
  
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
    prog_results[r] = 0;

    if (rc > 0)
    {
#ifndef NDEBUG
	check_back_edges_cleared_for_result(rc);
#endif
	results.reclaim_used(rc);
    }
}

std::vector<int>& reg_heap::triggers_for_context(int c)
{
    reroot_at_context(c);
    return triggers();
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
     loader(L),
     prog_steps(1),
     prog_results(1),
     prog_temp(1)
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
