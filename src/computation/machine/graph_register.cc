#include <iostream>
#include <range/v3/all.hpp>
#include <algorithm>
#include "util/truncate.H"
#include "util/io/vector.H"
#include "graph_register.H"
#include "computation/expression/var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/tuple.H"
#include "computation/expression/random_variable.H"
#include "computation/expression/modifiable.H"
#include "computation/operations.H"
#include "effect.H"

using std::string;
using std::vector;
using std::pair;

using std::cerr;
using std::endl;

using std::optional;

long total_reg_allocations = 0;
long total_step_allocations = 0;
long total_comp_allocations = 0;
long total_set_reg_value = 0;
long total_get_reg_value = 0;
long total_get_reg_value_non_const = 0;
long total_get_reg_value_non_const_with_result = 0;
long total_context_pr = 0;
long total_tokens = 0;

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
 * - used edges (forward: used_regs, backward: used_by)
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
 * 6. Move call and used_regs into reduction
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

bool Step::has_nonforce_effect() const
{
    return flags.test(6);
}

void Step::mark_with_nonforce_effect()
{
    flags.set(6);
}

bool Step::has_pending_nonforce_effect() const
{
    return flags.test(5);
}

void Step::set_pending_nonforce_effect()
{
    flags.set(5);
}

void Step::clear_pending_nonforce_effect()
{
    flags.set(5,false);
}

void Step::clear()
{
    source_reg = -1;
    call = 0;
    truncate(call_edge);
    // We are clearing created_regs in clear_back_edges_for_step.
    assert(created_regs.empty());

    flags.reset();
    // This should already be cleared.
    // assert(flags.none());
}

void Step::check_cleared() const
{
    assert(not call);
    assert(not call_edge.first);
    assert(created_regs.empty());
    assert(flags.none());
}

Step& Step::operator=(Step&& S) noexcept
{
    source_reg = S.source_reg;
    call = S.call;
    call_edge = S.call_edge;
    created_regs  = std::move( S.created_regs );
    flags = S.flags;

    return *this;
}

Step::Step(Step&& S) noexcept
    :source_reg( S.source_reg),
     call ( S.call ),
     call_edge (S.call_edge),
     created_regs ( std::move(S.created_regs) ),
     flags ( S.flags )
{ }

void reg::clear()
{
    C.clear();
    type = type_t::unevaluated;
    truncate(used_regs);
    truncate(forced_regs);
    truncate(used_by);
    truncate(called_by);
    created_by = {0,0};
    flags.reset();
}

void reg::check_cleared() const
{
    assert(not C);
    assert(type == type_t::unevaluated);
    assert(used_regs.empty());
    assert(forced_regs.empty());
    assert(used_by.empty());
    assert(called_by.empty());
    assert(created_by.first == 0);
    assert(created_by.second == 0);
    assert(flags.none());
}

reg& reg::operator=(reg&& R) noexcept
{
    C = std::move(R.C);

    type = R.type;

    used_regs  = std::move( R.used_regs );

    forced_regs  = std::move( R.forced_regs );

    used_by = std::move( R.used_by );

    called_by = std::move( R.called_by );

    created_by = std::move(R.created_by);

    flags = R.flags;

    return *this;
}

reg::reg(reg&& R) noexcept
    :C( std::move(R.C) ),
     type ( R.type ),
     used_regs ( std::move(R.used_regs) ),
     forced_regs (std::move(R.forced_regs) ),
     used_by ( std::move( R.used_by) ),
     called_by ( std::move( R.called_by) ),
     created_by( std::move(R.created_by) ),
     flags ( R.flags )
{ }

std::optional<int> reg_heap::creator_of_reg(int r) const
{
    int s = regs[r].created_by.first;
    assert(s >= 0);
    if (s == 0)
        return {};
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

void reg_heap::register_prior(int r)
{
    r = follow_index_var(r);

    if (not reg_has_value(r))
        throw myexception()<<"Can't register a prior reg that is unevaluated!";

    assert(access_value_for_reg(r).exp.is_log_double());

    // We set bit zero of regs[r].flag every time we register OR REREGISTER it.
    // Therefore if two different random variables share a PDF, we cannot detect it
    //  simply by the bit being set twice.

    if (reg_is_constant(r))
    {
        // Also avoid putting a bit on constant regs?
    }
    else
    {
        regs.access(r).flags.set(0);

        assert(reg_is_changeable(r));
    }
}

void reg_heap::unregister_prior(int r)
{
    regs.access(r).flags.reset(0);
}

void reg_heap::register_likelihood_(int r)
{
    r = follow_index_var(r);

    if (not reg_has_value(r))
        throw myexception()<<"Can't register a likelihood reg that is unevaluated!";

    assert(access_value_for_reg(r).exp.is_log_double());

    if (regs.access(r).flags.test(1))
        throw myexception()<<"Can't register a likelihood reg that is already registered!";

    // We can only put the bit on a changeable reg, not on (say) an index_var.
    // Therefore, we must evaluate r -> r2 here.
    // QUESTION: WHY can't we put the bit on constant regs?

    if (reg_is_constant(r))
    {
        // Also avoid putting a bit on constant regs?

        likelihood_heads.push_back(r);
    }
    else
    {
        regs.access(r).flags.set(1);

        assert(reg_is_changeable(r));

        likelihood_heads.push_back(r);
    }
}

void reg_heap::unregister_likelihood_(int r)
{
    regs.access(r).flags.reset(1);
}

log_double_t reg_heap::probability_for_context(int c)
{
    total_context_pr++;

    return prior_for_context(c) * likelihood_for_context(c);
}

int reg_heap::follow_index_var(int r) const
{
    while((*this)[r].exp.is_index_var())
        r = (*this)[r].reg_for_index_var();
    return r;
}

void reg_heap::register_effect_pending_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_nonforce_effect());

    // Step must have not be on pending list
    assert(not steps[s].has_pending_nonforce_effect());

    steps[s].set_pending_nonforce_effect();

    pending_effect_steps.insert(s);
}

void reg_heap::unregister_effect_pending_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_nonforce_effect());

    // Step must have be on pending list
    assert(steps[s].has_pending_nonforce_effect());
    assert(pending_effect_steps.count(s));

    // Remove step from pending list
    pending_effect_steps.erase(s);
    steps[s].clear_pending_nonforce_effect();
}

void reg_heap::register_effect_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_nonforce_effect());

    // Remove step from pending list if it is there.
    if (steps[s].has_pending_nonforce_effect())
    {
        steps[s].clear_pending_nonforce_effect();

        assert(pending_effect_steps.count(s));
        pending_effect_steps.erase(s);
    }
    else
        assert(not pending_effect_steps.count(s));

    int call = steps[s].call;
    auto& e = expression_at(call);
    assert(e.is_a<effect>());
    e.as_<effect>().register_effect(*this);
}

void reg_heap::unregister_effect_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_nonforce_effect());

    // Step must have not be on pending list
    assert(not steps[s].has_pending_nonforce_effect());
    assert(not pending_effect_steps.count(s));

    int call = steps[s].call;
    auto& e = expression_at(call);
    assert(e.is_a<effect>());
    e.as_<effect>().unregister_effect(*this);
}

void reg_heap::register_pending_effects()
{
    // Don't modify `pending_effect_steps` while we are walking it!
    auto v = pending_effect_steps | ranges::to<vector>;
    for(int s: v)
    {
        assert(steps[s].has_pending_nonforce_effect());
        assert(pending_effect_steps.count(s));
        register_effect_at_step(s);
    }

    assert(pending_effect_steps.empty());
}

expression_ref reg_heap::evaluate_program(int c)
{
    if (not program_result_head)
        throw myexception()<<"No program has been set!";

    auto result = lazy_evaluate(heads[*program_result_head], c, true).exp;

    // Force the computation of priors and likelihoods
    likelihood_for_context(c);
    prior_for_context(c);

    // Check that all the priors and likelihoods are forced.
#ifndef NDEBUG
    for(int r_likelihood: likelihood_heads)
    {
        assert(reg_exists(r_likelihood));
        assert(reg_has_value(follow_index_var(r_likelihood)));
    }

    for(int r_rv: random_variables_)
    {
        assert(reg_exists(r_rv));

        int r_pdf = (*this)[r_rv].reg_for_slot(1);
        assert(reg_exists(r_pdf));

        assert(reg_has_value(follow_index_var(r_pdf)));
    }
#endif

    register_pending_effects();

    return result;
}

prob_ratios_t reg_heap::probability_ratios(int c1, int c2)
{
#if DEBUG_MACHINE >= 2
    for(auto x : prog_temp)
        assert(x.none());
#endif

    constexpr int pdf_bit = 4;

    // 1. reroot to c1 and force the program
    evaluate_program(c1);
    int num_rvs1 = random_variables_.size();

    // 2. install another reroot handler
    vector<pair<int,int>> original_pdf_results;

    std::function<void(int)> handler = [&original_pdf_results,this](int old_root)
    {
        for(auto& p: tokens[old_root].delta_result())
        {
            int r =  p.first;

            // We're only interested in cases where both contexts have a result that is > 0.
            // But (i) we need to seen the "seen" flag in any case
            //    (ii) we need to remember that we have set it so that we can unset it.
            if (regs.is_used(r) and regs.access(r).flags.any() and not prog_temp[r].test(pdf_bit))
            {
                prog_temp[r].set(pdf_bit);
                original_pdf_results.push_back(p);
            }
        }
    };

    reroot_handlers.push_back(handler);

    // 3. reroot to c2 and force the program
    evaluate_program(c2);
    int num_rvs2 = random_variables_.size();

    // 4. compute the ratio only for (i) changed pdfs that (ii) exist in both c1 and c2
    prob_ratios_t R;
    R.variables_changed = (num_rvs1 != num_rvs2);

    for(auto [pdf_reg, orig_pdf_value]: original_pdf_results)
    {
        assert(prog_temp[pdf_reg].test(pdf_bit));

        prog_temp[pdf_reg].reset(pdf_bit);

        // Only compute a ratio if the pdf is present and computed in BOTH contexts.
        if (orig_pdf_value > 0 and has_result(pdf_reg))
        {
            int result_reg1 = orig_pdf_value;
            assert(reg_is_changeable(pdf_reg));
            int result_reg2 = result_for_reg(pdf_reg);
            log_double_t r = (*this)[result_reg2].exp.as_log_double() / (*this)[result_reg1].exp.as_log_double();

            // The reg might be unregistered between the two programs...
            if (regs.access(pdf_reg).flags.test(0))
                R.prior_ratio *= r;
            else if (regs.access(pdf_reg).flags.test(1))
                R.likelihood_ratio *= r;
        }
        else
            R.variables_changed = true;
    }

#if DEBUG_MACHINE >= 2
    for(auto x : prog_temp)
        assert(x.none());
#endif

    // 5. remove the reroot handler
    reroot_handlers.pop_back();

//  If pr1 and pr2 are off far enough, this test will fail...
//    if (pr1 > 0.0 and pr2 > 0.0)
//      assert( std::abs( (pr2/pr1).log() - R.prior_ratio.log() - R.likelihood_ratio.log()) < 1.0e-4 );

    return R;
}

void reg_heap::register_random_variable(int r)
{
    r = follow_index_var(r);

    if (not reg_has_value(r))
        throw myexception()<<"Can't register a random variable that is unevaluated!";

    if (not is_random_variable(expression_at(r)))
        throw myexception()<<"Trying to register `"<<expression_at(r)<<"` as random variable";
    random_variables_.push_back(r);

    int r_pdf = (*this)[r].reg_for_slot(1);
    register_prior(r_pdf);
}

void reg_heap::unregister_random_variable(int r)
{
    // NOTE: We are NOT going to clear the bit on the reg.
    //       That is supposed to be set even for things that are NOT in the root.
    //       It will be cleared when the reg is destroyed.

    // FIXME: This is SLOW because we have to walk the list to find the random variable.
    //        Alternatives: (i) use a set (ii) use a hash (iii) record the position in the list somehow.

    if (not is_random_variable(expression_at(r)))
        throw myexception()<<"Trying to unregister `"<<expression_at(r)<<"` as random variable";

    std::optional<int> index;
    for(int i=0;i<random_variables_.size();i++)
        if (random_variables_[i] == r)
            index = i;

    if (not index)
        throw myexception()<<"unregister_random_variable: random variable <"<<r<<"> not found!";

    if (*index + 1 < random_variables_.size())
        std::swap(random_variables_[*index], random_variables_.back());

    random_variables_.pop_back();

    int r_pdf = (*this)[r].reg_for_slot(1);
    unregister_prior(r_pdf);
}

const vector<int>& reg_heap::random_variables() const
{
    return random_variables_;
}

void reg_heap::register_transition_kernel(int r_rate, int r_kernel)
{
    transition_kernels_.push_back({r_rate, r_kernel});
}

void reg_heap::unregister_transition_kernel(int r_kernel)
{
    clear_transition_kernel_active(r_kernel);

    std::optional<int> index;
    for(int i=0;i<transition_kernels_.size();i++)
        if (transition_kernels_[i].second == r_kernel)
            index = i;

    if (not index)
        throw myexception()<<"unregister_transition_kernel: transition kernel <"<<r_kernel<<"> not found!";

    if (*index + 1 < transition_kernels_.size())
        std::swap(transition_kernels_[*index], transition_kernels_.back());

    transition_kernels_.pop_back();
}

const vector<pair<int,int>>& reg_heap::transition_kernels() const
{
    return transition_kernels_;
}

void reg_heap::mark_transition_kernel_active(int r)
{
    regs[r].flags.set(7);
}

bool reg_heap::transition_kernel_is_active(int r)
{
    return regs.is_used(r) and regs[r].flags.test(7);
}

void reg_heap::clear_transition_kernel_active(int r)
{
    if (regs.is_used(r) and transition_kernel_is_active(r))
        regs[r].flags.set(7,false);
}

optional<int> reg_heap::compute_expression_is_modifiable_reg(int index)
{
    int& H = heads[index];

    return find_update_modifiable_reg(H);
}

optional<int> reg_heap::find_update_modifiable_reg(int& R)
{
    // Note: here we always update R
    R = incremental_evaluate_unchangeable(R);

    auto& C = (*this)[R];

    if (is_modifiable(C.exp))
        return R;
    else if (is_random_variable(C.exp))
    {
        int R2 = C.reg_for_slot(0);
        return find_update_modifiable_reg(R2);
    }
    else if (is_seq(C.exp))
    {
        int R2 = C.reg_for_slot(1);
        return find_update_modifiable_reg(R2);
    }
    else if (is_join(C.exp))
    {
        int R2 = C.reg_for_slot(1);
        return find_update_modifiable_reg(R2);
    }
    else
        return {};
}

optional<int> reg_heap::find_modifiable_reg(int R)
{
    return find_update_modifiable_reg(R);
}


optional<int> reg_heap::find_modifiable_reg_in_context(int r, int c)
{
    // Note that value_for_precomputed_reg( ) also does
    // - follow_index_var( )
    // Although it does not do
    // - r = call_for_reg(r)
    // After we stop performing evaluation here, this looks a lot like
    // a hypothetical `value_for_precomputed_reg_in_context( )`,
    // except that we stop at modifiables.

    // Warning: ABOMINATION!
    // FIXME: This should be forced by a `seq` inside the program.
    // But that probably requires force-edges to be working.
    incremental_evaluate_in_context(r,c);

    assert(is_root_token(token_for_context(c)));

    r = follow_index_var(r);

    // r should not be unknown or an index_var
    assert(reg_is_constant(r) or (reg_is_changeable(r) and reg_has_call(r)));

    while (not reg_is_constant(r))
    {
        assert(reg_is_changeable(r));
        assert(reg_has_call(r));

        if (is_modifiable(expression_at(r)))
            return r;
        else
            r = call_for_reg(r);
    };

    // r is (now) a constant.
    // There is therefore no modifiable.
    return {};
}


optional<int> reg_heap::compute_expression_is_random_variable(int index)
{
    int& H = heads[index];

    return find_update_random_variable(H);
}

optional<int> reg_heap::find_update_random_variable(int& R)
{
    // Note: here we always update R
    R = incremental_evaluate_unchangeable(R);

    auto& C = closure_at(R);

    if (is_random_variable(C.exp))
        return R;
    else if (is_seq(C.exp))
    {
        int R2 = C.reg_for_slot(1);
        return find_update_random_variable(R2);
    }
    else if (is_join(C.exp))
    {
        int R2 = C.reg_for_slot(1);
        return find_update_random_variable(R2);
    }
    else
        return {};
}

optional<int> reg_heap::find_random_variable(int R)
{
    return find_update_random_variable(R);
}

const expression_ref reg_heap::get_range_for_random_variable(int c, int r)
{
    if (find_update_random_variable(r))
    {
        int r_range = closure_at(r).reg_for_slot(3);
        return get_reg_value_in_context(r_range, c);
    }
    else
        throw myexception()<<"Trying to get range from `"<<closure_at(r).print()<<"`, which is not a random_variable!";
}

double reg_heap::get_rate_for_random_variable(int c, int r)
{
    if (find_update_random_variable(r))
    {
        int r_rate = closure_at(r).reg_for_slot(4);
        return get_reg_value_in_context(r_rate, c).as_double();
    }
    else
        throw myexception()<<"Trying to get rate from `"<<closure_at(r).print()<<"`, which is not a random_variable!";
}

int reg_heap::step_index_for_reg(int r) const 
{
    assert(prog_steps[r] != 0);
    return prog_steps[r];
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

const closure& reg_heap::access_value_for_reg(int R1) const
{
    int R2 = value_for_reg(R1);
    return closure_at(R2);
}

const closure& reg_heap::value_for_precomputed_reg(int r) const
{
    r = follow_index_var(r);
    return access_value_for_reg(r);
}

bool reg_heap::reg_has_value(int r) const
{
    if (regs.access(r).type == reg::type_t::constant)
        return true;
    else
        return has_result(r);
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
    return result_for_reg(r)>0;
}

int reg_heap::value_for_reg(int r) const 
{
    assert(not expression_at(r).is_index_var());
    if (reg_is_changeable(r))
    {
        assert(has_result(r));
        return result_for_reg(r);
    }
    else
    {
        assert(reg_is_constant(r));
        return r;
    }
}

int reg_heap::result_for_reg(int r) const 
{
    assert(prog_results[r] != 0);
    return prog_results[r];
}

void reg_heap::set_result_for_reg(int r1)
{
    // 1. Find called reg
    int r2 = step_for_reg(r1).call;
    assert(reg_is_constant(r2) or reg_is_changeable(r2));

    // 2. Set the result value for the current reg
    prog_results[r1] = value_for_reg(r2);
    assert(prog_results[r1] > 0);
}

void reg_heap::set_used_reg(int r1, int r2)
{
    assert(reg_is_changeable(r2));

    assert(regs.is_used(r2));

    assert(closure_at(r2));

    assert(has_result(r2));

    // An index_var's value only changes if the thing the index-var points to also changes.
    // So, we may as well forbid using an index_var as an input.
    assert(not expression_at(r2).is_index_var());

    auto& R1 = regs[r1];
    auto& R2 = regs[r2];
    int back_index = R2.used_by.size();
    int forw_index = R1.used_regs.size();
    R2.used_by.push_back({r1,forw_index});
    R1.used_regs.push_back({r2,back_index});

    assert(reg_is_used_by(r1,r2));
}

void reg_heap::set_forced_input(int r1, int r2)
{
    assert(reg_is_changeable(r2));

    assert(regs.is_used(r2));

    assert(closure_at(r2));

    assert(has_result(r2));

    // An index_var's value only changes if the thing the index-var points to also changes.
    // So, we may as well forbid using an index_var as an input.
    assert(not expression_at(r2).is_index_var());

    regs[r1].forced_regs.push_back(r2);
}

void reg_heap::set_call(int s1, int r2)
{
    // Check that step s is legal
    assert(steps.is_used(s1));

    // Check that R2 is legal
    assert(regs.is_used(r2));

    // R2 could be unevaluated if we are setting the value of a modifiable.

    // R2 shouldn't have an index var.
    assert(not expression_at(r2).is_index_var());

    // Don't override an *existing* call
    assert(steps[s1].call == 0);

    auto& S1 = steps[s1];

    // Set the call
    S1.call = r2;

    if (not reg_is_constant(r2))
    {
        // 6. Add a call edge from to R2.
        auto& R2 = regs[r2];
        int back_index = R2.called_by.size();
        R2.called_by.push_back(s1);
        S1.call_edge = {r2, back_index};
    }
}

void reg_heap::clear_call(int s)
{
    auto& S = steps[s];
    int call = S.call;
    assert(call > 0);

    // 1. Remove the edge from step[s] <--- regs[call]
    if (S.call_edge.first > 0 and not regs.is_free(call))
    {
        auto [r2,j] = S.call_edge;
        assert(call == r2);
        auto& backward = regs[call].called_by;
        assert(0 <= j and j < backward.size());

        // Move the last element to the hole, and adjust index of correspond forward edge.
        if (j+1 < backward.size())
        {
            backward[j] = backward.back();
            auto& forward2 = steps[backward[j]];
            forward2.call_edge.second = j;

            assert(steps[backward[j]].call_edge.second == j);
        }
        backward.pop_back();
    }

    // 2. Clear the forward edge from steps[s] -> regs[call]
    S.call = 0;
    S.call_edge = {0, 0};
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
    for(int r: closure_at(R).Env)
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

void reg_heap::mark_step_with_nonforce_effect(int s)
{
    steps[s].mark_with_nonforce_effect();
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
    assert(not children_of_token(t).size());
    assert(reg_is_changeable(R));

    if (not is_root_token(t))
        assert(tokens[t].type == token_type::set);

    // Check that this reg is indeed settable
    if (not is_modifiable(expression_at(R)))
        throw myexception()<<"set_reg_value: reg "<<R<<" is not modifiable!";

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
        int Q = value.reg_for_index_var();

        // Set the call
        set_call(s, Q);
    }
    // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the value and call it.
    else
    {
        int R2 = allocate_reg_from_step_in_token(s,t);

        // Set the call
        set_C(R2, std::move( value ) );

        // clear 'reg created' edge from s to old call.
        set_call(s, R2);
    }

#if DEBUG_MACHINE >= 2
    check_used_regs();
    check_tokens();
#endif
}

std::vector<int> reg_heap::used_regs_for_reg(int r) const
{
    vector<int> U;
    if (not has_step(r)) return U;

    for(const auto& [r2,_]: regs[r].used_regs)
        U.push_back(r2);

    return U;
}

std::vector<int> reg_heap::forced_regs_for_reg(int r) const
{
    vector<int> U;
    if (not has_step(r)) return U;

    for(int r: regs[r].forced_regs)
        U.push_back(r);

    return U;
}

void reg_heap::reclaim_used(int r)
{
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
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

    // FIXME: We want to remove all of these.
    // * we should be able to remove random_variables_.  However, walking random_variables_ might find references to old, destroyed, variables then.
    insert_at_end(scan, likelihood_heads); // yes
    insert_at_end(scan, random_variables_); // yes
    for(auto& [_,r]: transition_kernels_)
        scan.push_back(r);

    for(const auto& C: closure_stack)
        for(int r: C.Env)
            scan.push_back(r);

    if (keep_identifiers)
        for(const auto& [name,reg]: identifiers) // no
            scan.push_back(reg);
}

/// Add an expression that may be replaced by its reduced form
int reg_heap::add_compute_expression(const expression_ref& E)
{
    allocate_head(preprocess(E));

    return heads.size() - 1;
}

int reg_heap::add_named_head(const string& name, int r)
{
    int h = heads.size();
    heads.push_back(r);
    assert(not named_heads.count(name));
    named_heads[name] = h;
    return h;
}

optional<int> reg_heap::lookup_named_head(const string& name)
{
    auto it = named_heads.find(name);
    if (it == named_heads.end())
        return {};
    else
        return it->second;
}

int reg_heap::add_perform_io_head()
{
    perform_io_head = add_compute_expression(var("Compiler.IO.unsafePerformIO"));
    return *perform_io_head;
}

// 1. Pass in the program without logging state.
// 2. Generate the loggers regardless.
// 3. Return the value, and store it in the program head
// 4. Register the logging head, but don't return it.

int reg_heap::add_program(const expression_ref& E)
{
    // 1. Get the program head
    if (program_result_head or logging_head)
        throw myexception()<<"Trying to set program a second time!";

    auto P = E;

    if (program->type == Program::exe_type::standard)
    {
        P = {var("Compiler.IO.unsafePerformIO"), P};
        int program_head = add_compute_expression(P);
        program_result_head = program_head;
        return *program_result_head;
    }

    P = {var("Probability.Random.gen_model_no_alphabet"), P};
    if (program->type == Program::exe_type::log_list)
    {
        // 2. If the program doesn't return a pair, make it a pair
        P = {var("Probability.Random.add_null_program_result"), P};
    }
    P = {var("Compiler.IO.unsafePerformIO"), P};

    int program_head = add_compute_expression(P);
    P = reg_var(heads[program_head]);

    // 3. Add the program RESULT head
    program_result_head = add_compute_expression({fst,P});

    // 4. Add the program LOGGING head
    logging_head = add_compute_expression({var("Data.JSON.c_json"), {var("Probability.Random.log_to_json"),{snd, P}}});

    return *program_result_head;
}

void reg_heap::stack_push(int r)
{
    stack.push_back(r);
}

void reg_heap::stack_pop(int r)
{
    int r2 = stack_pop();
    if (r != r2)
        throw myexception()<<"Trying to pop reg "<<r<<" but got reg "<<r2<<"!";
}

int reg_heap::stack_pop()
{
    if (stack.empty())
        throw myexception()<<"Trying to pop an empty stack!";
    int r = stack.back();
    stack.pop_back();
    return r;
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

void reg_heap::make_reg_changeable(int r)
{
    assert( regs.access(r).type == reg::type_t::changeable or regs.access(r).type == reg::type_t::unevaluated );

    regs.access(r).type = reg::type_t::changeable;
}

bool reg_heap::reg_is_called_by(int r1, int s1) const
{
    for(int s: regs[r1].called_by)
        if (s == s1)
            return true;

    return false;
}

bool reg_heap::reg_is_used_by(int r1, int r2) const
{
    for(auto& [r,_]: regs[r2].used_by)
        if (r == r1)
            return true;

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
#endif
}

void reg_heap::check_used_regs_in_token(int t) const
{
    assert(token_is_used(t));

    if (tokens[t].type == token_type::reverse_execute)
    {
        for(auto [r,result]: tokens[t].delta_result())
            assert(result < 0);

        for(auto [r,step]: tokens[t].delta_step())
            assert(step < 0);
    }
    else if (tokens[t].type == token_type::execute)
    {
        for(auto [r,result]: tokens[t].delta_result())
            assert(result > 0);

        for(auto [r,step]: tokens[t].delta_step())
            assert(step > 0);
    }

    for(auto [r,result]: tokens[t].delta_result())
    {
        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(0));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(0);

        // No results for constant regs
        if (result > 0)
            assert(regs.access(r).type != reg::type_t::constant);
    }
    for(auto [r,step]: tokens[t].delta_step())
    {
        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(1));

        // Mark the reg as having a step in the delta.
        prog_temp[r].set(1);

        // If the step is unshared, the result must be unshared as well: this allows us to just walk unshared results.
        assert(prog_temp[r].test(0) and prog_temp[r].test(1));
        // No steps for constant regs
        if (step > 0)
            assert(regs.access(r).type != reg::type_t::constant);
    }

    // FIXME - nonlocal. The same result/step are not set in multiple places!

    for(auto [reg,res]: tokens[t].delta_result())
    {
        prog_temp[reg].reset(0);
        prog_temp[reg].reset(1);
    }

    for(auto [reg,step]: tokens[t].delta_step())
    {
        prog_temp[reg].reset(0);
        prog_temp[reg].reset(1);
    }
}

void reg_heap::check_used_regs() const
{
    assert(tokens[root_token].vm_step.empty());
    assert(tokens[root_token].vm_result.empty());

    for(int t=0; t< tokens.size(); t++)
        if (token_is_used(t))
            check_used_regs_in_token(t);

    for(auto& S:steps)
    {
        if (S.call > 0)
            assert(not regs.is_free(S.call));
    }

    for(auto i = regs.begin(); i != regs.end(); i++)
    {
        int r1 = i.addr();

        if (not regs[r1].used_regs.empty())
            assert(reg_is_changeable(r1));

        for(const auto& [r2,_]: regs[r1].used_regs)
        {
            // Used regs should have back-references to R
            assert( reg_is_used_by(r1, r2) );

            // Used computations should be mapped computation for the current token, if we are at the root
            assert(reg_is_changeable(r2));

            // The used result should be referenced somewhere more root-ward
            // so that this result can be invalidated, and the used result won't be GC-ed.
            // FIXME - nonlocal.  assert(is_modifiable(expression_at(R2)) or result_is_referenced(t,res2));
        }
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

void reg_heap::check_back_edges_cleared_for_step(int s) const
{
    assert(steps[s].call_edge.first == 0);
    assert(steps[s].call_edge.second == 0);

    for(auto& r: steps.access_unused(s).created_regs)
    {
        auto [step, index] = regs.access(r).created_by;
        assert(step == 0);
        assert(index == 0);
    }
}

void reg_heap::clear_back_edges_for_reg(int r, bool creator_survives)
{
    // 1. When destroying a reg, remove edge from regs[r] <---used_by--- regs[r3]
    assert(r > 0);
    for(auto& forward: regs[r].used_regs)
    {
        auto [r3,j] = forward;
        if (regs.is_free(r3)) continue;
        auto& backward = regs[r3].used_by;
        assert(0 <= j and j < backward.size());

        forward = {0,0};

        if (j+1 < backward.size())
        {
            // erase the backward edge by moving another backward edge on top of it.
            backward[j] = backward.back();
            auto [r2,i2] = backward[j];
            // adjust the forward edge for that backward edge
            auto& forward2 = regs[r2].used_regs;
            assert(0 <= i2 and i2 < forward2.size());
            forward2[i2].second = j;

            assert(regs[r2].used_regs[i2].second == j);
            assert(regs[forward2[i2].first].used_by[forward2[i2].second].second == i2);
        }

        backward.pop_back();
    }


    // 2. When destroying a reg, remove edge from step[s] ---created_regs---> regs[r]
    if (not creator_survives) return;

    assert(r > 0);
    auto& created_by = regs.access(r).created_by;
    auto [s,j] = created_by;
    if (s > 0)
    {
        auto& backward = steps[s].created_regs;
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

void reg_heap::check_back_edges_cleared_for_reg(int r) const
{
    for(auto& [_,index]: regs.access_unused(r).used_regs)
        assert(index == 0);
}

void reg_heap::clear_back_edges_for_step(int s)
{
    // 2. Clear edges from steps[s] <---> reg[call]
    if (steps[s].call > 0)
        clear_call(s);

    // 3. Clear list of created regs.
#ifndef NDEBUG
    for(auto& r: steps[s].created_regs)
        assert(regs.is_free(r));
#endif
    steps[s].created_regs.clear();
}

void reg_heap::clear_step(int r)
{
    assert(not has_result(r));
    int s = prog_steps[r];
  
    if (s > 0)
    {
#ifndef NDEBUG
        check_back_edges_cleared_for_step(s);
#endif
        steps.reclaim_used(s);
    }

    prog_steps[r] = non_computed_index;
}

void reg_heap::clear_result(int r)
{
    prog_results[r] = non_computed_index;
}

const expression_ref& reg_heap::get_reg_value_in_context(int& R, int c)
{
    total_get_reg_value++;
    if (regs.access(R).type == reg::type_t::constant) return expression_at(R);

    total_get_reg_value_non_const++;
    reroot_at_context(c);

    if (has_result(R))
    {
        total_get_reg_value_non_const_with_result++;
        int R2 = result_for_reg(R);
        if (R2) return expression_at(R2);
    }

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    auto [R2, value] = incremental_evaluate_in_context(R,c);
    R = R2;

    return expression_at(value);
}

void reg_heap::set_reg_value_in_context(int P, closure&& C, int c)
{
    int t = switch_to_child_token(c, token_type::set);

    set_reg_value(P, std::move(C), t);
}

bool reg_heap::execution_allowed() const
{
    if (root_token < 0) return false;

    if (tokens[root_token].children.size() == 0) return true;

    if (tokens[root_token].children.size() == 1)
    {
        int t1 = tokens[root_token].children[0];
        return (tokens[t1].type == reverse(token_type::execute));
    }

    return false;
}

pair<int,int> reg_heap::incremental_evaluate_in_context(int R, int c, bool reforce)
{
#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    if (reg_is_constant(R)) return {R,R};

    reroot_at_context(c);

    // Don't create a new token for up-to-date results!
    if (reg_is_changeable(R))
    {
        int r2 = result_for_reg(R);

        if (reforce ? has_force(R) : r2 > 0)
        {
            assert(r2 > 0);
            return {R,r2};
        }
    }

    if (not execution_allowed())
    {
        switch_to_child_token(c, token_type::execute);

        // This should not allow removing the old root token.
        reroot_at_context(c);

        // We can't remove t1 even if its a knuckle.
        assert(execution_allowed());
    }

    assert(execution_allowed());

    auto p = incremental_evaluate(R, reforce);

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    return p;
}

const closure& reg_heap::lazy_evaluate(int& R)
{
    auto [R2, value] = incremental_evaluate(R, false);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate(int& R, int c, bool reforce)
{
    auto [R2, value] = incremental_evaluate_in_context(R, c, reforce);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate_head(int index, int c)
{
    int R1 = heads[index];
    auto [R2, value] = incremental_evaluate_in_context(R1,c);
    if (R2 != R1)
        set_head(index, R2);

    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate_unchangeable(int& R)
{
    R = incremental_evaluate_unchangeable(R);
    return closure_at(R);
}

int reg_heap::get_modifiable_value_in_context(int R, int c)
{
    assert( is_modifiable(expression_at(R)) );
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

// FIXME: We SHOULD be able to do each module in sequences, since there are no
//        transitive dependencies.
//
//        Currently that doesn't work, because things implicitly depend on
//        Foreign.String.unpack_cpp_string to get strings.
//
void reg_heap::allocate_identifiers_for_program()
{
    // 1. Give each identifier a pointer to an unused location; define parameter bodies.
    for(auto& M: *program)
        for(const auto& [name, _]: M.code_defs())
            add_identifier(name);

    // Since the body for any identifier could reference the body for any other, we have to
    // allocate locations for all identifiers before we preprocess the bodies for any.

    // 2. Use these locations to translate these identifiers, at the cost of up to 1 indirection per identifier.
    for(auto& M: *program)
        for(const auto& [name, body]: M.code_defs())
        {
            // get the root for each identifier
            auto loc = identifiers.find(name);
            assert(loc != identifiers.end());
            int R = loc->second;

#ifdef DEBUG_OPTIMIZE
            std::cerr<<name<<" := "<<body<<"\n\n";
            std::cerr<<name<<" := "<<preprocess(body).exp<<"\n\n\n\n";
#endif

            // load the body into the machine
            assert(R != -1);
            set_C(R, preprocess(body) );
        }
}

reg_heap::reg_heap(const Program& P)
    :regs(1,[this](int s){resize(s);}, [this](){collect_garbage();} ),
     steps(1),
     program(new Program(P)),
     args(program->get_module_loader()->args),
     prog_steps(1,non_existant_index),
     prog_results(1, non_existant_index),
     prog_temp(1)
{
    if (not program->size())
        program->add("Prelude");

    allocate_identifiers_for_program();

    if (P.main)
        add_program( var( *P.main ) );

    add_perform_io_head();
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

