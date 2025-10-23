#include <iostream>
#include <range/v3/all.hpp>
#include <algorithm>
#include "util/truncate.H"
#include "util/log-level.H"
#include "util/string/join.H"
#include "util/io/vector.H"
#include "util/set.H"
#include "graph_register.H"
#include "computation/module.H"
#include "computation/expression/core.H"
#include "computation/expression/var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/tuple.H"
#include "computation/expression/modifiable.H"
#include "computation/expression/interchangeable.H"
#include "computation/expression/expression.H" // is_WHNF( )
#include "computation/operations.H"
#include "effect.H"
#include "effects.H"


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

bool Step::has_effect() const
{
    return flags.test(6);
}

void Step::mark_with_effect()
{
    flags.set(6);
}

bool Step::has_pending_effect_registration() const
{
    return flags.test(5);
}

void Step::set_pending_effect_registration()
{
    flags.set(5);
}

void Step::clear_pending_effect_registration()
{
    flags.set(5,false);
}

bool Step::has_pending_effect_unregistration() const
{
    return flags.test(4);
}

void Step::set_pending_effect_unregistration()
{
    flags.set(4);
}

void Step::clear_pending_effect_unregistration()
{
    flags.set(4,false);
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
    assert(not call_edge);
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

void reg::mark_unconditionally_evaluated()
{
    flags.set(reg_is_always_evaluated_bit);
}

bool reg::is_unconditionally_evaluated() const
{
    return flags.test(reg_is_always_evaluated_bit);
}

void reg::clear()
{
    C.clear();
    type = type_t::unevaluated;
    truncate(used_regs);
    truncate(forced_regs);
    truncate(used_by);
    truncate(called_by);
    truncate(created_by_step);
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
    assert(not created_by_step);
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

    created_by_step = std::move(R.created_by_step);

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
     created_by_step( std::move(R.created_by_step) ),
     flags ( R.flags )
{ }

std::optional<int> reg_heap::creator_step_for_reg(int r) const
{
    if (regs[r].created_by_step)
    {
	int s = regs[r].created_by_step.value().first;
	assert(s >= 0);
	return s;
    }
    else
	return {};
}

bool reg_heap::reg_is_contingent(int r) const
{
    return (bool)creator_step_for_reg(r);
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
    auto s = creator_step_for_reg(r);
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
    assert(regs.size() == prog_unshare.size());
    return regs.size();
}

log_double_t reg_heap::probability_for_context(int c)
{
    total_context_pr++;

    return prior_for_context(c) * likelihood_for_context(c);
}

int reg_heap::follow_index_var(int r) const
{
    while(expression_at(r).is_index_var())
        r = closure_at(r).reg_for_index_var();
    return r;
}

int reg_heap::follow_index_var_target(int r) const
{
    assert(not reg_is_unevaluated(r));

    assert(not reg_is_index_var_no_force(r));

    if (reg_is_index_var_with_force(r))
    {
	r = closure_at(r).reg_for_index_var();
	assert(regs.is_free(r) or not reg_is_unevaluated(r));
    }

    assert(not expression_at(r).is_index_var());

    return r;
}

int reg_heap::follow_index_var_no_force(int r) const
{
    while(reg_is_index_var_no_force(r))
        r = closure_at(r).reg_for_index_var();
    return r;
}

void reg_heap::mark_effect_to_register_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Step must have not be on pending reg list
    assert(not steps[s].has_pending_effect_registration());

    steps[s].set_pending_effect_registration();

    steps_pending_effect_registration.insert(s);
}

void reg_heap::unmark_effect_to_register_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Step must have be on pending reg list
    assert(steps[s].has_pending_effect_registration());
    assert(steps_pending_effect_registration.count(s));

    // Remove step from pending reg list
    steps_pending_effect_registration.erase(s);
    steps[s].clear_pending_effect_registration();
}

void reg_heap::mark_effect_to_unregister_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Step must have not be on pending unreg list
    assert(not steps[s].has_pending_effect_unregistration());

    steps[s].set_pending_effect_unregistration();

    steps_pending_effect_unregistration.insert(s);
}

void reg_heap::unmark_effect_to_unregister_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Step must have be on pending unreg list
    assert(steps[s].has_pending_effect_unregistration());
    assert(steps_pending_effect_unregistration.count(s));

    // Remove step from pending unreg list
    steps_pending_effect_unregistration.erase(s);
    steps[s].clear_pending_effect_unregistration();
}

void reg_heap::register_effect_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Remove step from pending-reg list if it is there.
    if (steps[s].has_pending_effect_registration())
        unmark_effect_to_register_at_step(s);
    else
        assert(not steps_pending_effect_registration.count(s));

    _register_effect_at_reg(steps[s].call, s);
}

void reg_heap::unregister_effect_at_step(int s)
{
    // Step must have effect
    assert(steps[s].has_effect());

    // Remove step from pending-unreg list if it is there.
    if (steps[s].has_pending_effect_unregistration())
        unmark_effect_to_unregister_at_step(s);
    else
        assert(not steps_pending_effect_unregistration.count(s));

    _unregister_effect_at_reg(steps[s].call, s);
}

void reg_heap::_register_effect_at_reg(int r, int s)
{
    assert(closure_at(r).exp.head().is_a<effect>() or closure_at(r).exp.head().is_a<constructor>());
    auto& E = closure_at(r).exp.head();

    if (E.is_a<::register_prior>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<":   REGISTER! ("<<prior_terms.size()<<" -> "<<prior_terms.size()+1<<")\n";
        register_prior(E.as_<::register_prior>(), s);
    }
    else if (E.is_a<register_likelihood>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<":   REGISTER! ("<<likelihood_terms.size()<<" -> "<<likelihood_terms.size()+1<<")\n";
        register_likelihood_(E.as_<register_likelihood>(), s);
    }
    else if (auto I = E.to<RegisterInterchangeable>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<":   REGISTER!\n";
        register_interchangeable(*I, s);
    }
    else if (E.is_a<constructor>())
    {
        if (has_constructor(E, "Effect.TransitionKernel"))
        {
            double rate = expression_at(r).sub()[0].as_double();
            int r_kernel = closure_at(r).reg_for_slot(1);
            if (log_verbose >= 5)
                std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<r_kernel<<",id="<<s<<"]: REGISTER!\n";
            register_transition_kernel(r, s);
        }
        else if (has_constructor(E, "Effect.Logger"))
        {
            int r_logger = closure_at(r).reg_for_slot(0);
            if (log_verbose >= 5)
                std::cerr<<"register_logger[logger="<<r_logger<<"]: REGISTER!\n";
            register_logger(r, s);
        }
        else if (has_constructor(E, "Effect.InEdge"))
        {
            if (log_verbose >= 5) std::cerr<<E<<": REGISTER!\n";
            register_in_edge(r, s);
        }
        else if (has_constructor(E, "Effect.OutEdge"))
        {
            if (log_verbose >= 5) std::cerr<<E<<":  REGISTER!\n";
            register_out_edge(r, s);
        }
        else if (has_constructor(E, "Effect.Dist"))
        {
            if (log_verbose >= 5) std::cerr<<E<<":  REGISTER!\n";
            register_dist(r, s);
        }
        else if (has_constructor(E, "Effect.DistProperty"))
        {
            if (log_verbose >= 5) std::cerr<<E<<": REGISTER!\n";
            register_dist_property(r, s);
        }
        else
            throw myexception()<<"register_effect_at_reg("<<r<<","<<s<<"): unknown effect "<<E;
    }
    else
        throw myexception()<<"register_effect_at_reg("<<r<<","<<s<<"): unknown effect "<<E;
}

void reg_heap::_unregister_effect_at_reg(int r, int s)
{
    assert(closure_at(r).exp.head().is_a<effect>() or closure_at(r).exp.head().is_a<constructor>());
    auto& E = closure_at(r).exp.head();

    if (E.is_a<::register_prior>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<": UNregister! ("<<prior_terms.size()<<" -> "<<prior_terms.size()-1<<")\n";

        unregister_prior(E.as_<::register_prior>(), s);
    }
    else if (E.is_a<register_likelihood>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<": UNregister! ("<<likelihood_terms.size()<<" -> "<<likelihood_terms.size()-1<<")\n";
        unregister_likelihood_(E.as_<register_likelihood>(), s);
    }
    else if (auto I = E.to<RegisterInterchangeable>())
    {
        if (log_verbose >= 5)
            std::cerr<<E.print()<<":   UNREGISTER!\n";
        unregister_interchangeable(*I, s);
    }
    else if (E.is_a<constructor>())
    {
        if (has_constructor(E, "Effect.TransitionKernel"))
        {
            double rate = expression_at(r).sub()[0].as_double();
            int r_kernel = closure_at(r).reg_for_slot(1);
            if (log_verbose >= 5)
                std::cerr<<"register_transition_kernel[rate="<<rate<<",kernel="<<r_kernel<<",id="<<s<<"]: UNREGISTER!\n";
            unregister_transition_kernel(r, s);
        }
        else if (has_constructor(E, "Effect.Logger"))
        {
            int r_logger = closure_at(r).reg_for_slot(0);
            if (log_verbose >= 5)
                std::cerr<<"register_logger[logger="<<r_logger<<"]: UNREGISTER!\n";
            unregister_logger(r, s);
        }
        else if (has_constructor(E, "Effect.InEdge"))
        {
            if (log_verbose >= 5)std::cerr<<E<<": UNREGISTER!\n";
            unregister_in_edge(r, s);
        }
        else if (has_constructor(E, "Effect.OutEdge"))
        {
            if (log_verbose >= 5) std::cerr<<E<<": UNREGISTER!\n";
            unregister_out_edge(r, s);
        }
        else if (has_constructor(E, "Effect.Dist"))
        {
            if (log_verbose >= 5) std::cerr<<E<<": UNREGISTER!\n";
            unregister_dist(r, s);
        }
        else if (has_constructor(E, "Effect.DistProperty"))
        {
            if (log_verbose >= 5) std::cerr<<E<<": UNREGISTER!\n";
            unregister_dist_property(r, s);
        }
        else
            throw myexception()<<"register_effect_at_reg("<<r<<","<<s<<"): unknown effect "<<E;
    }
    else
        throw myexception()<<"unregister_effect_at_reg("<<r<<","<<s<<"): unknown effect "<<E;
}

bool reg_heap::step_has_effect(int s) const
{
    assert(not steps.is_free(s));
    return steps[s].has_effect();
}

const closure& reg_heap::get_effect(int s) const
{
    assert(not steps.is_free(s));
    int r = steps[s].call;
    assert(steps[s].has_effect());

    return closure_at(r);
}

std::set<int> reg_heap::find_affected_sampling_events(int c, const std::function<void(void)>& do_changes)
{
    std::set<int> downstream_sampling_events;

    auto register_sampling_event = [&](const register_prob& r,int) { downstream_sampling_events.insert(r.r_dist);};

    unregister_likelihood_handlers.push_back(register_sampling_event);
    unregister_prior_handlers.push_back(register_sampling_event);

    do_changes();
    reroot_at_context(c);
    do_pending_effect_unregistrations();

    unregister_prior_handlers.pop_back();
    unregister_likelihood_handlers.pop_back();

    return downstream_sampling_events;
}

void reg_heap::do_pending_effect_registrations()
{
    // Don't modify `steps_pending_effect_registration` while we are walking it!
    auto v = steps_pending_effect_registration | ranges::to<vector>;
    for(int s: v)
    {
        assert(steps[s].has_pending_effect_registration());
        assert(steps_pending_effect_registration.count(s));
        register_effect_at_step(s);
    }

    assert(steps_pending_effect_registration.empty());
}

void reg_heap::do_pending_effect_unregistrations()
{
    // Don't modify `steps_pending_effect_unregistration` while we are walking it!
    auto v = steps_pending_effect_unregistration | ranges::to<vector>;
    for(int s: v)
    {
        assert(steps[s].has_pending_effect_unregistration());
        assert(steps_pending_effect_unregistration.count(s));
        unregister_effect_at_step(s);
    }

    assert(steps_pending_effect_unregistration.empty());
}

void reg_heap::compute_initial_force_counts()
{
    vector<int> forced_regs;

    auto force_reg = [&,this](int r)
        {
            if (prog_force_counts[r] == 0)
                forced_regs.push_back(r);
            prog_force_counts[r]++;
        };

    int r = heads[*program_result_head];
    if (reg_is_changeable_or_forcing(r))
    {
        force_reg(r);
        assert(steps.size() > 0);
    }

    for(int i=0;i<forced_regs.size();i++)
    {
        int r = forced_regs[i];
        auto& R = regs[r];

        // 3a. Count uses
        for(auto [ur,_,__]: R.used_regs)
            force_reg(ur);

        // 3b. Count forces
        for(auto fr: R.forced_regs)
            force_reg(fr);

        if (has_step1(r))
        {
            int call = step_for_reg(r).call;
            assert(call);
            if (reg_is_changeable_or_forcing(call))
                force_reg(call);
        }
    }
}


void reg_heap::mark_unconditional_regs()
{
    if (not program_result_head) return;

    int program_result_reg = heads[*program_result_head];
    if (not reg_is_changeable_or_forcing(program_result_reg)) return;

    auto & unconditionally_evaluated_regs = get_scratch_list();

    auto use_reg_unconditionally = [&](int r)
    {
        if (not regs[r].is_unconditionally_evaluated())
        {
            regs[r].mark_unconditionally_evaluated();
            unconditionally_evaluated_regs.push_back(r);
        }
    };

    use_reg_unconditionally(program_result_reg);

    for(int i=0;i<unconditionally_evaluated_regs.size();i++)
    {
        int r = unconditionally_evaluated_regs[i];
        auto & R = regs[r];

        // Mark used regs
        for(auto [fr, ur, _]: R.used_regs)
	{
            use_reg_unconditionally(fr);
            use_reg_unconditionally(ur);
	}

        // Mark force regs
        for(auto fr: R.forced_regs)
            use_reg_unconditionally(fr);
    }

    release_scratch_list(); // unconditionally_evaluated_regs
}

void reg_heap::first_evaluate_program(int r_prog, int r_log, int c)
{
    if (program_result_head or logging_head)
        throw myexception()<<"Trying first_evaluate_program for a second time!";

    program_result_head = add_compute_expression(reg_var(r_prog));

    logging_head = add_compute_expression(reg_var(r_log));

    assert(get_prev_prog_token_for_context(c));

    // 1. Execute with reforce = true.  (For the first execution, this shouldn't matter though.)
    assert(token_for_context(c) == root_token);
    auto [program_result_reg, _] = incremental_evaluate1(heads[*program_result_head]);
    heads[*program_result_head] = program_result_reg;

    assert(get_prev_prog_token_for_context(c));
    assert(is_program_execution_token(*get_prev_prog_token_for_context(c)));

    // 2. Nothing to unmap!

    // 3. Perform any pending registration or unregistration of effects.
    do_pending_effect_registrations();
    assert(steps_pending_effect_unregistration.empty());

    // 4. Compute initial force counts
    compute_initial_force_counts();

    // 5. Mark unconditionally-evaluated regs.
    mark_unconditional_regs();

#ifdef DEBUG_MACHINE
    check_force_counts();
#endif
}

bool reg_heap::simple_set_path_to(int child_token) const
{
    assert(token_is_used(child_token));

    for(int t = child_token; not tokens[t].is_root(); t = parent_token(t))
    {
        if (directed_token_type(t) != token_type::set) return false;

        if (t != child_token and tokens[t].children.size() != 1) return false;
    }

    return true;
}

vector<set_interchange_op> reg_heap::find_set_regs_on_path(int child_token) const
{
    assert(token_is_used(child_token));

    vector<set_interchange_op> reg_values;
    for(int t = child_token; not tokens[t].is_root(); t = parent_token(t))
    {
        assert(directed_token_type(t) != token_type::reverse_set);
        assert(directed_token_type(t) != token_type::reverse_set_unshare);
        if (directed_token_type(t) == token_type::set or directed_token_type(t) == token_type::set_unshare)
        {
            if (tokens[t].n_modifiables_set > 0)
            {
                // If this is a set token, there shouldn't be anything else beyond n_modifiables_set entries.
                assert(directed_token_type(t) != token_type::set or tokens[t].vm_step.delta().size() == tokens[t].n_modifiables_set);
                // If this is a set_unshare token, there should additionally be the results of unsharing.
                assert(directed_token_type(t) != token_type::set_unshare or tokens[t].vm_step.delta().size() >= tokens[t].n_modifiables_set);

                for(int i=0;i<tokens[t].n_modifiables_set;i++)
                {
                    auto& [r,s] = tokens[t].vm_step.delta()[i];
                    assert(is_modifiable(expression_at(r)));
                    assert(s > 0);
                    int call = steps[s].call;
                    auto value = closure_at(call);
                    assert(value.exp.is_atomic());
                    reg_values.push_back(set_op{r, value});
                }
            }
            else
            {
                // If this is a set token, there shouldn't be anything else beyond [0] and [1].
                assert(directed_token_type(t) != token_type::set or tokens[t].vm_step.delta().size() == 2);
                // If this is a set_unshare token, there should additionally be the results of unsharing.
                assert(directed_token_type(t) != token_type::set_unshare or tokens[t].vm_step.delta().size() >= 2);

                auto [r1, s1] = tokens[t].vm_step.delta()[0];
                assert(is_interchangeable(expression_at(r1)));
                auto [r2,_] = tokens[t].vm_step.delta()[1];
                assert(is_interchangeable(expression_at(r2)));
                reg_values.push_back(interchange_op{r1,r2});
            }
        }
    }

    std::reverse(reg_values.begin(), reg_values.end());

    return reg_values;
}

void reg_heap::check_force_counts() const
{
    vector<int> true_force_counts(regs.size(),0);

    if (program_result_head)
    {
	int h = heads[*program_result_head];;
	if (reg_is_changeable_or_forcing(h))
	    true_force_counts[h]++;
    }

    for(auto i = regs.begin(); i != regs.end(); i++)
    {
	int r = i.addr();

	// Skip if prog_force_counts is zero.
	if (prog_force_counts[r] == 0) continue;

	for(auto& [r2,_,__]: regs[r].used_regs)
	    true_force_counts[r2]++;

	for(auto& r2: regs[r].forced_regs)
	    true_force_counts[r2]++;

	if (reg_is_changeable(r))
	{
	    assert(has_step1(r));

	    int call = step_for_reg(r).call;

	    if (reg_is_changeable_or_forcing(call))
		true_force_counts[call]++;
	}
    }

    for(int r=1;r<regs.size();r++)
    {
        assert(prog_unshare[r].none());
        if (not regs.is_free(r))
	{
            assert(prog_force_counts[r] == true_force_counts[r]);
	}
        if (has_step1(r) and not reg_is_unforgettable(r))
            assert(prog_force_counts[r] > 0);
        if (prog_force_counts[r] > 0)
            assert(reg_has_value(r));
    }
}

int reg_heap::force_simple_set_path_to_PPET(int c)
{
    // 1. Reroot to the PPET
    int t = token_for_context(c);
    int PPET = tokens[t].prev_prog_token->token;
    reroot_at_token(PPET);

    // 2. Ensure that the path from the PPET is composed only of SET tokens!
    if (not simple_set_path_to(t))
    {
        // What if the path starts with a set?  We could keep that part.

        auto reg_values = find_set_regs_on_path(t);
        set_token_for_context(c, PPET);
        for(auto& op: reg_values)
        {
            if (std::holds_alternative<set_op>(op))
            {
                auto [reg,value] = std::get<set_op>(op);
                set_reg_value_in_context(reg, std::move(value), c);
            }
            else if (std::holds_alternative<interchange_op>(op))
            {
                auto [r1, r2] = std::get<interchange_op>(op);
                interchange_regs_in_context_(r1, r2, c);
            }
        }
        t = token_for_context(c);
    }

    assert( simple_set_path_to(t) );

    return t;
}




expression_ref reg_heap::unshare_and_evaluate_program(int c)
{
    // 1. Reroot to the PPET
    int t = token_for_context(c);
    int PPET = tokens[t].prev_prog_token->token;
    reroot_at_token(PPET);
    assert(not tokens[t].is_root());

    // 2. Find all equivalent contexts and revert them to the most recent non-execute token.
    t = revert_token(t);
    for(int c2: equivalent_contexts(c))
	set_token_for_context(c2,t);

    // 3. Refuse to execute if there are children!!!
    if (not tokens[t].children.empty())
    {
	std::cerr<<"unshare_and_evaluate_program("<<c<<"): executing token "<<t<<" that has descendants!";
	std::abort();
    }

    // 4. Reroot to the PPET
    auto cs = tokens[token_for_context(c)].context_refs;
    t = force_simple_set_path_to_PPET(c);
    for(int c: cs)
	set_token_for_context(c, t);

    // 5. Merge the set tokens and all the result an execute token.

    // NOTE: This creates merged SET tokens, which violates the assumptions of find_set_regs_on_path( ).
    //       Therefore we need to ensure that find_set_regs_on_path( ) never sees these.
    release_knuckle_tokens(t);
    assert(is_root_token(parent_token(t)));
    tokens[t].utype = token_type::execute2;

    // 6. Unshare regs in the token.
    auto result = unshare_regs2(t);

    assert(get_prev_prog_token_for_context(c));
    assert(is_program_execution_token(*get_prev_prog_token_for_context(c)));

    return result;
}

expression_ref reg_heap::evaluate_program(int c)
{
    check_tokens();

    if (not program_result_head)
        throw myexception()<<"No program has been set!";

    assert(get_prev_prog_token_for_context(c));

    expression_ref result;

    // 1. If we can revert to a previously executed program instead of unmapping, then do that.
    int t = token_for_context(c);
    if (tokens[t].prev_prog_token->can_revert)
    {
        int t2 = *get_prev_prog_token_for_context(c);

        // Check that there are only execution tokens on the path to the previous evaluation token.
        assert(is_program_execution_token(t2));

        // Revert to previous program token
        set_token_for_context(c, t2);
        reroot_at_context(c);

        // Check that the program head is evaluated.
        int r = heads[*program_result_head];
        assert(reg_has_value(r));

        result = value_for_precomputed_reg(r).exp;
    }
    else
    {
        // 2. Actually evaluate the program.
        unshare_and_evaluate_program(c);
    }

#if DEBUG_MACHINE >= 2
    check_force_counts();
#endif

    // 4. Perform any pending registration or unregistration of effects.
    do_pending_effect_registrations();
    assert(steps_pending_effect_unregistration.empty());
    assert(steps_pending_effect_registration.empty());

    return result;
}

prob_ratios_t reg_heap::probability_ratios(int c1, int c2)
{
#if DEBUG_MACHINE >= 2
    for(auto x : prog_temp)
        assert(x.none());
#endif

    // 1. reroot to c1 and force the program
    evaluate_program(c1);

    // 2. install handlers for register_dist, register_prior, and register_likelihood
    std::unordered_set<int> random_vars_removed;
    std::unordered_set<int> random_vars_added;

    std::unordered_map<int,pair<int,log_double_t>> priors1;
    std::unordered_map<int,pair<int,log_double_t>> priors2;

    std::unordered_map<int,log_double_t> likelihoods1;
    std::unordered_map<int,log_double_t> likelihoods2;

    auto register_prior_handler = [&](const register_prob& E, int)
    {
        priors2.insert({E.r_prob, {E.r_dist,E.prob}});
    };

    auto unregister_prior_handler = [&](const register_prob& E, int)
    {
        priors1.insert({E.r_prob, {E.r_dist, E.prob}});
    };

    auto register_likelihood_handler = [&](const register_prob& E, int)
    {
        likelihoods2.insert({E.r_prob, E.prob});
    };

    auto unregister_likelihood_handler = [&](const register_prob& E, int)
    {
        likelihoods1.insert({E.r_prob, E.prob});
    };

    auto register_dist_handler = [&](int r, int)
    {
        int r_dist = closure_at(r).reg_for_slot(0);
        int observation = expression_at(r).sub()[1].as_int();
        if (not observation)
            random_vars_added.insert(r_dist);
    };

    auto unregister_dist_handler = [&](int r, int)
    {
        int r_dist = closure_at(r).reg_for_slot(0);
        int observation = expression_at(r).sub()[1].as_int();
        if (not observation)
            random_vars_removed.insert(r_dist);
    };

    register_likelihood_handlers.push_back(register_likelihood_handler);
    unregister_likelihood_handlers.push_back(unregister_likelihood_handler);
    register_prior_handlers.push_back(register_prior_handler);
    unregister_prior_handlers.push_back(unregister_prior_handler);
    register_dist_handlers.push_back(register_dist_handler);
    unregister_dist_handlers.push_back(unregister_dist_handler);

    // 3. reroot to c2 and force the program
    evaluate_program(c2);

    // 4. compute the ratio only for (i) changed pdfs that (ii) exist in both c1 and c2
    prob_ratios_t R;
    R.variables_changed = (not random_vars_added.empty()) or (not random_vars_removed.empty());

    for(auto [r_pdf1, r_dist_and_pdf1]: priors1)
    {
        auto& [r_dist1, pdf1] = r_dist_and_pdf1;

        if (random_vars_removed.count(r_dist1))
        {
            assert(not random_vars_added.count(r_dist1));
            continue;
        }

        auto it2 = priors2.find(r_pdf1);

        if (it2 == priors2.end())
            R.prior_ratio /= pdf1;
        else
        {
            auto& [r_dist2, pdf2] = it2->second;
            assert(r_dist2 == r_dist1);
            R.prior_ratio *= (pdf2 / pdf1);
        }
    }

    for(auto [r_pdf2, r_dist_and_pdf2]: priors2)
    {
        auto& [r_dist2, pdf2] = r_dist_and_pdf2;

        if (random_vars_added.count(r_dist2))
        {
            assert(not random_vars_removed.count(r_dist2));
            continue;
        }

        if (not priors2.count(r_pdf2))
            R.prior_ratio *= pdf2;
    }

    for(auto [r_likelihood, likelihood1]: likelihoods1)
    {
        auto it2 = likelihoods2.find(r_likelihood);

        if (it2 == likelihoods2.end())
            R.likelihood_ratio /= likelihood1;
        else
        {
            auto likelihood2 = it2->second;
            R.likelihood_ratio *= (likelihood2 / likelihood1);
        }
    }

    for(auto [r_likelihood, likelihood2]: likelihoods2)
    {
        auto it1 = likelihoods1.find(r_likelihood);

        if (it1 == likelihoods1.end())
            R.likelihood_ratio *= likelihood2;
    }

#if DEBUG_MACHINE >= 2
    for(auto x : prog_temp)
        assert(x.none());
#endif

    // 5. remove the handlers
    register_likelihood_handlers.pop_back();
    unregister_likelihood_handlers.pop_back();
    register_prior_handlers.pop_back();
    unregister_prior_handlers.pop_back();
    register_dist_handlers.pop_back();
    unregister_dist_handlers.pop_back();

//  auto L2 = likelihood_for_context(c2);
//
//  If L1 and L2 are off far enough, this test will fail...
//  if (L1 > 0.0 and L2 > 0.0)
//      assert( std::abs( (L2/L1).log() - R.likelihood_ratio.log()) < 1.0e-4 );


    return R;
}

void reg_heap::register_likelihood_(const register_prob& E, int s)
{
    assert(not likelihood_terms.count(s));
    likelihood_terms.insert({s,E});
    for(auto& handler: register_likelihood_handlers)
        handler(E, s);
}

void reg_heap::unregister_likelihood_(const register_prob& E, int s)
{
    assert(likelihood_terms.count(s));
    likelihood_terms.erase(s);
    // FIXME: run these in reverse order?
    for(auto& handler: unregister_likelihood_handlers)
        handler(E, s);
}

void reg_heap::register_prior(const register_prob& E, int s)
{
    // We aren't supposed to ever register the same step twice.
    assert(not prior_terms.count(s));
    prior_terms.insert({s,E});
    for(auto& handler: register_prior_handlers)
        handler(E, s);
}

void reg_heap::unregister_prior(const register_prob& E, int s)
{
    assert(prior_terms.count(s));
    prior_terms.erase(s);

    // FIXME: run these in reverse order?
    for(auto& handler: unregister_prior_handlers)
        handler(E, s);
}

void reg_heap::register_interchangeable(const RegisterInterchangeable& I, int /*s*/)
{
    if (not interchangeables.count(I.id))
        interchangeables.insert({I.id, {}});

    auto& i_regs = interchangeables.at(I.id);

    assert(not i_regs.count(I.r_interchangeable));

    i_regs.insert(I.r_interchangeable);
}

void reg_heap::unregister_interchangeable(const RegisterInterchangeable& I, int /*s*/)
{
    auto& i_regs = interchangeables.at(I.id);

    assert(i_regs.count(I.r_interchangeable));

    i_regs.erase(I.r_interchangeable);

    if (i_regs.empty())
        interchangeables.erase(I.id);
}

void reg_heap::register_transition_kernel(int r, int s)
{
    assert(not steps.is_free(s));

    double rate = expression_at(r).sub()[0].as_double();
    int r_kernel = closure_at(r).reg_for_slot(1);

    assert(reg_is_constant(r_kernel));

    // Multiple steps from different contexts COULD register the same transition kernel.
    assert(not transition_kernels_.count(s));

    if (rate > 0)
    {
        transition_kernels_.insert(s);

        for(auto& handler: register_tk_handlers)
            handler(r,s);
    }
}

void reg_heap::unregister_transition_kernel(int r, int s)
{
    double rate = expression_at(r).sub()[0].as_double();
    int r_kernel = closure_at(r).reg_for_slot(1);

    if (rate > 0)
    {
        for(auto& handler: unregister_tk_handlers)
            handler(r,s);

        if (not transition_kernels_.count(s))
            throw myexception()<<"unregister_transition_kernel: transition kernel <r="<<r_kernel<<",s="<<s<<"> not found!";

        transition_kernels_.erase(s);
    }
    assert(not transition_kernels_.count(s));
}

const std::unordered_set<int>& reg_heap::transition_kernels() const
{
    return transition_kernels_;
}

void reg_heap::register_logger(int r, int s)
{
    assert(not steps.is_free(s));

    int r_logger = closure_at(r).reg_for_slot(0);

    assert(reg_is_constant(r_logger));

    // Multiple steps from different contexts COULD register the same transition kernel.
    assert(not loggers_.count(s));

    loggers_.insert(s);
}

void reg_heap::unregister_logger(int r, int s)
{
    int r_logger = closure_at(r).reg_for_slot(0);

    if (not loggers_.count(s))
        throw myexception()<<"unregister_logger: logger <r="<<r_logger<<",s="<<s<<"> not found!";

    loggers_.erase(s);

    assert(not loggers_.count(s));
}

const std::unordered_set<int>& reg_heap::loggers() const
{
    return loggers_;
}

optional<int> reg_heap::compute_expression_is_modifiable_reg(int index)
{
    int& H = heads[index];

    return find_update_modifiable_reg(H);
}

void reg_heap::register_in_edge(int r, int /* s */)
{
    // NOTE: the source node is lazy -- it could be an index-var
    // int r_from_node = closure_at(r).reg_for_slot(0);
    int r_to_dist   = expression_at(r).sub()[1].as_int();
    string arg_name = expression_at(r).sub()[2].as_<String>();

    // 1. Check that this edge is not a duplicate:

    //   Only one in-edges to this dist should have this arg_name.
    auto& in_edges_to_this_dist = in_edges_to_dist[r_to_dist];
    assert(not in_edges_to_this_dist.count(arg_name));

    // 2. Check that there is in fact a distribution at r_to_dist;
    assert(has_constructor(expression_at(r_to_dist), "Effect.Dist"));
    // assert(dist_type.count(r_to_dist));

    // 3. Insert the edge.
    in_edges_to_this_dist.insert({arg_name, r}); // r_to_dist -> arg_name -> r
}

void reg_heap::unregister_in_edge(int r, int /* s */)
{
    // NOTE: the source node is lazy -- it could be an index-var
    // int r_from_node = closure_at(r).reg_for_slot(0);
    int r_to_dist   = expression_at(r).sub()[1].as_int();
    string arg_name = expression_at(r).sub()[2].as_<String>();

    // Check that this edge is registered.
    assert(in_edges_to_dist.count(r_to_dist));
    auto& in_edges_to_this_dist = in_edges_to_dist.at(r_to_dist);
    assert(in_edges_to_this_dist.count(arg_name));

    // 2. Check that there is in fact a distribution at to_reg?
    assert(has_constructor(expression_at(r_to_dist), "Effect.Dist"));
    // assert(dist_type.count(r_to_dist));

    // 3. Erase the edge
    in_edges_to_this_dist.erase(arg_name);
    if (in_edges_to_this_dist.empty())
    {
        // Erase the map, if there are no more edges to this dist.
        in_edges_to_dist.erase(r_to_dist);
    }
}

void reg_heap::register_out_edge(int r, int /* s */)
{
    int r_from_dist = closure_at(r).reg_for_slot(0);
    int r_to_var    = closure_at(r).reg_for_slot(1);

    // Check that this edge is not a duplicate.
    assert(not out_edges_from_dist.count(r_from_dist));
    assert(not out_edges_to_var.count(r_to_var) or not out_edges_to_var.at(r_to_var).count(r_from_dist));
    
    // Check that there is in fact a distribution at I.to_reg.
    assert(has_constructor(expression_at(r_from_dist), "Effect.Dist"));
    // assert(dist_type.count(O.r_from_dist));
    assert(reg_is_constant(r_to_var) or (reg_is_changeable(r_to_var) and is_modifiable(expression_at(r_to_var))));

    out_edges_from_dist.insert({r_from_dist, r_to_var});
    out_edges_to_var[r_to_var].insert(r_from_dist);

    // Check that this edge is registered.
    assert(out_edges_from_dist.count(r_from_dist));
    assert(out_edges_to_var.count(r_to_var) and out_edges_to_var.at(r_to_var).count(r_from_dist));
}

void reg_heap::unregister_out_edge(int r, int /* s */)
{
    int r_from_dist = closure_at(r).reg_for_slot(0);
    int r_to_var    = closure_at(r).reg_for_slot(1);

    // Check that this edge is registered.
    assert(out_edges_from_dist.count(r_from_dist));
    assert(out_edges_to_var.count(r_to_var) and out_edges_to_var.at(r_to_var).count(r_from_dist));

    // Check that there is in fact a distribution at r_from_dist.
    assert(has_constructor(expression_at(r_from_dist), "Effect.Dist"));
    // assert(dist_type.count(r_from_dist));

    // Erase the edge
    out_edges_from_dist.erase(r_from_dist);
    auto& to_var = out_edges_to_var.at(r_to_var);
    to_var.erase(r_from_dist);
    if (to_var.empty())
        out_edges_to_var.erase(r_to_var);

    // Check that this edge is not registered.
    assert(not out_edges_from_dist.count(r_from_dist));
    assert(not out_edges_to_var.count(r_to_var) or not out_edges_to_var.at(r_to_var).count(r_from_dist));
}

void reg_heap::register_dist(int r, int s)
{
    int r_dist = closure_at(r).reg_for_slot(0);
//    int observation = expression_at(r).sub()[1].as_<String>();
    const string& name = expression_at(r).sub()[2].as_<String>();

//    assert(not dist_type.count(r_dist));

    dist_type.insert({r_dist, name});

    for(auto& handler: register_dist_handlers)
        handler(r,s);
}

void reg_heap::unregister_dist(int r, int s)
{
    int r_dist = closure_at(r).reg_for_slot(0);

    assert(has_constructor(expression_at(r_dist), "Effect.Dist"));
//    assert(dist_type.count(r_dist));

    dist_type.erase(r_dist);

    for(auto& handler: unregister_dist_handlers)
        handler(r,s);
}

void reg_heap::register_dist_property(int r, int /* s */)
{
    int r_from_dist = expression_at(r).sub()[0].as_int();
    const string& property =  expression_at(r).sub()[1].as_<String>();
    // NOTE: the target (property) reg is lazy -- it could be an index-var.
    // int r_to_prop   = closure_at(r).reg_for_slot(2);

    // Check that there is in fact a distribution at P.s_from_dist.
    assert(has_constructor(expression_at(r_from_dist), "Effect.Dist"));
    // assert(dist_type.count(r_from_dist));

    dist_properties[r_from_dist].insert({property, r});
}

void reg_heap::unregister_dist_property(int r, int /* s */)
{
    int r_from_dist = expression_at(r).sub()[0].as_int();
    const string& property =  expression_at(r).sub()[1].as_<String>();

    // Check that there is in fact a distribution at P.r_from_dist.
    assert(has_constructor(expression_at(r_from_dist), "Effect.Dist"));
    // assert(dist_type.count(r_from_dist));

    auto& this_dist_properties = dist_properties.at(r_from_dist);
    auto it = this_dist_properties.find(property);
    assert(it != this_dist_properties.end());
    this_dist_properties.erase(it);

    if (this_dist_properties.empty())
        dist_properties.erase(r_from_dist);
}

optional<int> reg_heap::find_update_modifiable_reg(int& R)
{
    // Note: here we always update R
    R = incremental_evaluate_unchangeable(R);

    auto& C = (*this)[R];

    if (is_modifiable(C.exp))
        return R;
    else
        return {};

    // This does NOT handle index_var_with_force!
}

optional<int> reg_heap::find_modifiable_reg(int R)
{
    return find_update_modifiable_reg(R);
}

optional<int> reg_heap::find_precomputed_const_or_modifiable_reg_in_context(int r, int c)
{
    reroot_at_context(c);
    return find_precomputed_const_or_modifiable_reg(r);
}

// This is an evaluation loop that follows calls instead of results
// so that it doesn't jump over modifiables.
optional<int> reg_heap::find_precomputed_const_or_modifiable_reg(int r)
{
    while(true)
    {
        auto& C = closure_at(r);

        if (reg_is_unevaluated(r))  // 0
            return {};
        else if (reg_is_index_var_no_force(r) or reg_is_index_var_with_force(r)) // 1, 4, 5
            r = C.reg_for_index_var();
        else if (reg_is_constant(r)) // 2, 6
            return r;
        else if (reg_is_changeable(r)) // 3
        {
            if (is_modifiable(C.exp))
                return r;
            else if (not reg_has_call(r))
                return {};
            else
                r = call_for_reg(r);
        }
        else
            std::abort();
    }

    // unreachable
}

// This is an evaluation loop that follows calls instead of results
// so that it doesn't jump over modifiables.
optional<int> reg_heap::find_precomputed_modifiable_reg_in_context(int r, int c)
{
    reroot_at_context(c);

    while(true)
    {
        auto& C = closure_at(r);

        if (reg_is_unevaluated(r))  // 0
            return {};
        // Here we follow unforced index_var_with_force
        else if (reg_is_index_var_no_force(r) or reg_is_index_var_with_force(r)) // 1, 4, 5
            r = C.reg_for_index_var();
        else if (reg_is_constant(r)) // 2, 6
            return {};
        else if (reg_is_changeable(r)) // 3
        {
            if (is_modifiable(C.exp))
            {
                // We might want to set the call for an unforced modifiable in the tree.
                return r;
            }
            else if (not reg_has_call(r))
                return {};
            else
                r = call_for_reg(r);
        }
        else
            std::abort();
    }

    // unreachable
}

// This is an evaluation loop that follows calls instead of results
// so that it doesn't jump over modifiables.
optional<int> reg_heap::find_precomputed_interchangeable_reg_in_context(int r, int c)
{
    reroot_at_context(c);

    return find_precomputed_interchangeable_reg(r);
}

// This is an evaluation loop that follows calls instead of results
// so that it doesn't jump over modifiables.
optional<int> reg_heap::find_precomputed_interchangeable_reg(int r)
{
    while(true)
    {
        auto& C = closure_at(r);

        if (reg_is_unevaluated(r))  // 0
            return {};
        // Here we follow unforced index_var_with_force
        else if (reg_is_index_var_no_force(r) or reg_is_index_var_with_force(r)) // 1, 4, 5
            r = C.reg_for_index_var();
        else if (reg_is_constant(r)) // 2, 6
            return {};
        else if (reg_is_changeable(r)) // 3
        {
            if (is_interchangeable(C.exp))
            {
                assert(reg_has_call(r));
                assert(reg_is_forced(r));
                return r;
            }
            else if (not reg_has_call(r))
                return {};
            else
                r = call_for_reg(r);
        }
        else
            std::abort();
    }

    // unreachable
}

// We used to perform this execution in a new context, and then
// roll it back.  However, this meant that when doing random samples
// during proposals, a random sample could be lost and re-sampled during
// evaluation of another argument in the same context.

optional<int> reg_heap::find_modifiable_reg_in_context(int R, int c1)
{
    R = follow_index_var(R);

    if (reg_is_constant(R)) return {};

    if (reg_is_changeable(R) and is_modifiable(expression_at(R))) return R;

    // 2. Evaluate R in context c2, and get the first changeable reg on the path.
    auto [r, _] = incremental_evaluate_in_context(R, c1);

    // 3. Walk the call chain to find the modifiable, if any.
    auto mod_reg = find_precomputed_modifiable_reg_in_context(r, c1);

    // 5. Check that mod_reg is executed in c1, where R may not be evaluated.
    if (mod_reg)
    {
        assert(is_modifiable(expression_at(*mod_reg)));
        if (not call_for_reg(*mod_reg))
            mod_reg = {};
    }

    // 6. Return the result.
    return mod_reg;
}

// We used to perform this execution in a new context, and then
// roll it back.  However, this meant that when doing random samples
// during proposals, a random sample could be lost and re-sampled during
// evaluation of another argument in the same context.

int reg_heap::find_const_or_modifiable_reg_in_context(int R, int c1)
{
    R = follow_index_var(R);

    if (reg_is_constant(R)) return R;

    if (reg_is_changeable(R) and is_modifiable(expression_at(R))) return R;

    // 1. Evaluate R in context c2, and get the first changeable reg on the path.
    auto [r, _] = incremental_evaluate_in_context(R, c1);

    // 2. Walk the call chain to find the modifiable, if any.
    auto r2 = find_precomputed_const_or_modifiable_reg_in_context(r, c1);

    // 3. The reg should be evaluated.
    assert(r2);

    // 4. Return the result.
    return r2.value();
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

bool reg_heap::reg_has_value(int r) const
{
    // We can't assume that r is evaluated in all callers...
    if (reg_is_index_var_with_force(r))
	r = closure_at(r).reg_for_index_var();

    if (reg_is_constant(r))
        return true;
    else
        return has_result1(r);
}

bool reg_heap::reg_has_call(int r) const
{
    return has_step1(r) and call_for_reg(r);
}

int reg_heap::call_for_reg(int r) const
{
    return step_for_reg(r).call;
}

bool reg_heap::has_step1(int r) const
{
    return step_index_for_reg(r)>0;
}

bool reg_heap::has_step2(int r) const
{
    return (not prog_unshare[r].test(unshare_step_bit)) and step_index_for_reg(r)>0;
}

bool reg_heap::has_result1(int r) const
{
    return result_for_reg(r)>0;
}

bool reg_heap::has_result2(int r) const
{
    return (not prog_unshare[r].test(unshare_result_bit)) and has_result1(r);
}

bool reg_heap::force_regs_check_same_inputs(int r)
{
    assert(reg_is_changeable(r));

    // We can't use a range-for here because regs[r] can be moved
    // during the loop if we do evaluation, and the range-for saves
    // the location of regs[r] from before it was moved.

    bool zero_count = not reg_is_forced(r);
    for(int i=0;i<regs[r].forced_regs.size();i++)
    {
        int r2 = regs[r].forced_regs[i];

        incremental_evaluate2(r2, zero_count);

        assert(reg_is_constant(follow_index_var_target(r2)) or has_result2(follow_index_var_target(r2)));
        assert(reg_is_forced(r2));
    }

    // We can only have the same inputs as a previous step if we have a step from the
    // previous program that was marked invalid.
    bool same_inputs = prog_unshare[r].test(unshare_step_bit);
    for(int i=0;i<regs[r].used_regs.size();i++)
    {
        auto [r2,r3,__] = regs[r].used_regs[i];

        incremental_evaluate2(r2, zero_count);

	assert(follow_index_var_target(r2) == r3);
        assert(reg_is_forced(r3));
        assert(reg_is_constant(r3) or reg_is_changeable(r3));
        assert(reg_is_constant(r3) or has_result2(r3));

        same_inputs = same_inputs and not prog_unshare[r3].test(different_result_bit);
    }
    return same_inputs;
}

void reg_heap::force_reg_no_call(int r)
{
    assert(reg_is_changeable_or_forcing(r));
    assert(not reg_is_forced(r));

    // We can't use a range-for here because regs[r] can be moved
    // during the loop if we do evaluation.
    for(int i=0; i < regs[r].used_regs.size(); i++)
    {
        auto [r2,_,__] = regs[r].used_regs[i];

        incremental_evaluate2(r2, true);

        assert(reg_is_constant(follow_index_var_target(r2)) or has_result2(follow_index_var_target(r2)));
        assert(reg_is_forced(r2));
    }

    for(int i=0; i < regs[r].forced_regs.size(); i++)
    {
        int r2 = regs[r].forced_regs[i];

        incremental_evaluate2(r2, true);

        assert(reg_is_constant(follow_index_var_target(r2)) or has_result2(follow_index_var_target(r2)));
        assert(reg_is_forced(r2));
    }
}

void reg_heap::force_reg_with_call(int r)
{
    assert(reg_is_changeable(r));
    assert(has_step2(r));
    assert(has_result2(r));
    assert(not reg_is_forced(r));

    force_reg_no_call(r);

    int s = step_index_for_reg(r);
    int call = steps[s].call;
    assert(call > 0);

    assert(reg_is_constant(call) or reg_is_changeable_or_forcing(call));

    // If R2 is WHNF then we are done
    if (reg_is_changeable_or_forcing(call))
    {
        // If r has a result, then shouldn't its call have a result?
        // In this case, the call is to a constant-with-force.
        // Do those have results?
        assert(reg_is_constant(follow_index_var_target(call)) or has_result2(follow_index_var_target(call)));
        incremental_evaluate2(call, true);
	assert(reg_is_constant(follow_index_var_target(call)) or has_result2(follow_index_var_target(call)));
        assert(reg_is_forced(call));
    }
}

int reg_heap::value_for_reg(int r) const
{
    assert(not reg_is_unevaluated(r));

    r = follow_index_var_target(r);

    if (reg_is_constant(r))
        return r;
    else
    {
        assert(reg_is_changeable(r));
        assert(has_result1(r));
        return result_for_reg(r);
    }
}

int reg_heap::result_for_reg(int r) const 
{
    assert(prog_results[r] != 0);
    return prog_results[r];
}

bool reg_heap::regs_maybe_different_value(int r1, int r2) const
{
    assert(r2 > 0);

    if (r1 < 0) return true;

    if (r1 == r2) return false;

    auto& E1 = expression_at(r1);
    auto& E2 = expression_at(r2);

    if (E1.type() != E2.type()) return true;

    if (E1.is_int() and E1.as_int() == E2.as_int())
    {
        assert(E1 == E2);
        return false;
    }

    return true;
}

void reg_heap::set_result_for_reg(int r1)
{
    assert(reg_is_changeable(r1));

    // 1. Find called reg
    int r2 = step_for_reg(r1).call;
    assert(reg_is_constant(r2) or reg_is_changeable_or_forcing(r2));

    // 2. Set the result value for the current reg
    prog_results[r1] = value_for_reg(r2);
    assert(prog_results[r1] > 0);
}

void reg_heap::set_used_reg(int r1, int r2)
{
    assert(regs.is_used(r1));
    assert(regs.is_used(r2));

    assert(closure_at(r2));
    assert(reg_has_value(r2));

    // An index_var's value only changes if the thing the index-var points to also changes.
    // So, we may as well forbid using an index_var as an input.
    assert(not reg_is_index_var_no_force(r2));

    // We are going to put the back-edge on the first non-index-var that we see.
    int r3 = follow_index_var_target(r2);

    assert(reg_is_changeable(r3));

    auto& R1 = regs[r1];
    auto& R3 = regs[r3];
    int back_index = R3.used_by.size();
    int forw_index = R1.used_regs.size();
    R3.used_by.push_back({r1,forw_index});
    R1.used_regs.push_back({r2,r3,back_index});

    assert(reg_is_used_by(r1,r2));
}


int reg_heap::follow_single_force_index_var(int r) const
{
    assert(regs.is_used(r));

    assert(closure_at(r));

    assert(reg_is_evaluated(r));

    assert(reg_is_changeable_or_forcing(r));

    assert(reg_has_value(r));

    while(reg_is_index_var_with_force(r) and regs[r].forced_regs.size() == 1)
    {
	r = regs[r].forced_regs[0];

	assert(regs.is_used(r));

	assert(closure_at(r));

	assert(reg_is_evaluated(r));

	assert(reg_is_changeable_or_forcing(r));

	assert(reg_has_value(r));
    }

    assert(reg_is_changeable(r) or regs[r].forced_regs.size() > 1);

    return r;
}

std::optional<int> reg_heap::reg_has_single_force(int r1) const
{
    if (expression_at(r1).is_index_var() and regs[r1].forced_regs.size() == 1)
	return regs[r1].forced_regs[0];
    else
	return {};
}

int reg_heap::set_forced_reg(int r1, int r2)
{
    assert(regs.is_used(r2));

    assert(reg_is_evaluated(r2));

    assert(closure_at(r2));

    assert(reg_is_changeable_or_forcing(r2));

    assert(reg_has_value(r2));

    // An index_var's value only changes if the thing the index-var points to also changes.
    // So, we may as well forbid using an index_var as an input.
    assert(not reg_is_index_var_no_force(r2));

    if (auto r3 = reg_has_single_force(r2))
    {
	assert(not reg_has_single_force(*r3));

	r2 = *r3;

	assert(regs.is_used(r2));

	assert(reg_is_evaluated(r2));

	assert(closure_at(r2));

	assert(reg_is_changeable_or_forcing(r2));

	assert(reg_has_value(r2));

	assert(not reg_is_index_var_no_force(r2));
    }

    regs[r1].forced_regs.push_back(r2);

    assert(reg_is_forced_by(r1,r2));

    return r2;
}

void reg_heap::set_call(int s1, int r2, bool unsafe)
{
    // Check that step s is legal
    assert(steps.is_used(s1));

    // Check that R2 is legal
    assert(regs.is_used(r2));

    auto& S1 = steps[s1];

    // Don't override an *existing* call
    assert(S1.call == 0);
    assert(not S1.call_edge);

    // Set the call
    S1.call = r2;

    // We may need a call edge to index-var-with-force nodes in order to achieve the proper force count.
    // But back-edges can only be on changeables.
    // In order to achieve that, we only set call back-edges when the target is evaluated.
    if (not unsafe)
    {
	assert(not reg_is_unevaluated(r2));

	// put the back-edge on the first non-index-var that we see
	int r3 = r2;
	r2 = follow_index_var(r2);

	if (reg_is_changeable(r2))
	{
	    // 6. Add a call edge from to R2.
	    auto& R2 = regs[r2];
	    int back_index = R2.called_by.size();
	    R2.called_by.push_back(s1);

	    // Maybe this should just be optional<int> back_index?
	    S1.call_edge = {r3, r2, back_index};
	}
	else
	{
	    assert(reg_is_constant(r2));
	    S1.call_edge = {r3, -1, -1};
	}
    }
    else
	assert(not S1.call_edge);
}

void reg_heap::clear_call(int s)
{
    auto& S = steps[s];
    int call = S.call;
    assert(call > 0);

    // 1. Remove the edge from step[s] <--- regs[call]
    if (S.call_edge)
    {
        auto [_, r2, j] = S.call_edge.value();

	if (r2 > 0 and not regs.is_free(r2))
	{
	    assert(follow_index_var(call) == r2);
	    auto& backward = regs[r2].called_by;
	    assert(0 <= j and j < backward.size());

	    // Move the last element to the hole, and adjust index of correspond forward edge.
	    if (j+1 < backward.size())
	    {
		backward[j] = backward.back();
		auto& [_, rr2, jj] = steps[backward[j]].call_edge.value();
		jj = j;
	    }
	    backward.pop_back();
	}
    }

    // 2. Clear the forward edge from steps[s] -> regs[call]
    S.call = 0;
    S.call_edge = {};
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
    assert(not regs.access(r).created_by_step);
    regs.access(r).created_by_step = {s,index};
}

void reg_heap::mark_step_with_effect(int s)
{
    steps[s].mark_with_effect();
}

int reg_heap::allocate()
{
    total_reg_allocations++;
    int r = regs.allocate();
    mark_reg_unevaluated(r);
    assert(regs[r].used_regs.empty());
    return r;
}

int reg_heap::allocate_reg_from_step(int s)
{
    int r = allocate();
    mark_reg_created_by_step(r,s);
    assert(not has_step1(r));
    return r;
}

int reg_heap::allocate_reg_from_step(int s, closure&& C)
{
    int r = allocate_reg_from_step(s);
    set_C(r, std::move(C));
    return r;
}

void reg_heap::interchange_regs(int r1, int r2, int t)
{
    assert(r1 != r2);
    assert(reg_is_changeable(r1));
    assert(reg_is_changeable(r2));
    assert(not reg_is_unevaluated(r1));
    assert(not reg_is_unevaluated(r2));

    assert(not children_of_token(t).size());
    if (not is_root_token(t))
        assert(directed_token_type(t) == token_type::set);

    // Check that this r1 indeed interchangeable
    if (not is_interchangeable(expression_at(r1)))
        throw myexception()<<"interchange_regs: reg1 ["<<r1<<"] = "<<closure_at(r1).print()<<" is not interchangeable!";

    // Check that this r2 is indeed interchangeable
    if (not is_interchangeable(expression_at(r2)))
        throw myexception()<<"interchange_regs: reg2 ["<<r2<<"] = "<<closure_at(r2).print()<<" is not interchangeable!";

    // Check that we are only interchanging steps for the same computation.
    if (closure_at(r1) != closure_at(r2))
        throw myexception()<<"interchange_regs: reg1 ["<<r1<<"] = "<<closure_at(r1).print()<<
                                        "and reg2 ["<<r2<<"] = "<<closure_at(r2).print()<<" are not the same!";

    assert(not is_root_token(t));

    assert(is_root_token(parent_token(t)));

    tokens[t].interchanges.push_back({r1,r2});

    assert(not children_of_token(t).size());
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
int reg_heap::set_reg_value(int R, closure&& value, int t, bool unsafe)
{
    total_set_reg_value++;
    assert(not children_of_token(t).size());
    assert(reg_is_changeable(R));
    assert(value);

    if (not is_root_token(t))
        assert(unsafe or directed_token_type(t) == token_type::set);

    // 1. Check that this reg is indeed settable
    if (not is_modifiable(expression_at(R)))
        throw myexception()<<"set_reg_value: reg "<<R<<" is not modifiable!";

    assert(not is_root_token(t));

    // 2. Find or create a step for reg R.
    optional<int> found_step_for_mod;
    for(auto& [r2,s2]: tokens[t].vm_step.delta())
    {
        if (r2 == R)
        {
            found_step_for_mod = s2;
            break;
        }
    }

    int s = -1;
    if (found_step_for_mod)
	s = *found_step_for_mod;
    else
    {
        // Allocate a new step if we haven't allocated a step for this reg yet.
        s = get_shared_step(R);

        tokens[t].vm_step.add_value(R, s);
        tokens[t].vm_result.add_value(R, non_computed_index);
        tokens[t].n_modifiables_set++;
    }

    // 3. If the step already has a call, then reclaim the target if necessary.
    if (steps[s].call != 0)
    {
	int r_call = steps[s].call;

	// We are over-writing a previously-set value.
	clear_call(s);

	if (creator_step_for_reg(r_call) == s)
	{
	    clear_back_edges_for_reg(r_call);
	    assert(not reg_is_unforgettable(r_call));
	    reclaim_used(r_call);
	}
    }

    // 4. If the value is a pre-existing reg_var, then call it.
    if (value.exp.is_index_var())
    {
        int Q = value.reg_for_index_var();

        // Never set the call to an index var.
        Q = follow_index_var_no_force(Q);

        // Never call something unevaluated either.
        assert(not reg_is_unevaluated(Q));

        // Set the call
        set_call(s, Q, unsafe);
    }
    // 5. Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the value and call it.
    else
    {
	if (not unsafe)
	{
	    assert(value.exp.size() == 0);
	    assert(is_WHNF(value.exp));
	}

        int R2 = allocate_reg_from_step(s, std::move(value) );

	if (not unsafe)
	{
	    // How important is this?
	    // What if we want to call something that IS unevaluated?
	    mark_reg_constant(R2);
	}

        set_call(s, R2, unsafe);
    }

#if DEBUG_MACHINE >= 2
    check_used_regs();
    check_tokens();
#endif

    return steps[s].call;
}

std::vector<int> reg_heap::used_regs_for_reg(int r) const
{
    vector<int> U;

    for(const auto& [r2,_,__]: regs[r].used_regs)
        U.push_back(r2);

    return U;
}

std::vector<int> reg_heap::forced_regs_for_reg(int r) const
{
    vector<int> U;

    for(int r2: regs[r].forced_regs)
        U.push_back(r2);

    return U;
}

void reg_heap::reclaim_used(int r)
{
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
    assert(not has_step1(r));
    assert(not has_result1(r));

    // Clear any force counts.
    // This reg was in a tip token that could have forced it.  But no other programs will force it.
    prog_force_counts[r] = 0;
  
    regs.reclaim_used(r);
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
    perform_io_head = add_compute_expression( Core::unsafePerformIO() );
    return *perform_io_head;
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

int reg_heap::reg_for_head(int index) const
{
    return heads[index];
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
    // This is a called from pool<reg>::expand_memory( )
    assert(regs.size() == s);

    auto old_size = prog_steps.size();
    if (log_verbose >= 3)
        std::cerr<<"Expanding VM heap from "<<old_size<<" -> "<<s<<".\n";

    // Extend program.  Use regs.size() instead of size()
    prog_steps.resize(regs.size());
    prog_results.resize(regs.size());
    prog_force_counts.resize(regs.size());
    prog_temp.resize(regs.size());
    prog_unshare.resize(regs.size());

    // Now we can use size() again.
    for(auto i=old_size;i<size();i++)
    {
        prog_steps[i] = non_computed_index;
        prog_results[i] = non_computed_index;

        assert(prog_steps[i] == non_computed_index);
        assert(prog_results[i] == non_computed_index);
        assert(prog_force_counts[i] == 0);
        assert(prog_temp[i].none());
        assert(prog_unshare[i].none());
    }
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
    for(auto& [r,_,__]: regs[r1].used_regs)
        if (r == r2)
            return true;

    return false;
}

bool reg_heap::reg_is_forced_by(int r1, int r2) const
{
    for(int r: regs[r1].forced_regs)
        if (r == r2)
            return true;

    return false;
}

void reg_heap::check_used_regs_in_token(int t) const
{
    assert(token_is_used(t));

    if (directed_token_type(t) == token_type::reverse_execute)
    {
        for(auto [r,result]: tokens[t].delta_result())
            assert(result < 0);

        for(auto [r,step]: tokens[t].delta_step())
            assert(step < 0);
    }
    else if (directed_token_type(t) == token_type::execute)
    {
        for(auto [r,result]: tokens[t].delta_result())
            assert(result > 0);

        for(auto [r,step]: tokens[t].delta_step())
            assert(step > 0);
    }

    constexpr int force_count_bit = 3;
    constexpr int result_bit = 0;
    constexpr int step_bit = 1;

    for(auto [r,count]: tokens[t].delta_force_count())
    {
        // Regs can have inaccurate force counts if they are not program-execution tokens.
        // So its possible to have positive force counts for destroyed regs, in tokens where
        // the previous program-execution token is destroyed.

        // assert(not regs.is_free(r) or count == 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(force_count_bit));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(force_count_bit);

        // No results for constant regs
        assert(count >= 0);
    }

    std::unordered_map<int,int> reg_to_result;
    for(auto [r,result]: tokens[t].delta_result())
    {
        // Check that there are no duplicate regs.
        assert(not reg_to_result.count(r));
        reg_to_result[r] = result;

        // Deltas should not contain free regs except resets.
        assert(not regs.is_free(r) or result < 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(result_bit));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(result_bit);

        // Only changeable or forcing regs can have results.
        if (result > 0)
            assert(reg_is_changeable_or_forcing(r));

    }

    bool root_child = not is_root_token(t) and is_root_token(parent_token(t)) and tokens[t].flags.test(0);
    std::unordered_map<int,int> reg_to_step;
    for(auto [r,step]: tokens[t].delta_step())
    {
        // Check that there are no duplicate regs.
        assert(not reg_to_step.count(r));
        reg_to_step[r] = step;

        // Deltas should not contain free regs except resets.
        assert(not regs.is_free(r) or step < 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(step_bit));

        // Mark the reg as having a step in the delta.
        prog_temp[r].set(step_bit);

        // If the step is unshared, the result must be unshared as well: this allows us to just walk unshared results.
        if (root_child)
            assert(prog_temp[r].test(result_bit) or prog_unshare[r].test(unshare_result_bit));
        else
            assert(prog_temp[r].test(result_bit));

        // === Only regs with actual steps after here === //
        if (step < 0) continue;

        // Only  changeable regs can have steps.
        assert(reg_is_changeable(r));

        if (not root_child)
        {
            // If this step calls a constant reg, then that should be the result.
            int call = steps[step].call;
            // There can be a step with call == 0 when we do set_reg_value -> allocate -> here.
            // The call is only 0 temporarily.
            if (call > 0 and reg_is_constant(call))
            {
                int result = reg_to_result.at(r);
                assert(result < 0 or result == call);
            }

            // Since this step is in a non-root token, any steps of its child regs should not be in more root-ward tokens.
            // So they certainly should not be in the root token.
            for(int r2: steps[step].created_regs)
            {
                assert(not has_step1(r2));
                assert(not has_result1(r2));
            }
        }
    }

    // FIXME - nonlocal. The same result/step are not set in multiple places!
    // TODO: Use a map from step -> (reg,token)
    for(auto [reg,res]: tokens[t].delta_force_count())
        prog_temp[reg].reset(force_count_bit);

    for(auto [reg,res]: tokens[t].delta_result())
    {
        prog_temp[reg].reset(result_bit);
        prog_temp[reg].reset(step_bit);
    }

    for(auto [reg,step]: tokens[t].delta_step())
    {
        prog_temp[reg].reset(result_bit);
        prog_temp[reg].reset(step_bit);
    }
}

void reg_heap::check_used_regs1() const
{
    bool ok = true;
    for(auto i = regs.begin(); i != regs.end(); i++)
    {
        int r1 = i.addr();

	for(int r2: regs[r1].C.Env)
	{
	    if (regs.is_free(r2))
	    {
		std::cout<<"Reg "<<r1<<" with closure "<<closure_at(r1).print()<<" refers to free reg "<<r2<<"\n";
		ok = false;
	    }
	}
    }
    if (not ok)
	std::abort();
}

void reg_heap::check_used_regs() const
{
    if (root_token >= 0)
    {
        assert(tokens[root_token].vm_step.empty());
        assert(tokens[root_token].vm_result.empty());
        assert(tokens[root_token].vm_force_count.empty());
    }

    for(int t=0; t< tokens.size(); t++)
        if (token_is_used(t))
            check_used_regs_in_token(t);

    for(auto& S:steps)
    {
        if (S.call > 0)
            assert(not regs.is_free(S.call));
    }

    bool in_pe_token = (root_token>=0)?is_program_execution_token(root_token):false;
    bool do_check_force_counts = in_pe_token and stack.empty();

    if (do_check_force_counts)
	check_force_counts();

    for(auto i = regs.begin(); i != regs.end(); i++)
    {
        int r1 = i.addr();

	if (not reg_is_contingent(r1))
	{
	    auto& R = regs[r1];

	    for(int r2: R.C.Env)
		assert(not reg_is_contingent(r2));

	    for(auto& [r2,_,__]: R.used_regs)
		assert(not reg_is_contingent(r2));

	    for(int r2: R.forced_regs)
		assert(not reg_is_contingent(r2));
	}

        if (do_check_force_counts)
        {
            if (has_step1(r1))
                assert(reg_is_unforgettable(r1) or reg_is_forced(r1));

            if (has_step1(r1) and reg_is_forced(r1))
                assert(prog_force_counts[r1] > 0);

            if (has_result1(r1))
                assert(prog_force_counts[r1] > 0);
        }

        if (prog_force_counts[r1] > 0)
            assert(reg_is_changeable_or_forcing(r1));

        if (not regs[r1].used_regs.empty())
            assert(reg_is_changeable(r1) or reg_is_unevaluated(r1));

        if (not regs[r1].forced_regs.empty())
            assert(reg_is_changeable_or_forcing(r1) or reg_is_unevaluated(r1));

        // Under what conditions should a constant_with_force have finished forcing its regs?
        // If we add a result to it, then we can use that to mark it as being done.

        if (in_pe_token and has_result1(r1))
        {
            for(auto r2: used_regs_for_reg(r1))
                assert(reg_has_value(r2));
            for(auto r2: forced_regs_for_reg(r1))
                assert(reg_has_value(r2));
            if (has_step1(r1))
            {
                int call = step_for_reg(r1).call;
                assert(reg_has_value(call));
            }
        }

        for(const auto& [r2,_,__]: regs[r1].used_regs)
        {
            // Used regs should have back-references to R
            assert( reg_is_used_by(r1, r2) );

            // Used computations should be mapped computation for the current token, if we are at the root
            assert(reg_is_to_changeable(r2));

            // The used result should be referenced somewhere more root-ward
            // so that this result can be invalidated, and the used result won't be GC-ed.
            // FIXME - nonlocal.  assert(is_modifiable(expression_at(R2)) or result_is_referenced(t,res2));
        }
        for(int r2: regs[r1].forced_regs)
        {
            // Used regs should have back-references to R
            assert( reg_is_forced_by(r1, r2) );

            // Used computations should be mapped computation for the current token, if we are at the root
            assert(reg_is_changeable_or_forcing(r2));

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
    assert(not has_step1(r));

    // Allocate a step
    int s = get_shared_step(r);

    // Link it in to the mapping
    prog_steps[r] = s;

    assert(s > 0);

    return s;
}

void reg_heap::check_back_edges_cleared_for_step(int s) const
{
    assert(not steps[s].call_edge);

    for(auto& r: steps.access_unused(s).created_regs)
        assert(not regs.access(r).created_by_step);
}

void reg_heap::clear_back_edges_for_reg(int r, bool creator_survives)
{
    // 1. When destroying a reg, remove edge from regs[r] <---used_by--- regs[r3]
    assert(r > 0);
    for(auto& forward: regs[r].used_regs)
    {
        auto [_,r3,j] = forward;

	if (regs.is_free(r3)) continue;

	assert(reg_is_changeable(r3));

	auto& backward = regs[r3].used_by;
        assert(0 <= j and j < backward.size());

	// erase regs[r3].used_by[j]
        if (j+1 < backward.size())
        {
            // erase the backward edge by moving another backward edge on top of it.
            backward[j] = backward.back();
            auto [r2,i2] = backward[j];
            // adjust the forward edge for that backward edge
            auto& forward2 = regs[r2].used_regs;
            assert(0 <= i2 and i2 < forward2.size());
	    std::get<2>(forward2[i2]) = j;

	    // We can assume that r2 isn't deleted, because if it was this entry would be gone.
            assert(std::get<2>(regs[r2].used_regs[i2]) == j);
            assert(regs[std::get<1>(forward2[i2])].used_by[std::get<2>(forward2[i2])].second == i2);
        }

        backward.pop_back();

        forward = {0,0,0};
    }

    // 3. When destroying a reg, remove edge from step[s] ---created_regs---> regs[r]
    if (creator_survives)
    {
        assert(r > 0);
        auto& created_by_step = regs.access(r).created_by_step;
        if (created_by_step)
        {
	    auto [s,j] = *created_by_step;
            auto& backward = steps[s].created_regs;
            assert(0 <= j and j < backward.size());

            // Clear the forward edge.
            created_by_step.reset();

            // Move the last element to the hole, and adjust index of correspond forward edge.
            if (j + 1 < backward.size())
            {
                backward[j] = backward.back();
                auto& forward2 = regs.access(backward[j]);
                forward2.created_by_step->second = j;

                assert(regs.access(backward[j]).created_by_step->second == j);
            }
            backward.pop_back();
        }
    }
}

void reg_heap::check_back_edges_cleared_for_reg(int r) const
{
    for(auto& [_,__,index]: regs.access_unused(r).used_regs)
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
    assert(not has_result1(r));
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
    if (reg_is_constant(R)) return expression_at(R);

    total_get_reg_value_non_const++;
    reroot_at_context(c);

    if (reg_has_value(R))
    {
        total_get_reg_value_non_const_with_result++;
        int R2 = value_for_reg(R);
        if (R2) return expression_at(R2);
    }

    // If the value needs to be computed (e.g. its a call expression) then compute it.
    auto [R2, value] = incremental_evaluate_in_context(R,c);
    R = R2;

    return expression_at(value);
}

int reg_heap::set_reg_value_in_context(int P, closure&& C, int c)
{
    int t = token_for_context(c);
    if (directed_token_type(t) != token_type::set or not tokens[t].children.empty() or tokens[t].context_refs.size() > 1)
	t = switch_to_child_token(c, token_type::set);
    else
	assert(tokens[t].context_refs.size() == 1);

    return set_reg_value(P, std::move(C), t);
}

void reg_heap::interchange_regs_in_context_(int r1, int r2, int c)
{
    int t = switch_to_child_token(c, token_type::set);

    interchange_regs(r1, r2, t);
}

void reg_heap::interchange_regs_in_context(int r1, int r2, int c)
{
    force_simple_set_path_to_PPET(c);

    interchange_regs_in_context_(r1, r2, c);
}

// Is execution allowed IN THE ROOT TOKEN?
bool reg_heap::execution_allowed_at_root() const
{
    if (root_token < 0) return false;

    if (tokens[root_token].children.size() == 0) return true;

    if (tokens[root_token].children.size() == 1)
    {
        int t1 = tokens[root_token].children[0];
        return (directed_token_type(t1) == reverse(token_type::execute) or directed_token_type(t1) == reverse(token_type::execute2));
    }

    return false;
}

const closure& reg_heap::value_for_precomputed_reg(int r) const
{
    r = follow_index_var(r);
    return access_value_for_reg(r);
}

optional<int> reg_heap::precomputed_value_in_context(int r, int c)
{
    // QUESTION: Should I replace this with incremental_evaluate_unchangeable?
    r = follow_index_var(r);

    if (reg_is_constant(r))
	return r;
    else
    {
	assert(reg_is_changeable(r));

	reroot_at_context(c);

	// In theory, variants of this routine could allow
	// * having a result, but no force.
	// * having a chain of steps, but no result.
	if (has_result1(r))
	    return result_for_reg(r);
	else
	{
	    std::abort();
	    return {};
	}
    }
}

pair<int,int> reg_heap::incremental_evaluate_in_context(int R, int c)
{
#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    R = follow_index_var(R);

    if (reg_is_constant(R)) return {R,R};

    int t = token_for_context(c);
    // Write-Read coalescening.
    // If the context is a set-token that is a child of the root, then look up the value there...
    if (reg_is_changeable(R) and
	is_modifiable(expression_at(R)) and
        undirected_token_type(t) == token_type::set)
    {
	// 0. Reroot to our older neighbor.
	int parent = older_neighbor(t).value();
	reroot_at_token(parent);
	assert(tokens[t].parent == root_token);
	assert(directed_token_type(t) == token_type::set);

	// 1. Search the Delta for changes to the value.
	int r_constant = 0;
	for(auto& [r,s]: tokens[t].vm_step.delta())
	{
	    auto& E = expression_at(r);
	    if (is_interchangeable(E))
	    {
		r_constant = -1; // Do NOT look in root program.
		break;
	    }
	    else
	    {
		assert(is_modifiable(E));
		if (r == R)
		{
		    int call = steps[s].call;
		    if (is_WHNF(expression_at(call)))
			r_constant = call;
		    else
			r_constant = -1; // Do NOT look in root program.
		                         // (We don't currently allow modifiables to call non-WHNF expressions though).
		}
	    }
	}

	// 2a. If we found the modifiable set to a constant, return the constant.
        if (r_constant > 0)
            return {R, r_constant};
        // 2b. Otherwise this is a non-interchange token AND we didn't find R.
        //     Therefore we are allowed to look in the root token.
        else if (r_constant == 0)
        {
            // If we have a result, use that.
            int r2 = result_for_reg(R);
            if (r2 > 0)
                return {R,r2};

            // If we have a call to a WHNF reg, use that.
            int s2 = step_index_for_reg(R);
            if (s2 > 0)
            {
                int call = steps[s2].call;
                if (is_WHNF(expression_at(call)))
                    return {R,call};
            }
        }
    }

    reroot_at_context(c);

    // Move the context toward the single executable child token as long as there is one.
    while (true)
    {
	std::optional<int> executable_child;
	int t = token_for_context(c);
	for(int t2: children_of_token(t))
	    if (directed_token_type(t2) == token_type::execute)
	    {
		assert(token_younger_than(t2,t));
		executable_child = t2;
	    }

	if (executable_child)
	{
	    set_token_for_context(c, *executable_child);
	    reroot_at_context(c);
	}
	else
	    break;
    }

    // Don't create a new token to find results that are already up-to-date!
    if (reg_is_changeable_or_forcing(R))
    {
        // If we have a result, use that.
        int r2 = result_for_reg(R);
        if (r2 > 0)
            return {R,r2};

        // If we have a call to a WHNF reg, use that.
        int s2 = step_index_for_reg(R);
        if (s2 > 0)
        {
            int call = steps[s2].call;
            if (is_WHNF(expression_at(call)))
                return {R, call};
        }
    }

    if (not execution_allowed_at_root() or is_program_execution_token(token_for_context(c)))
    {
        switch_to_child_token(c, token_type::execute);

        // This should not allow removing the old root token.
        reroot_at_context(c);

        // We can't remove t1 even if its a knuckle.
        assert(execution_allowed_at_root());
    }

    assert(execution_allowed_at_root());

    auto p = incremental_evaluate1(R);

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    return p;
}

const closure& reg_heap::lazy_evaluate1(int& R)
{
    auto [R2, value] = incremental_evaluate1(R);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate2(int& R)
{
    auto [R2, value] = incremental_evaluate2(R,false);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate(int& R, int c)
{
    auto [R2, value] = incremental_evaluate_in_context(R, c);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate_head(int index, int c)
{
    int R1 = heads[index];
    auto [R2, value] = incremental_evaluate_in_context(R1, c);
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
    {
        if (special_prelude_symbols.count(name))
            return identifiers.at(name);
        else
            throw myexception()<<"Cannot add identifier '"<<name<<"': there is already an identifier with that name.";
    }

    int R = allocate();

    identifiers[name] = R;
    return R;
}

// FIXME: We SHOULD be able to do each module in sequence, since there are no
//        transitive dependencies.
//
//        Currently that doesn't work, because things implicitly depend on
//        Foreign.String.unpack_cpp_string to get strings.
//
void reg_heap::allocate_identifiers_for_program()
{
    // 0. Free symbol table memory
    for(auto& M: *program)
        M->clear_symbol_table();

    // 1. Pre-allocate locations for symbols that can be used without being imported.
    for(auto name: special_prelude_symbols)
        add_identifier(name);

    // 2. Give each identifier a pointer to an unused location; define parameter bodies.
    for(auto& M: *program)
    {
        // 2.1 Pre-allocate locations for all symbols in the module.
        for(const auto& [x, _]: M->code_defs())
            add_identifier(x.name);

        for(const auto& [x, body]: M->code_defs())
        {
            // get the root for each identifier
            auto loc = identifiers.find(x.name);
            assert(loc != identifiers.end());
            int R = loc->second;

#ifdef DEBUG_OPTIMIZE
            std::cerr<<"     "<<x<<" := "<<body<<"\n\n";
            std::cerr<<"     "<<x<<" := "<<preprocess(body).exp<<"\n\n\n\n";
#endif

            // load the body into the machine
            assert(R != -1);
            set_C(R, preprocess(body) );
        }

        M->clear_code();
    }
}

reg_heap::reg_heap(const Program& P)
    :reg_heap(std::make_unique<Program>(P))
{
}

reg_heap::reg_heap(std::unique_ptr<Program> P)
    :regs(1,[this](int s){resize(s);}, [this](){collect_garbage();} ),
     steps(1),
     program(std::move(P)),
     fresh_var_state(*program->fresh_var_state()),
     args(program->get_module_loader()->args),
     prog_steps(1,non_existant_index),
     prog_results(1, non_existant_index),
     prog_force_counts(1, 0),
     prog_temp(1),
     prog_unshare(1)
{
    allocate_identifiers_for_program();

    if (program->get_main_name())
    {
        expression_ref M = var( *program->get_main_name() );
        main_head = add_compute_expression( Core::unsafePerformIO(M) );
    }

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

void reg_heap::release_pair_scratch_list() const
{
    n_active_pair_scratch_lists--;
}

vector<pair<int,int>>& reg_heap::get_pair_scratch_list() const
{
    while(n_active_pair_scratch_lists >= pair_scratch_lists.size())
        pair_scratch_lists.push_back( new Vector<pair<int,int>> );

    vector<pair<int,int>>& v = *pair_scratch_lists[ n_active_pair_scratch_lists++ ];

    v.clear();

    return v;
}

void reg_heap::run_main()
{
    int r = heads[main_head.value()];
    incremental_evaluate_unchangeable(r);
}
