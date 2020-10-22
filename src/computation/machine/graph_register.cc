#include <iostream>
#include <range/v3/all.hpp>
#include <algorithm>
#include "util/truncate.H"
#include "util/log-level.H"
#include "util/string/join.H"
#include "util/io/vector.H"
#include "graph_register.H"
#include "computation/expression/var.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/tuple.H"
#include "computation/expression/modifiable.H"
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
    truncate(forced_by);
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
    assert(forced_by.empty());
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

    forced_by = std::move( R.forced_by );

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
     forced_by ( std::move( R.forced_by) ),
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
    assert(regs.size() == prog_forces.size());
    assert(regs.size() == prog_temp.size());
    assert(regs.size() == prog_unshare.size());
    return regs.size();
}

void reg_heap::register_likelihood_(const effect& e, int s)
{
    auto & E = dynamic_cast<const register_likelihood&>(e);
    assert(not likelihood_heads.count(s));
    likelihood_heads[s] = E.likelihood_reg;
    for(auto& handler: register_likelihood_handlers)
        handler(e, s);
}

void reg_heap::unregister_likelihood_(const effect& e, int s)
{
    assert(likelihood_heads.count(s));
    likelihood_heads.erase(s);
    // FIXME: run these in reverse order?
    for(auto& handler: unregister_likelihood_handlers)
        handler(e, s);
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

    int call = steps[s].call;
    auto& e = expression_at(call);
    assert(e.is_a<effect>());
    e.as_<effect>().register_effect(*this, s);
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

    int call = steps[s].call;
    auto& e = expression_at(call);
    assert(e.is_a<effect>());
    e.as_<effect>().unregister_effect(*this, s);
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

int reg_heap::force_count(int r) const
{
    if (not reg_is_changeable(r)) return 0;

    // Count a reference from the program_result_head.
    int count = 0;
    if (program_result_head and r == heads[*program_result_head])
        count = 1;

    // Look at steps that USE the root's result
    for(auto& [r2,_]: regs[r].used_by)
        if (prog_steps[r2] > 0)
            count++;

    // Look at steps that FORCE the root's result
    for(auto& [r2,_]: regs[r].forced_by)
        if (prog_steps[r2] > 0)
            count++;

    // Look at steps that CALL the root's result
    for(auto& s2: regs[r].called_by)
        if (int r2 = steps[s2].source_reg; prog_steps[r2] == s2)
            count++;

    return count;
}

/*
 * FIXME: The simplest method of unmapping would be to leave the root at t1 and do
 *              tokens[t2].vm_step.add_value(r, non_computed_index);
 *        This has the benefit of handling effects automatically we reroot
 *        to t2.
 *
 *        However, the force_count( ) function only computes force_counts *in the root*.
 *        So we need to root to be at t2 to use that.
 *
 *        If we store a prog_force_count and decrement force counts for t2 when we move steps
 *        back to t2, it also seems like the root should be at t2 to modify prog_force_count.
 *
 */

int reg_heap::unmap_unforced_steps(int c)
{
    // 0. Record original token for context c.
    int t1 = token_for_context(c);

    // 1. Leave new marker context c2 at t1 to avoid t1 being deleted.
    int c2 = copy_context(c);

    // 2. Switch context c to new child token of t1
    switch_to_child_token(c, token_type::unmap);

    // 3. Reroot at the new child context.
    reroot_at_context(c);

    // 4. Define function to check and unmap
    assert(token_is_used(t1));
    auto& vm_force_count = tokens[t1].vm_force_count;
    auto& vm_force = tokens[t1].vm_force;
    auto& vm_result = tokens[t1].vm_result;
    auto& vm_step = tokens[t1].vm_step;

    auto update_force_count = [&](int r, int count)
                                  {
                                      assert(reg_is_changeable(r));
                                      assert(count >= 0);
                                      assert(prog_force_counts[r] != count);
                                      if (not prog_temp[r].test(0))
                                      {
                                          prog_temp[r].set(0);
                                          vm_force_count.add_value(r, prog_force_counts[r]);
                                      }
                                      prog_force_counts[r] = count;
                                  };

    assert(token_is_used(t1));
    auto check_unmap = [&,this](int r) {
                           if (has_step(r) and prog_force_counts[r] == 0)
                           {
                               vm_step.add_value(r, prog_steps[r]);
                               vm_result.add_value(r, prog_results[r]);
                               vm_force.add_value(r, prog_forces[r]);

                               prog_steps[r] = non_computed_index;
                               prog_results[r] = non_computed_index;
                               prog_forces[r] = non_computed_index;
                           }
                       };

    auto inc_force_count = [&](int r)
                               {
                                   update_force_count(r, prog_force_counts[r]+1);
                               };

    auto dec_force_count = [&,this](int r)
                               {
                                   update_force_count(r, prog_force_counts[r]-1);
                                   check_unmap(r);
                               };

    // 5. Find all the unforced steps.

    // 5a. Walk all the delta_steps from the PPET to the root, recording the step that would
    //     be in the PPET if we rerooted to it.
    int ppe_token = get_prev_prog_token_for_context(c).value();

    // This token's child has type reverse_execute, making it an execute_token, in a sense.  Its type is reverse_unmap.
    assert(tokens[root_token].children.size() == 1);
    int root_child_token = tokens[root_token].children[0];

    // This is the only grandchild of the root.  It is the place we were when we decided to evaluate the program.
    assert(tokens[root_child_token].children.size() == 1);
    int root_grandchild_token = tokens[root_child_token].children[0];

    vector<pair<int,int>> modified_steps;
    optional<int> n_steps_between_progs;
    for(int path_token = ppe_token; path_token != root_grandchild_token; path_token = tokens[path_token].parent)
    {
        for(auto& [r,s]: tokens[path_token].vm_step.delta())
        {
            // If this is the first time we've seen this reg
            if (not prog_temp[r].test(1))
            {
                prog_temp[r].set(1);
                modified_steps.push_back({r,s});
            }
        }
    }
    n_steps_between_progs = modified_steps.size();
    for(auto& [r,s]: tokens[root_grandchild_token].vm_step.delta())
    {
        // If this is the first time we've seen this reg
        if (not prog_temp[r].test(1))
        {
            prog_temp[r].set(1);
            modified_steps.push_back({r,s});
        }
    }
    assert(n_steps_between_progs);

    // 5b. Increment force counts for new steps
    for(const auto& [r,s2]: modified_steps)
    {
        int s1 = prog_steps[r];

        if (s1 > 0)
        {
            if (s1 == s2) continue;

            int call1 = steps[s1].call;
            int call2 = -1;
            if (s2 < 0)
            {
                for(auto& [r2,_]: regs[r].used_regs)
                    inc_force_count(r2);

                for(auto& [r2,_]: regs[r].forced_regs)
                    inc_force_count(r2);
            }
            else
                call2 = steps[s2].call;

            if (not reg_is_constant(call1) and call1 != call2)
                inc_force_count(call1);
        }
    }

    // We currently record regs to unmap on this list instead of just unmapping them directly.
    // This is because:
    // (i)  we are using prog_steps to guide how we handle modified_steps below, and unmapping
    //      things modifies prog_steps.
    // (ii) we want to check the force counts after we decrement counts from old steps, but
    //      before we modify the counts by unmapping things.
    auto& regs_to_unmap = get_scratch_list();

    // 5c. First find regs that start with zero force count.
    //     We need to do this before decrementing to avoid putting regs on the list twice.
    for(int i=0; i < *n_steps_between_progs; i++)
    {
        auto [r,s] = modified_steps[i];
        if (has_step(r) and prog_force_counts[r] == 0)
            regs_to_unmap.push_back(r);
    }

    // 5d. Second, decrement force counts for old steps, and find unforced regs
    //     that started with positive force count.
    auto dec_force_count_ = [&,this](int r)
                               {
                                   update_force_count(r, prog_force_counts[r]-1);
                                   if (has_step(r) and prog_force_counts[r] == 0)
                                       regs_to_unmap.push_back(r);
                               };

    for(const auto& [r,s2]: modified_steps)
    {
        int s1 = prog_steps[r];

        if (s2 > 0)
        {
            if (s1 == s2) continue;

            int call1 = -1;
            int call2 = steps[s2].call;
            if (s1 < 0)
            {
                for(auto& [r2,_]: regs[r].used_regs)
                    dec_force_count_(r2);

                for(auto& [r2,_]: regs[r].forced_regs)
                    dec_force_count_(r2);
            }
            else
                call1 = steps[s1].call;

            if (not reg_is_constant(call2) and call1 != call2)
                dec_force_count_(call2);
        }
    }

#ifdef DEBUG_MACHINE
    for(int r=1;r<regs.size();r++)
        if (not regs.is_free(r))
            assert(prog_force_counts[r] == force_count(r));
#endif

    for(int r: regs_to_unmap)
    {
        // Don't unmap regs twice!
        assert(has_step(r));

        vm_step.add_value(r, prog_steps[r]);
        vm_result.add_value(r, prog_results[r]);
        vm_force.add_value(r, prog_forces[r]);

        prog_steps[r] = non_computed_index;
        prog_results[r] = non_computed_index;
        prog_forces[r] = non_computed_index;
    }

    release_scratch_list();

    // 6. Begin iteratively unmapping steps.
    const auto& delta_step   = vm_step.delta();
    for(int i=0;i<delta_step.size();i++)
    {
        // Don't use a reference, since delta_step can be moved, leaving an invalid reference.
        auto [r,s] = delta_step[i];

        // 6a. Unregister any effects marked on the unmapped steps!
        if (steps[s].has_effect())
        {
            if (steps[s].has_pending_effect_registration())
                unmark_effect_to_register_at_step(s);
            else
                unregister_effect_at_step(s);
        }

        // 6b. Check if any of used used, forced, or called steps are now unforced.
        for(auto& [r2,_]: regs[r].used_regs)
            dec_force_count(r2);

        for(auto& [r2,_]: regs[r].forced_regs)
            dec_force_count(r2);

        int call = steps[s].call;
        if (not reg_is_constant(call))
            dec_force_count(call);
    }

    // 7a. Clear mark that force count has changed between the root token and the PPET.
    for(auto [r,_]: vm_force_count.delta())
        prog_temp[r].reset(0);
    // 7b. Clear mark that the step has changed between the root token and the PPET.
    for(auto [r,_]: modified_steps)
        prog_temp[r].reset(1);

#ifdef DEBUG_MACHINE
    for(int r=1;r<regs.size();r++)
    {
        if (not regs.is_free(r))
            assert(prog_force_counts[r] == force_count(r));
        if (has_step(r))
            assert(prog_force_counts[r] > 0);
    }
#endif

#ifndef NDEBUG
    for(auto [r,s]: delta_step)
    {
        for(int r2: steps[s].created_regs)
            assert(not has_step(r2));
    }
#endif

    // Remove deltas from force_count where don't change anything.
    auto& delta_force_count = vm_force_count.delta();
    for(int i=0;i<delta_force_count.size();)
    {
        auto [r,count] = delta_force_count[i];

        if (count == prog_force_counts[r])
        {
            if (i + 1 < delta_force_count.size())
                std::swap(delta_force_count[i], delta_force_count.back());
            delta_force_count.pop_back();
        }
        else
            i++;
    }

    assert(root_token == token_for_context(c));

    // 8. Mark the current token as a previous_program_token.
    int t = token_for_context(c);
    auto t2 = unset_prev_prog_token(t);
    set_prev_prog_token(t, prev_prog_token_t(t,0,true));
    if (t2)
        release_unreferenced_tips(*t2);

    // 9. Release marker context c2
    release_context(c2);

    check_tokens();

#ifdef DEBUG_MACHINE
    for(int r=1;r<regs.size();r++)
        if (not regs.is_free(r))
            assert(prog_force_counts[r] == force_count(r));
#endif

    return token_for_context(c);
}

void reg_heap::first_evaluate_program(int c)
{
    if (not program_result_head)
        throw myexception()<<"No program has been set!";

    assert(get_prev_prog_token_for_context(c));

    // 1. Execute with reforce = true.  (For the first execution, this shouldn't matter though.)
    assert(token_for_context(c) == root_token);
    auto [program_result_reg, _] = incremental_evaluate(heads[*program_result_head], true);
    heads[*program_result_head] = program_result_reg;

    assert(get_prev_prog_token_for_context(c));
    assert(is_program_execution_token(*get_prev_prog_token_for_context(c)));

    // 2. Nothing to unmap!

    // 3. Perform any pending registration or unregistration of effects.
    do_pending_effect_registrations();
    assert(steps_pending_effect_unregistration.empty());

    // 4. Update force_counts
    for(auto& S: steps)
    {
        auto& R = regs[S.source_reg];

        // 3a. Count uses
        for(auto [ur,_]: R.used_regs)
            prog_force_counts[ur]++;

        // 3b. Count forces
        for(auto [fr,_]: R.forced_regs)
            prog_force_counts[fr]++;

        // 3c. Count calls
        if (reg_is_changeable(S.call))
            prog_force_counts[S.call]++;
    }

    if (reg_is_changeable(program_result_reg))
    {
        prog_force_counts[program_result_reg]++;
        assert(steps.size() > 0);
    }

    // Check that all the priors and likelihoods are forced.
#ifndef NDEBUG
    for(auto [s,r_likelihood]: likelihood_heads)
    {
        assert(reg_exists(r_likelihood));
        assert(reg_has_value(r_likelihood));
    }

    for(auto [s,r_pdf]: random_variables)
    {
        assert(reg_exists(r_pdf));
        assert(reg_has_value(r_pdf));
    }
#endif

#ifdef DEBUG_MACHINE
    for(int r=1;r<regs.size();r++)
    {
        if (not regs.is_free(r))
            assert(prog_force_counts[r] == force_count(r));
        if (has_step(r))
            assert(prog_force_counts[r] > 0);
    }
#endif
}

bool reg_heap::simple_set_path_to(int child_token) const
{
    assert(token_is_used(child_token));

    for(int t = child_token; t != root_token; t = tokens[t].parent)
    {
        if (tokens[t].type != token_type::set) return false;

        if (t != child_token and tokens[t].children.size() != 1) return false;
    }

    return true;
}

vector<pair<int,closure>> reg_heap::find_set_regs_on_path(int child_token) const
{
    assert(token_is_used(child_token));

    vector<pair<int,closure>> reg_values;
    for(int t = child_token; t != root_token; t = tokens[t].parent)
    {
        assert(tokens[t].type != token_type::reverse_set);
        assert(tokens[t].type != token_type::reverse_set_unshare);
        if (tokens[t].type == token_type::set or tokens[t].type == token_type::set_unshare)
        {
            auto [reg0, step0] = tokens[t].vm_step.delta()[0];
            auto [reg1, step1] = tokens[t].vm_step.delta()[1];
            auto value = closure_at(reg1);

            assert(is_modifiable(expression_at(reg0)));
            assert(steps[step0].call == reg1);
            assert(step1 == non_computed_index);
            assert(value.exp.is_atomic());

            reg_values.push_back({reg0, value});
        }
    }
    std::reverse(reg_values.begin(), reg_values.end());
    assert(not reg_values.empty());

    return reg_values;
}

expression_ref reg_heap::unshare_and_evaluate_program(int c)
{
    // 1. Reroot to the PPET
    int t = token_for_context(c);
    int PPET = tokens[t].prev_prog_token->token;
    reroot_at_token(PPET);

    // 2. Ensure that the path from the PPET is composed only of SET tokens!
    if (not simple_set_path_to(t))
    {
        auto reg_values = find_set_regs_on_path(t);
        set_token_for_context(c, PPET);
        for(auto& [reg, value]: reg_values)
            set_reg_value_in_context(reg, std::move(value), c);
        t = token_for_context(c);
    }

    assert( simple_set_path_to(t) );

    // 3. Merge the set tokens and all the result an execute token.

    // NOTE: This creates merged SET tokens, which violates the assumptions of find_set_regs_on_path( ).
    //       Therefore we need to ensure that find_set_regs_on_path( ) never sees these.
    release_knuckle_tokens(t);
    assert(tokens[t].parent == root_token);
    tokens[t].type = token_type::execute;

    // 4. Unshare regs in the token.
    unshare_regs2(t);

    // 5. Always perform execution in a new token.
    // Evaluation with re-force=true should be in a new context in we've
    // don't any previous evaluation with re-force=false, in order to avoid
    // double-unsharing of forces.
    {
        // We need to reroot to here first, so that switching to a child token
        // doesn't delete the current token as a knuckle.
        switch_to_child_token(c, token_type::execute);

        // This should not allow removing the old root token.
        reroot_at_context(c);

        // We can't remove t1 even if its a knuckle.
        assert(execution_allowed());
    }

    // 6. Execute with reforce = true
    auto result = lazy_evaluate2(heads[*program_result_head], c).exp;

    assert(get_prev_prog_token_for_context(c));
    assert(is_program_execution_token(*get_prev_prog_token_for_context(c)));

    return result;
}

expression_ref reg_heap::evaluate_program(int c)
{
    if (not program_result_head)
        throw myexception()<<"No program has been set!";

    assert(get_prev_prog_token_for_context(c));

    expression_ref result;

    // 1. If we can revert to a previously executed program instead of unmapping, then do that.
    int t = token_for_context(c);
    if (tokens[t].prev_prog_token->can_revert)
    {
        // assert that there are only execution tokens on the path to the previous evaluation token.
        assert(is_program_execution_token(*get_prev_prog_token_for_context(c)));
        set_token_for_context(c,t);
        reroot_at_context(c);
        int r = heads[*program_result_head];
        assert(reg_is_constant(r) or has_force(r));

        result = value_for_precomputed_reg(r).exp;
    }
    else
    {
        // 2. Actually evaluate the program.
        unshare_and_evaluate_program(c);
        
        // 3. Remove unforced steps.
        unmap_unforced_steps(c);
    }

    // 4. Perform any pending registration or unregistration of effects.
    do_pending_effect_registrations();
    assert(steps_pending_effect_unregistration.empty());
    assert(steps_pending_effect_registration.empty());

    // 5. Check that all the priors and likelihoods are forced.
#ifndef NDEBUG
    for(auto [s,r_likelihood]: likelihood_heads)
    {
        assert(reg_exists(r_likelihood));
        assert(reg_has_value(r_likelihood));
    }

    for(auto [s,r_pdf]: random_variables)
    {
        assert(reg_exists(r_pdf));
        assert(reg_has_value(r_pdf));
    }
#endif

    return result;
}

prob_ratios_t reg_heap::probability_ratios(int c1, int c2)
{
#if DEBUG_MACHINE >= 2
    for(auto x : prog_temp)
        assert(x.none());
#endif

    // 1. reroot to c1 and force the program
//  auto L1 = likelihood_for_context(c1);
    evaluate_program(c1);

    // 2. install handlers for register_random_variable and register_likelihood
    std::unordered_map<int,log_double_t> priors1;
    std::unordered_map<int,log_double_t> priors2;

    std::unordered_map<int,log_double_t> likelihoods1;
    std::unordered_map<int,log_double_t> likelihoods2;

    std::function<void(const effect&, int)> register_prior_handler = [&,this](const effect& e, int)
    {
        auto & E = dynamic_cast<const ::register_random_variable&>(e);
        assert(not priors2.count(E.variable_reg));
        priors2.insert({E.variable_reg, E.pdf});
    };

    std::function<void(const effect&, int)> unregister_prior_handler = [&,this](const effect& e, int)
    {
        auto & E = dynamic_cast<const ::register_random_variable&>(e);
        assert(not priors1.count(E.variable_reg));
        priors1.insert({E.variable_reg, E.pdf});
    };

    std::function<void(const effect&, int)> register_likelihood_handler = [&,this](const effect& e, int)
    {
        auto & E = dynamic_cast<const register_likelihood&>(e);
        assert(not likelihoods2.count(E.likelihood_reg));
        likelihoods2.insert({E.likelihood_reg, E.likelihood});
    };

    std::function<void(const effect&, int)> unregister_likelihood_handler = [&,this](const effect& e, int)
    {
        auto & E = dynamic_cast<const register_likelihood&>(e);
        assert(not likelihoods1.count(E.likelihood_reg));
        likelihoods1.insert({E.likelihood_reg, E.likelihood});
    };

    register_likelihood_handlers.push_back(register_likelihood_handler);
    unregister_likelihood_handlers.push_back(unregister_likelihood_handler);
    register_prior_handlers.push_back(register_prior_handler);
    unregister_prior_handlers.push_back(unregister_prior_handler);

    // 3. reroot to c2 and force the program
    evaluate_program(c2);

    // 4. compute the ratio only for (i) changed pdfs that (ii) exist in both c1 and c2
    prob_ratios_t R;
    R.variables_changed = priors1.size() != priors2.size();

    for(auto [r_variable, pdf1]: priors1)
    {
        auto it2 = priors2.find(r_variable);

        if (it2 == priors2.end())
        {
            R.variables_changed = true;
            continue;
        }

        auto pdf2 = it2->second;
        R.prior_ratio *= (pdf2 / pdf1);
    }

    for(auto [r_likelihood, likelihood1]: likelihoods1)
    {
        auto it2 = likelihoods2.find(r_likelihood);

        if (it2 == priors2.end())
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

        if (it1 == priors2.end())
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

//  auto L2 = likelihood_for_context(c2);
//
//  If L1 and L2 are off far enough, this test will fail...
//  if (L1 > 0.0 and L2 > 0.0)
//      assert( std::abs( (L2/L1).log() - R.likelihood_ratio.log()) < 1.0e-4 );


    return R;
}

void reg_heap::register_random_variable(const effect& e, int s)
{
    auto& E = dynamic_cast<const ::register_random_variable&>(e);
    // We aren't supposed to ever register the same step twice.
    assert(not random_variables.count(s));
    random_variables[s] = E.variable_reg;
    for(auto& handler: register_prior_handlers)
        handler(e, s);
}

void reg_heap::unregister_random_variable(const effect& e, int s)
{
    assert(random_variables.count(s));
    random_variables.erase(s);

    // FIXME: run these in reverse order?
    for(auto& handler: unregister_prior_handlers)
        handler(e, s);
}

void reg_heap::register_transition_kernel(int r_rate, int r_kernel, int /*s*/)
{
    // We can't register index_vars -- they could go away!
    assert(not expression_at(r_rate).is_index_var());

    assert(not expression_at(r_kernel).is_index_var());

    // Multiple steps from different contexts COULD register the same transition kernel.
    transition_kernels_.push_back({r_rate, r_kernel});
}

void reg_heap::unregister_transition_kernel(int r_kernel, int /*s*/)
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
    else if (is_seq(C.exp))
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
    if (reg_is_constant(r))
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

void reg_heap::force_reg(int r)
{
    assert(reg_is_changeable(r));
    assert(has_step(r));
    assert(has_result(r));
    assert(not has_force(r));

    int s = step_index_for_reg(r);

    assert(reg_is_constant(steps[s].call) or reg_is_changeable(steps[s].call));
    if (reg_is_changeable(steps[s].call))
        assert(has_result(steps[s].call));

    // We can't use a range-for here because regs[r] can be moved
    // during the loop if we do evaluation.
    for(int i=0; i < regs[r].used_regs.size(); i++)
    {
        auto [r2,_] = regs[r].used_regs[i];
        if (not has_force(r2))
            incremental_evaluate(r2, true);
        assert(has_result(r2));
        assert(has_force(r2));
    }

    for(int i=0; i < regs[r].forced_regs.size(); i++)
    {
        auto [r2,_] = regs[r].forced_regs[i];
        if (not has_force(r2))
            incremental_evaluate(r2, true);
        assert(has_result(r2));
        assert(has_force(r2));
    }

    // If R2 is WHNF then we are done
    int call = steps[s].call;
    assert(call > 0);
    if (not reg_is_constant(call))
    {
        if (not has_force(call))
            incremental_evaluate(call, true);
        assert(has_result(call));
        assert(has_force(call));
    }
}


void reg_heap::force_reg2(int r)
{
    assert(reg_is_changeable(r));
    assert(has_step(r));
    assert(has_result(r));
    assert(not has_force(r));

    int s = step_index_for_reg(r);

    assert(reg_is_constant(steps[s].call) or reg_is_changeable(steps[s].call));
    if (reg_is_changeable(steps[s].call))
        assert(has_result(steps[s].call));

    // We can't use a range-for here because regs[r] can be moved
    // during the loop if we do evaluation.
    for(int i=0; i < regs[r].used_regs.size(); i++)
    {
        auto [r2,_] = regs[r].used_regs[i];
        if (not has_force(r2))
            incremental_evaluate2(r2);
        assert(has_result(r2));
        assert(has_force(r2));
    }

    for(int i=0; i < regs[r].forced_regs.size(); i++)
    {
        auto [r2,_] = regs[r].forced_regs[i];
        if (not has_force(r2))
            incremental_evaluate2(r2);
        assert(has_result(r2));
        assert(has_force(r2));
    }

    // If R2 is WHNF then we are done
    int call = steps[s].call;
    assert(call > 0);
    if (not reg_is_constant(call))
    {
        if (not has_force(call))
            incremental_evaluate2(call);
        assert(has_result(call));
        assert(has_force(call));
    }
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

void reg_heap::set_forced_reg(int r1, int r2)
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
    int back_index = R2.forced_by.size();
    int forw_index = R1.forced_regs.size();
    R2.forced_by.push_back({r1,forw_index});
    R1.forced_regs.push_back({r2,back_index});

    assert(reg_is_forced_by(r1,r2));
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

void reg_heap::mark_step_with_effect(int s)
{
    steps[s].mark_with_effect();
}

int reg_heap::allocate()
{
    total_reg_allocations++;
    int r = regs.allocate();
    mark_reg_unevaluated(r);
    return r;
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
    tokens[t].vm_force.add_value(r, non_computed_index);
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

    assert(tokens[t].vm_force.empty());
    tokens[t].vm_force.add_value(R, non_computed_index);

    assert(not children_of_token(t).size());

    // if the value is NULL, just leave the value and call both unset.
    //  (this could happen if we set a parameter value to null.)
    if (not value) return;

    // If the value is a pre-existing reg_var, then call it.
    if (value.exp.head().type() == index_var_type)
    {
        int Q = value.reg_for_index_var();

        // Never set the call to an index var.
        Q = follow_index_var(Q);

        // Never call something unevaluated either.
        assert(not reg_is_unevaluated(Q));

        // Set the call
        set_call(s, Q);
    }
    // Otherwise, regardless of whether the expression is WHNF or not, create a new reg for the value and call it.
    else
    {
        assert(value.exp.size() == 0);
        assert(is_WHNF(value.exp));

        int R2 = allocate_reg_from_step_in_token(s,t);

        mark_reg_constant(R2);

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

    for(const auto& [r2,_]: regs[r].forced_regs)
        U.push_back(r2);

    return U;
}

void reg_heap::reclaim_used(int r)
{
    // Mark this reg as not used (but not free) so that we can stop worrying about upstream objects.
    assert(not has_step(r));

    // Clear any force counts.
    // This reg was in a tip token that could have forced it.  But no other programs will force it.
    prog_force_counts[r] = 0;
  
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
    prog_forces.resize(regs.size());
    prog_force_counts.resize(regs.size());
    prog_temp.resize(regs.size());
    prog_unshare.resize(regs.size());

    // Now we can use size() again.
    for(auto i=old_size;i<size();i++)
    {
        prog_steps[i] = non_computed_index;
        prog_results[i] = non_computed_index;
        prog_forces[i] = non_computed_index;

        assert(prog_steps[i] == non_computed_index);
        assert(prog_results[i] == non_computed_index);
        assert(prog_forces[i] == non_computed_index);
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
    for(auto& [r,_]: regs[r2].used_by)
        if (r == r1)
            return true;

    return false;
}

bool reg_heap::reg_is_forced_by(int r1, int r2) const
{
    for(auto& [r,_]: regs[r2].forced_by)
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

            // Check that forward prev_prog_token edges are right.
            if (tokens[t].prev_prog_token and tokens[t].prev_prog_token->index)
            {
                int t2 = tokens[t].prev_prog_token->token;
                int j = *tokens[t].prev_prog_token->index;
                assert(tokens[t2].used);

                auto& prev_prog_refs = (tokens[t].n_context_refs > 0)
                    ? tokens[t2].prev_prog_active_refs
                    : tokens[t2].prev_prog_inactive_refs;

                assert(prev_prog_refs[j] == t);
            }

            // Check that backward prev_prog_token edges are right.
            for(auto t2: tokens[t].prev_prog_active_refs)
            {
                assert(tokens[t2].used);
                assert(tokens[t2].prev_prog_token);
                assert(tokens[t2].prev_prog_token->token == t);
            }
            for(auto t2: tokens[t].prev_prog_inactive_refs)
            {
                assert(tokens[t2].used);
                assert(tokens[t2].prev_prog_token);
                assert(tokens[t2].prev_prog_token->token == t);
            }
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

    constexpr int force_count_bit = 3;
    constexpr int force_bit = 2;
    constexpr int result_bit = 0;
    constexpr int step_bit = 1;

    for(auto [r,count]: tokens[t].delta_force_count())
    {
        // Regs can have inaccurate force counts if they are not program-execution tokens.
        // So its possible to have positive force counts for destroyed regs, in tokens where
        // the previous program-execution token is destroyed.

        // assert(not regs.is_free(r) or count == 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(force_bit));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(force_count_bit);

        // No results for constant regs
        assert(count >= 0);
    }

    for(auto [r,result]: tokens[t].delta_force())
    {
        // Deltas should not contain free regs except resets.
        assert(not regs.is_free(r) or result < 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(force_bit));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(force_bit);

        // No results for constant regs
        if (result > 0)
            assert(not reg_is_constant(r));
    }

    for(auto [r,result]: tokens[t].delta_result())
    {
        // Deltas should not contain free regs except resets.
        assert(not regs.is_free(r) or result < 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(result_bit));

        // Mark the reg as having a result in the delta.
        prog_temp[r].set(result_bit);

        // No results for constant regs
        if (result > 0)
            assert(not reg_is_constant(r));
    }

    for(auto [r,step]: tokens[t].delta_step())
    {
        // Deltas should not contain free regs except resets.
        assert(not regs.is_free(r) or step < 0);

        // Check that there are no duplicate regs.
        assert(not prog_temp[r].test(step_bit));

        // Mark the reg as having a step in the delta.
        prog_temp[r].set(step_bit);

        // If the step is unshared, the result must be unshared as well: this allows us to just walk unshared results.
        assert(prog_temp[r].test(result_bit) and prog_temp[r].test(step_bit));
        // No steps for constant regs
        if (step > 0)
            assert(not reg_is_constant(r));
    }

    // FIXME - nonlocal. The same result/step are not set in multiple places!

    for(auto [reg,res]: tokens[t].delta_force_count())
        prog_temp[reg].reset(force_count_bit);

    for(auto [reg,res]: tokens[t].delta_force())
    {
        prog_temp[reg].reset(force_bit);
        prog_temp[reg].reset(result_bit);
        prog_temp[reg].reset(step_bit);
    }

    for(auto [reg,res]: tokens[t].delta_result())
    {
        prog_temp[reg].reset(force_bit);
        prog_temp[reg].reset(result_bit);
        prog_temp[reg].reset(step_bit);
    }

    for(auto [reg,step]: tokens[t].delta_step())
    {
        prog_temp[reg].reset(force_bit);
        prog_temp[reg].reset(result_bit);
        prog_temp[reg].reset(step_bit);
    }
}

void reg_heap::check_used_regs() const
{
    assert(tokens[root_token].vm_step.empty());
    assert(tokens[root_token].vm_result.empty());
    assert(tokens[root_token].vm_force.empty());
    assert(tokens[root_token].vm_force_count.empty());

    for(int t=0; t< tokens.size(); t++)
        if (token_is_used(t))
            check_used_regs_in_token(t);

    for(auto& S:steps)
    {
        if (S.call > 0)
            assert(not regs.is_free(S.call));
    }

    bool check_force_counts = is_program_execution_token(root_token);

    for(auto i = regs.begin(); i != regs.end(); i++)
    {
        int r1 = i.addr();

        if (check_force_counts)
        {
            assert(prog_force_counts[r1] == force_count(r1));
            if (has_step(r1))
                assert(prog_force_counts[r1] > 0);
        }

        if (prog_force_counts[r1] > 0)
            assert(reg_is_changeable(r1));

        if (not regs[r1].used_regs.empty())
            assert(reg_is_changeable(r1));

        if (not regs[r1].forced_regs.empty())
            assert(reg_is_changeable(r1));

        if (not has_step(r1))
        {
            assert(not has_force(r1));
            assert(not has_result(r1));
        }

        if (not has_result(r1))
        {
            assert(not has_force(r1));
        }

        if (has_force(r1))
        {
            for(auto r2: used_regs_for_reg(r1))
                assert(has_force(r2));
            for(auto r2: forced_regs_for_reg(r1))
                assert(has_force(r2));
            int call = step_for_reg(r1).call;
            if (reg_is_changeable(call))
                assert(has_force(call));
        }

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
        for(const auto& [r2,_]: regs[r1].forced_regs)
        {
            // Used regs should have back-references to R
            assert( reg_is_forced_by(r1, r2) );

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


    // 2. When destroying a reg, remove edge from regs[r] <---forced_by--- regs[r3]
    assert(r > 0);
    for(auto& forward: regs[r].forced_regs)
    {
        auto [r3,j] = forward;
        if (regs.is_free(r3)) continue;
        auto& backward = regs[r3].forced_by;
        assert(0 <= j and j < backward.size());

        forward = {0,0};

        if (j+1 < backward.size())
        {
            // erase the backward edge by moving another backward edge on top of it.
            backward[j] = backward.back();
            auto [r2,i2] = backward[j];
            // adjust the forward edge for that backward edge
            auto& forward2 = regs[r2].forced_regs;
            assert(0 <= i2 and i2 < forward2.size());
            forward2[i2].second = j;

            assert(regs[r2].forced_regs[i2].second == j);
            assert(regs[forward2[i2].first].forced_by[forward2[i2].second].second == i2);
        }

        backward.pop_back();
    }


    // 3. When destroying a reg, remove edge from step[s] ---created_regs---> regs[r]
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
    for(auto& [_,index]: regs.access_unused(r).forced_regs)
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
    if (reg_is_constant(R)) return expression_at(R);

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

const closure& reg_heap::value_for_precomputed_reg(int r) const
{
    r = follow_index_var(r);
    return access_value_for_reg(r);
}

optional<int> reg_heap::precomputed_value_in_context(int r, int c)
{
    r = follow_index_var(r);

    if (reg_is_constant(r)) return r;

    reroot_at_context(c);

    // In theory, variants of this routine could allow
    // * having a result, but no force.
    // * have a chain of steps, but no result.
    if (reg_is_changeable(r) and has_force(r))
        return result_for_reg(r);
    else
    {
        std::abort();
        return {};
    }
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

    if (not execution_allowed() or is_program_execution_token(token_for_context(c)))
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

pair<int,int> reg_heap::incremental_evaluate_in_context2(int R, int c)
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

        if (has_force(R))
        {
            assert(r2 > 0);
            return {R,r2};
        }
    }

    if (not execution_allowed() or is_program_execution_token(token_for_context(c)))
    {
        switch_to_child_token(c, token_type::execute);

        // This should not allow removing the old root token.
        reroot_at_context(c);

        // We can't remove t1 even if its a knuckle.
        assert(execution_allowed());
    }

    assert(execution_allowed());

    auto p = incremental_evaluate2(R);

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    return p;
}

const closure& reg_heap::lazy_evaluate(int& R, bool reforce)
{
    auto [R2, value] = incremental_evaluate(R, reforce);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate2(int& R)
{
    auto [R2, value] = incremental_evaluate2(R);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate(int& R, int c, bool reforce)
{
    auto [R2, value] = incremental_evaluate_in_context(R, c, reforce);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate2(int& R, int c)
{
    auto [R2, value] = incremental_evaluate_in_context2(R, c);
    R = R2;
    return closure_at(value);
}

const closure& reg_heap::lazy_evaluate_head(int index, int c, bool reforce)
{
    int R1 = heads[index];
    auto [R2, value] = incremental_evaluate_in_context(R1, c, reforce);
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
     prog_forces(1, non_existant_index),
     prog_force_counts(1, 0),
     prog_temp(1),
     prog_unshare(1)
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

