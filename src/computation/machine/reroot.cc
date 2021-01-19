#include "range/v3/all.hpp"
#include <iostream>
#include "graph_register.H"
#include <algorithm>
#include "util/range.H"
#include "mapping.H"

using std::string;
using std::vector;
using std::pair;

using std::cerr;
using std::endl;

namespace views = ranges::views;

long total_steps_pivoted = 0;
long total_results_pivoted = 0;
long total_forces_pivoted = 0;
long total_force_counts_pivoted = 0;
long total_reroot = 0;
long total_reroot_one = 0;
long total_invalidate = 0;
long total_steps_invalidated = 0;
long total_results_invalidated = 0;
long total_forces_invalidated = 0;
long total_steps_scanned = 0;
long total_results_scanned = 0;
long total_forces_scanned = 0;
long total_invalid_control_flow = 0;
long total_valid_control_flow = 0;

void reg_heap::reroot_at_context(int c)
{
    reroot_at_token(token_for_context(c));
}

void reg_heap::reroot_at_token(int t)
{
    int old_root = root_token;

    // 1. Bail if we are already at the root.
    if (is_root_token(t)) return;

    total_reroot++;

    // 2. Get the tokens on the path to the root.
    boost::container::small_vector<int,10> path;
    path.push_back(t);
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

    // 4. Unregister steps that were in the initial root but not the final root.

    // We postpone registrations until evaluate_program( ), but UNregistrations are performed here so that
    // we don't have to scan for unregistrations in destroy_all_computations_in_token( ).
    do_pending_effect_unregistrations();

    // 4. Clean up old root token if it became an unused tip
    int t2 = release_unreferenced_tips(old_root);

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif

    // 5. Remove sequences of knuckles - only remove a knuckle if its child was part of the original path
//    for(; t2 != root_token;)
//        t2 = release_knuckle_tokens(t2);

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif
}


// reroot_at( ) is only called from (i) itself and (ii) reroot_at_context( ).
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
    maybe_unshare_regs(t);

    // 3. Change the relative mappings
    total_steps_pivoted += tokens[t].delta_step().size();
    total_results_pivoted += tokens[t].delta_result().size();
    total_force_counts_pivoted += tokens[t].delta_force_count().size();
    pivot_mapping(prog_steps, tokens[t].vm_step);
    std::swap(tokens[parent].vm_step, tokens[t].vm_step);
    pivot_mapping(prog_results, tokens[t].vm_result);
    std::swap(tokens[parent].vm_result, tokens[t].vm_result);
    pivot_mapping(prog_force_counts, tokens[t].vm_force_count);
    std::swap(tokens[parent].vm_force_count, tokens[t].vm_force_count);

    // 4. Alter the inheritance tree
    tokens[t].type = reverse(tokens[t].type);
    std::swap(tokens[t].type, tokens[parent].type);

    tokens[parent].parent = t;
    int index = remove_element(tokens[parent].children, t);
    assert(index != -1);

    tokens[t].parent = -1;
    tokens[t].children.push_back(parent);

    root_token = t;
    assert(is_root_token(t));

    // 5. Pivot effects
    for(auto& [r,s1]: tokens[parent].delta_step())
    {
        if (s1 > 0 and steps.access(s1).has_effect())
        {
            if (steps[s1].has_pending_effect_registration())
                unmark_effect_to_register_at_step(s1);
            else
                mark_effect_to_unregister_at_step(s1);
        }

        int s2 = step_index_for_reg(r);
        if (s2 > 0 and steps.access(s2).has_effect())
            mark_effect_to_register_at_step(s2);
    }

    total_reroot_one++;
  
    for(int t2: tokens[t].children)
        assert(tokens[t2].type != token_type::reverse_set);

    assert(is_root_token(t));
}


void reg_heap::check_created_regs_unshared(int t)
{
    const auto& delta_step   = tokens[t].vm_step.delta();

    for(auto [_,s]: delta_step)
    {
        if (s < 0) continue;

        const auto& Step = steps[s];

        // Any results or steps in the delta should already have their regs unshared.
        for(int r2: Step.created_regs)
        {
            if (prog_steps[r2] > 0)
            {
                assert(reg_is_changeable(r2));
                assert(prog_temp[r2].test(unshare_result_bit));
                assert(prog_temp[r2].test(unshare_step_bit));
            }
        }
    }
}

/*
 * In the current framework, regs may be incorrectly shared, if they are not overridden in the child and should be.
 * However, if a reg is overridden in the child, then its result must be correct.
 * Therefore before pivoting a child of the root into the root, we just need to unshare steps and results that should not be shared.
 *
 * Consider a child of the root.
 *
 * - Some STEPS   in the root may be incorrectly shared into the child.
 *   This happens if the root STEP    uses a  RESULT that is overridden in the child, but the step   is shared into the child.
 *
 * - Some RESULTS in the root may be incorrectly shared into the child.
 *   This happens if the child RESULT calls a RESULT that is overridden in the child, but the result is shared into the child.
 *   The result is also incorrectly shared if the corresponding step is incorrectly shared.
 *
 * If a step is unshared, then its result must also be unshared.  Therefore, if we want to walk a list of unshared things that
 * we wish to consider processessing at a later time, we can just want the list of unshared results.
 *
 * Now, if we unshare all regs that are created in the root but overridden in the child,
 * then the child won't inherit any values from the root that it can't access.
 * This might help to ensure that when we delete a token, we destroy all of the objects that it created.
 * Currently we can't destroy created regs at the time of token destruction, because their steps might have floated down to the root.
 *
 * How do we add to this loop an unsharing of result AND steps for regs that were created by steps?
 * It seems like we could, in theory walk all the steps that we unshare after each round, and invalidate their steps and results,
 *  and then restart the inner loop to invalidate downstream steps and results.
 * result <-- called_by -- result
 * result <-- used_by  --- (step,result)
 *                         step <--- created_by --- reg <---located-at-- (step,result)
 */

void reg_heap::unshare_regs1(int t)
{
    // parent_token(t) should be the root.
    assert(is_root_token(parent_token(t)));
    assert(tokens[t].type != token_type::reverse_set);

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    total_invalidate++;

    auto& vm_result = tokens[t].vm_result;
    auto& vm_step = tokens[t].vm_step;

    // find all regs in t that are not shared from the root
    const auto& delta_result = vm_result.delta();
    const auto& delta_step   = vm_step.delta();

    int n_delta_result0 = delta_result.size();
    int n_delta_step0   = delta_step.size();

    auto unshare_result = [&](int r)
                              {
                                  // This result is already unshared
                                  if (not prog_temp[r].test(unshare_result_bit))
                                  {
                                      prog_temp[r].set(unshare_result_bit);
                                      vm_result.add_value(r, non_computed_index);
                                  }
                              };

    auto unshare_step = [&](int r)
                            {
                                // This step is already unshared
                                if (prog_temp[r].test(unshare_step_bit)) return;

                                unshare_result(r);

                                prog_temp[r].set(unshare_step_bit);
                                vm_step.add_value(r, non_computed_index);
                            };

    // 1. Mark unshared regs in prog_temp.

    // 1a. All the regs with delta_result set have results invalidated in t
    for(auto [r,_]: delta_result)
    {
        prog_temp[r].set(unshare_result_bit);
        assert(prog_temp[r].test(unshare_result_bit));
    }

    // 1b. All the regs with delta_step set have steps (and results) invalidated in t
    for(auto [r,_]: delta_step)
    {
        prog_temp[r].set(unshare_step_bit);
        assert(prog_temp[r].test(unshare_result_bit));
        assert(prog_temp[r].test(unshare_step_bit));
    }

#ifndef NDEBUG
    check_created_regs_unshared(t);
#endif

    int i =0; // (FIXME?) We have to rescan all the existing steps and results because there might be new EDGES to them that have been added.

    // 2. Scan regs with different result in t that are used/called by root steps/results
    for(;i<delta_result.size();i++)
        if (auto [r,_] = delta_result[i]; has_result1(r))
        {
            auto& R = regs[r];

	    // Look at steps that CALL the reg in the root (that has overridden result in t)
            for(int s2: R.called_by)
                if (int r2 = steps[s2].source_reg; prog_steps[r2] == s2)
                    unshare_result(r2);

	    // Look at steps that CALL the reg in the root (that has overridden result in t)
            for(int r2: R.called_by_index_vars)
                unshare_result(r2);

	    // Look at steps that USE the reg in the root (that has overridden result in t)
            for(auto& [r2,_]: R.used_by)
                if (prog_steps[r2] > 0)
                    unshare_step(r2);
        }


    // 4. Scan unshared steps, and unshare steps for created regs IF THEY HAVE A STEP.

//  int j = delta_step.size();
    int j=0; // FIXME if the existing steps don't share any created regs, then we don't have to scan them.
             // FIXME: while the overriding steps in the child should have their created regs unshared, the overridden steps in the root need not!
             //        this means that we need to scan all overridden steps each time :-(

    // Also unshare any results and steps that are for regs created in the root context.
    // LOGIC: Any reg that uses or call a created reg must either
    //          (i) be another created reg, or
    //          (ii) access the created reg through a chain of use or call edges to the step that created the reg.
    //        In the former case, this loop handles them.  In the later case, they should be invalid.
    for(;j<delta_step.size();j++)
        if (auto [r,_] = delta_step[j]; has_step1(r))
            for(int r2: step_for_reg(r).created_regs)
            {
                if (prog_steps[r2] > 0)
                {
                    assert(reg_is_changeable(r2));
                    unshare_step(r2);
                }
                else if (prog_results[r2] > 0)
                {
                    assert(reg_is_changeable_or_forcing(r2));
                    unshare_result(r2);
                }
            }

#ifndef NDEBUG
    check_created_regs_unshared(t);
#endif

    // 5. Erase the marks that we made on prog_temp.
    for(auto [r,_]: delta_result)
    {
        prog_temp[r].reset(unshare_result_bit);
        prog_temp[r].reset(unshare_step_bit);
    }

    total_results_invalidated += (delta_result.size() - n_delta_result0);
    total_steps_invalidated += (delta_step.size() - n_delta_step0);

    total_results_scanned += delta_result.size();
    total_steps_scanned += delta_step.size();

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif
}

template <typename T,typename F>
void filter_unordered_vector(vector<T>& x, F ok)
{
    for(int i=0;i<x.size();)
    {
        if (ok(x[i]))
            i++;
        else
        {
            x[i] = std::move(x.back());
            x.pop_back();
        }
    }
}

// find all regs in t that are not shared from the root
void reg_heap::find_unshared_regs(vector<int>& unshared_regs, vector<int>& zero_count_regs_initial, int t)
{
    auto& delta_result = tokens[t].vm_result.delta();
    auto& delta_step   = tokens[t].vm_step.delta();

    auto unshare_result = [&](int r)
                              {
                                  assert(reg_is_changeable_or_forcing(r));

                                  if (prog_unshare[r].none())
                                      unshared_regs.push_back(r);
                                  prog_unshare[r].set(unshare_result_bit);
                              };

    auto unshare_step = [&](int r)
                            {
                                assert(reg_is_changeable(r));

                                if (prog_unshare[r].none())
                                    unshared_regs.push_back(r);
                                prog_unshare[r].set(unshare_result_bit);
                                prog_unshare[r].set(unshare_step_bit);
                            };

    // 1. Mark unshared regs.  All modified regs should have STEP/RESULT/FORCE unshared.
    for(auto [r,s]: delta_step)
    {
        assert(s > 0);
        if (not reg_is_forced(r))
            zero_count_regs_initial.push_back(r);
        unshare_step(r);
    }
    // All the same regs should have (r,-) in the result.
    assert(delta_step.size() == delta_result.size());

#ifndef NDEBUG
    check_created_regs_unshared(t);
#endif

    // 2. Scan regs with different result in t that are used/called by root steps/results
    for(int i=0;i<unshared_regs.size();i++)
    {
        auto& R = regs[unshared_regs[i]];

        // Look at steps that CALL the reg in the root (that has overridden result in t)
        for(int s2: R.called_by)
            if (int r2 = steps[s2].source_reg; prog_steps[r2] == s2 and has_result1(r2))
                unshare_result(r2);

        // Look at steps that have a FIXED CALL to the reg in the root (that has overridden result in t)
        for(int r2: R.called_by_index_vars)
            if (has_result1(r2))
                unshare_result(r2);

        // Look at steps that USE the reg in the root (that has overridden result in t)
        for(auto& [r2,_]: R.used_by)
            if (prog_steps[r2] > 0 and has_result1(r2))
                unshare_step(r2);
    }
}

// Remove (r,-) entries from Delta, and remove the unshare bit for the (r,>0) remainder.
void reg_heap::tweak_deltas_and_unshare_bits(int t)
{
    auto& delta_result = tokens[t].vm_result.delta();
    auto& delta_step   = tokens[t].vm_step.delta();

    // 1. We can't have (r,-) entries in deltas, because they if we execute them, we would add a
    //     second override during execution.
    auto keep = [](const pair<int,int>& p) {return p.second > 0;};

    filter_unordered_vector(delta_step, keep);
    filter_unordered_vector(delta_result, keep);

    // 2. We can't have bits on the remaining (r,+) entries in the deltas during execution
    //    because (r,x)* is really shorthand for -/x at r, and we have x/y at r.
    for(auto& [r,_]: delta_step)
        prog_unshare[r].reset(unshare_step_bit);

    for(auto& [r,_]: delta_result)
        prog_unshare[r].reset(unshare_result_bit);
}

void reg_heap::reroot_at_token_with_tweaked_deltas_and_bits(int t)
{
    assert(is_root_token(parent_token(t)));
    int t2 = root_token;

    // 1. Mark tokens as having tweaked bits.
    tokens[t].flags.set(0);
    tokens[t2].flags.set(0);

    // 2. Mark tokens as having tweaked bits.
    reroot_at_token(t);

    assert(tokens[t].children.size() == 1);
    assert(tokens[t].children[0] == t2);

    // 3. Unmark tokens
    tokens[t].flags.reset(0);
    tokens[t2].flags.reset(0);
}

void reg_heap::increment_counts_from_new_calls()
{
    int t2 = tokens[root_token].children[0];

    for(auto& [r,_]: tokens[t2].vm_step.delta())
    {
        if (reg_is_forced(r))
        {
            int call = call_for_reg(r);
            if (reg_is_changeable_or_forcing(call))
                inc_count(call);
        }
    }
}

void reg_heap::evaluate_unconditional_regs(const vector<int>& unshared_regs)
{
    for(int r: unshared_regs | views::reverse)
        if (regs[r].is_unconditionally_evaluated() and not has_result2(r))
        {
            incremental_evaluate2(r,false);
            assert(has_result2(r));
        }
}

void reg_heap::decrement_counts_from_invalid_calls(const vector<int>& unshared_regs, vector<int>& zero_count_regs)
{
    int t2 = tokens[root_token].children[0];
    
    auto* vm_step2   = &tokens[t2].vm_step;
    auto* vm_count2  = &tokens[t2].vm_force_count;

    auto dec_force_count = [&](int r)
    {
        assert(reg_is_changeable_or_forcing(r));
        if (not prog_unshare[r].test(unshare_count_bit))
        {
            prog_unshare[r].set(unshare_count_bit);
            vm_count2->add_value(r, prog_force_counts[r]);
        }

        prog_force_counts[r]--;
        assert(prog_force_counts[r] >= 0);

        if (prog_force_counts[r] == 0)
            zero_count_regs.push_back(r);
    };

    // 1. Decrement calls from bumped steps
    for(auto& [r,s]: vm_step2->delta())
    {
        // We can get bumped -1 steps here from executing new regs.
        if (s > 0)
        {
            // But we should only be able to bump old steps here with new valid steps.
            assert(prog_steps[r] > 0);

            int call = steps[s].call;
            if (reg_is_changeable_or_forcing(call))
                dec_force_count(call);
        }
    }

    // 2. Decrement calls from invalid steps.
    int n_invalid_control_flow = 0;
    for(int r : unshared_regs)
    {
        // At this point, unshare_step bit only picks out regs that are really -1,
        // and so excludes e.g. the calls from modifiables.
        if (prog_unshare[r].test(unshare_step_bit))
        {
            prog_unshare[r].set(call_decremented_bit);
            int s = prog_steps[r];
            int call = steps[s].call;
            if (reg_is_changeable_or_forcing(call))
            {
                n_invalid_control_flow++;
                dec_force_count(call);
            }
        }
    }

    if (n_invalid_control_flow > 0)
        total_invalid_control_flow++;
    else
        total_valid_control_flow++;

    // 3. Iteratively decrement counts from steps of zero_count regs.
    for(int i=0;i<zero_count_regs.size();i++)
    {
        int r = zero_count_regs[i];
        for(auto [r2,_]: regs[r].used_regs)
            dec_force_count(r2);
        for(auto [r2,_]: regs[r].forced_regs)
            dec_force_count(r2);
        int ref = regs[r].index_var_ref.first;
        if (ref > 0)
            dec_force_count(ref);

        // If unshare_step_bit is set, then we've already decremented any force_count!
        if (not prog_unshare[r].test(unshare_step_bit) and reg_is_changeable(r))
        {
            int s = prog_steps[r];
            assert(s > 0 and s < steps.size());
            int call = steps[s].call;
            if (reg_is_changeable_or_forcing(call))
                dec_force_count(call);
        }
    }

}

void reg_heap::evaluate_forced_invalid_regs(const std::vector<int>& unshared_regs)
{
    for(int r: unshared_regs | views::reverse)
    {
        if (reg_is_forced(r) and not has_result2(r))
        {
            incremental_evaluate2(r,false);
            assert(has_result2(r));
        }
    }

    // We can't clear the different_result_bit until evaluation is finished!
    for(int r: unshared_regs)
        prog_unshare[r].reset(different_result_bit);

#ifndef NDEBUG
    for(int r: unshared_regs | views::reverse)
        if (reg_is_forced(r))
            assert(has_result2(r));
#endif
}

void reg_heap::cleanup_count_deltas_and_bits()
{
    int t2 = tokens[root_token].children[0];
    auto& delta_count = tokens[t2].vm_force_count.delta();

    // 1. Clear unsharing bits for vm_
    for(auto [r,_]: delta_count)
        prog_unshare[r].reset(unshare_count_bit);

    // 2. Remove count overrides that are of no effect.
    auto count_changed = [&](const pair<int,int>& p)
    {
        auto [r,count] = p;
        return count != prog_force_counts[r];
    };

    filter_unordered_vector(delta_count, count_changed);
}

void reg_heap::remove_zero_count_regs(const std::vector<int>& zero_count_regs_initial, const std::vector<int>& zero_count_regs)
{
    int t2 = tokens[root_token].children[0];

    auto* vm_result2 = &tokens[t2].vm_result;
    auto* vm_step2   = &tokens[t2].vm_step;

    // 1. Bump zero-count previously-executed regs to child token, and clear result and step bits.
    for(int r: zero_count_regs)
    {
        // Any regs that still have invalid_step or invalid_result should be zero-count,
        // so we can clear the bits here.
        prog_unshare[r].reset();

        if (reg_is_forced(r)) continue;

        if (has_result1(r))
        {
            vm_result2->add_value(r, prog_results[r]);
            prog_results[r] = non_computed_index;
        }
        if (has_step1(r))
        {
            vm_step2->add_value(r, prog_steps[r]);
            prog_steps[r] = non_computed_index;
        }
    }

    // 2. Remove new steps and results for unforced modifiables.
    //
    //    * We need to make sure that unshared results in the form [-]* are pushed into delta result;
    //
    //    * We should ideally consider modifiable that start with {0,+} counts and end with {0,+} counts.
    //      This block handles regs that start with 0 and end with 0.

    for(int r: zero_count_regs_initial)
    {
        if (reg_is_forced(r))
        {
            assert(prog_unshare[r].none());
            continue;
        }

        if (prog_unshare[r].test(unshare_result_bit))
        {
            assert(not has_result1(r));
            vm_result2->add_value(r, prog_results[r]);
        }
        prog_results[r] = non_computed_index;

        assert(not prog_unshare[r].test(unshare_step_bit));
        prog_steps[r] = non_computed_index;

        prog_unshare[r].reset();
    }
}

void reg_heap::unregister_effects_for_bumped_steps()
{
    int t2 = tokens[root_token].children[0];

    for(auto [_,s]: tokens[t2].vm_step.delta())
    {
        if (s > 0 and steps.access(s).has_effect())
        {
            if (steps[s].has_pending_effect_registration())
                unmark_effect_to_register_at_step(s);
            else
                mark_effect_to_unregister_at_step(s);
        }
    }
    do_pending_effect_unregistrations();
}

void reg_heap::mark_as_program_execution_token(int t)
{
    auto t3 = unset_prev_prog_token(t);
    set_prev_prog_token(t, prev_prog_token_t(t,0,true));
    if (t3)
        release_unreferenced_tips(*t3);
}

expression_ref reg_heap::unshare_regs2(int t)
{
    // parent_token(t) should be the root.
    assert(is_root_token(parent_token(t)));
    assert(tokens[t].type != token_type::reverse_set);

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    total_invalidate++;

    auto& delta_result = tokens[t].vm_result.delta();
    auto& delta_step   = tokens[t].vm_step.delta();

    int n_delta_result0 = delta_result.size();
    int n_delta_step0   = delta_step.size();

    // 1. Mark regs with unshared steps or result.
    auto& unshared_regs = get_scratch_list();
    auto& zero_count_regs_initial = get_scratch_list();

    find_unshared_regs(unshared_regs, zero_count_regs_initial, t);

    // 2. Fixup deltas and unshare bits
    tweak_deltas_and_unshare_bits(t);

    // 3. Reroot to token t.
    int t2 = root_token;
    reroot_at_token_with_tweaked_deltas_and_bits(t);

    // 4. Determine which invalid regs we can safely execute.

    // 4a. Increment counts for new calls, if count > 0
    increment_counts_from_new_calls();

    // 4b. Evaluate unconditionally-executed regs.
    evaluate_unconditional_regs(unshared_regs);

    // 4c. Decrement counts from invalid calls
    auto& zero_count_regs = get_scratch_list();
    decrement_counts_from_invalid_calls(unshared_regs, zero_count_regs);

    // 5. Evaluate forced invalid regs.
    evaluate_forced_invalid_regs(unshared_regs);

    // 6. Get the program result.
    auto result = lazy_evaluate2(heads[*program_result_head]).exp;

    // 7. Clear unshare_count_bit and remove no-effect override from delta-force-count
    cleanup_count_deltas_and_bits();

    // 8. Remove zero count regs (previously-executed, and new modifiables) and clear unshare_* bits on unexecuted regs.
    remove_zero_count_regs(zero_count_regs_initial, zero_count_regs);

    release_scratch_list(); // zero_count_regs
    release_scratch_list(); // zero_count_regs_initial
    release_scratch_list(); // unshared_regs

#ifdef DEBUG_MACHINE
    check_force_counts();

    for(int r=1;r<regs.size();r++)
    {
        if (regs.is_free(r)) continue;

        if (reg_is_changeable_or_forcing(r) and reg_is_forced(r))
            assert(reg_exists(r));
    }
#endif

    // 9. Handle moving steps out of the root.
    unregister_effects_for_bumped_steps();

    // 10. Mark the current token as a previous_program_token.
    mark_as_program_execution_token(t);

    auto* vm_result2 = &tokens[t2].vm_result;
    auto* vm_step2   = &tokens[t2].vm_step;

    total_results_invalidated += (vm_result2->delta().size() - n_delta_result0);
    total_steps_invalidated += (vm_step2->delta().size() - n_delta_step0);

    total_results_scanned += vm_result2->delta().size();
    total_steps_scanned += vm_step2->delta().size();

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    return result;
}

void reg_heap::check_unshare_regs(int t)
{
    assert(t >= 0);

#if DEBUG_MACHINE >= 1
    const auto& delta_result = tokens[t].vm_result.delta();
    const auto& delta_step = tokens[t].vm_step.delta();

    // All the regs with delta_result set have results invalidated in t
    for(auto [r,_]: delta_result)
    {
        prog_temp[r].set(unshare_result_bit);
        assert(prog_temp[r].test(unshare_result_bit));
    }

    // All the regs with delta_step set have steps (and results) invalidated in t
    for(auto [r,_]: delta_step)
    {
        prog_temp[r].set(unshare_step_bit);
        assert(prog_temp[r].test(unshare_result_bit));
        assert(prog_temp[r].test(unshare_step_bit));
    }

    for(int j=0;j<delta_step.size();j++)
        if (auto [r,_] = delta_step[j]; has_step1(r))
            for(int r2: step_for_reg(r).created_regs)
            {
                if (prog_steps[r2] > 0)
                {
                    assert(reg_is_changeable(r2));
                    assert(prog_temp[r].test(unshare_step_bit));
                }
                assert(prog_temp[r].test(unshare_result_bit));
            }

    // Erase the marks that we made on prog_temp.
    for(auto [r,_]: delta_result)
    {
        prog_temp[r].reset(unshare_result_bit);
        prog_temp[r].reset(unshare_step_bit);
    }
#endif
}

void reg_heap::maybe_unshare_regs(int t)
{
    // parent_token(t) should be the root.
    assert(is_root_token(parent_token(t)));
    assert(tokens[t].type != token_type::reverse_set);

    if (tokens[t].type == token_type::set)
    {
        unshare_regs1(t);

        tokens[t].type = token_type::set_unshare;
    }
    else if (tokens[t].parent != root_token or not tokens[t].flags.test(0))
        check_unshare_regs(t);
}


