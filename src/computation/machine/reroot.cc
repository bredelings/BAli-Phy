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
                assert(prog_temp[r2].test(unshare_force_bit));
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

expression_ref reg_heap::unshare_regs2(int t)
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
    auto& delta_result = vm_result.delta();
    auto& delta_step   = vm_step.delta();

    int n_delta_result0 = delta_result.size();
    int n_delta_step0   = delta_step.size();

    assert(n_delta_step0 == n_delta_result0);

    auto& unshared_regs = get_scratch_list();

    auto unshare_result = [&](int r)
                              {
                                  if (not has_result1(r))
                                      return;
                                  if (prog_unshare[r].none())
                                      unshared_regs.push_back(r);
                                  prog_unshare[r].set(unshare_result_bit);
                              };

    auto unshare_step = [&](int r)
                            {
                                if (not has_result1(r))
                                    return;
                                if (prog_unshare[r].none())
                                    unshared_regs.push_back(r);
                                prog_unshare[r].set(unshare_result_bit);
                                prog_unshare[r].set(unshare_step_bit);
                            };

    // 1. Mark unshared regs.  All modified regs should have STEP/RESULT/FORCE unshared.
    for(auto [r,_]: delta_step)
        unshare_step(r);

#ifndef NDEBUG
    check_created_regs_unshared(t);
#endif

    // 2. Scan regs with different result in t that are used/called by root steps/results
    for(int i=0;i<unshared_regs.size();i++)
    {
        auto& R = regs[unshared_regs[i]];

        // Look at steps that CALL the reg in the root (that has overridden result in t)
        for(int s2: R.called_by)
            if (int r2 = steps[s2].source_reg; prog_steps[r2] == s2)
                unshare_result(r2);

        // Look at steps that USE the reg in the root (that has overridden result in t)
        for(auto& [r2,_]: R.used_by)
            if (prog_steps[r2] > 0)
                unshare_step(r2);
    }

    // 3. Remove (r,-) entries from Delta, and remove the unshare bit for the (r,>0) remainder.
    auto keep = [](const pair<int,int>& p) {return p.second > 0;};

    filter_unordered_vector(delta_step, keep);
    for(auto& [r,_]: delta_step)
        prog_unshare[r].reset(unshare_step_bit);

    filter_unordered_vector(delta_result, keep);
    for(auto& [r,_]: delta_result)
        prog_unshare[r].reset(unshare_result_bit);

    // 4. Reroot to token t.
    tokens[t].flags.set(0);
    tokens[root_token].flags.set(0);

    reroot_at_token(t);
    assert(tokens[t].children.size() == 1);
    int t2 = tokens[t].children[0];

    tokens[t].flags.reset(0);
    tokens[t2].flags.reset(0);

    // 5. Determine which invalid regs we can safely execute.
    auto& zero_count_regs = get_scratch_list();
    auto& vm_count2 = tokens[t2].vm_force_count;

    auto dec_force_count = [&](int r)
    {
        if (not prog_unshare[r].test(unshare_count_bit))
        {
            prog_unshare[r].set(unshare_count_bit);
            vm_count2.add_value(r, prog_force_counts[r]);
        }

        prog_force_counts[r]--;
        assert(prog_force_counts[r] >= 0);

        assert(has_step1(r));
        if (prog_force_counts[r] == 0)
            zero_count_regs.push_back(r);
    };

    // 5a. Decrement counts for calls of invalid steps.
    int n_invalid_control_flow = 0;
    for(int r : unshared_regs)
    {
        // At this point, unshare_step bit only picks out regs that are really -1,
        // and so excludes e.g. the calls from modifiables.
        if (prog_unshare[r].test(unshare_step_bit))
        {
            int s = prog_steps[r];
            int r2 = steps[s].call;
            if (reg_is_changeable(r2))
            {
                n_invalid_control_flow++;
                dec_force_count(r2);
            }
        }
    }

    if (n_invalid_control_flow > 0)
        total_invalid_control_flow++;
    else
        total_valid_control_flow++;

    std::cerr<<"reroot: n_invalid_control_flow = "<<n_invalid_control_flow<<"\n";

    // 5b. Iteratively decrement counts from steps of zero_count regs.
    for(int i=0;i<zero_count_regs.size();i++)
    {
        int r = zero_count_regs[i];
        for(auto [r2,_]: regs[r].used_regs)
            dec_force_count(r2);
        for(auto [r2,_]: regs[r].forced_regs)
            dec_force_count(r2);

        // If unshare_step_bit is set, then we've already decremented any force_count!
        if (not prog_unshare[r].test(unshare_step_bit))
        {
            int s = prog_steps[r];
            int call = steps[s].call;
            if (reg_is_changeable(call))
                dec_force_count(call);
        }
    }

    std::cerr<<"reroot: n_zero_count = "<<zero_count_regs.size()<<"\n";

    // 6.  Evaluate the program
    for(int r: unshared_regs | views::reverse)
        if (prog_force_counts[r] > 0 and prog_unshare[r].test(unshare_result_bit))
            incremental_evaluate2(r);

    auto result = lazy_evaluate2(heads[*program_result_head]).exp;

    // 7. Restore force_counts from previous program
    for(auto [r,count]: tokens[t2].vm_force_count.delta())
    {
        prog_force_counts[r] = count;
        prog_unshare[r].reset(unshare_count_bit);
    }
    tokens[t2].vm_force_count.delta().clear();
    release_scratch_list(); // zero_count_regs

    // 8. Flush pending unshares and clear unsharing bits.
    auto& vm_result2 = tokens[t2].vm_result;
    auto& vm_step2   = tokens[t2].vm_step;
    for(int r: unshared_regs)
    {
        if (prog_unshare[r].test(unshare_result_bit))
        {
            vm_result2.add_value(r, prog_results[r]);
            prog_results[r] = non_computed_index;
        }
        if (prog_unshare[r].test(unshare_step_bit))
        {
            vm_step2.add_value(r, prog_steps[r]);
            prog_steps[r] = non_computed_index;
        }
        prog_unshare[r].reset();
    }

    // 9. Handle moving steps out of the root.
    for(auto [_,s]: vm_step2.delta())
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

    release_scratch_list(); // unshared_regs

    total_results_invalidated += (vm_result.delta().size() - n_delta_result0);
    total_steps_invalidated += (vm_step.delta().size() - n_delta_step0);

    total_results_scanned += vm_result.delta().size();
    total_steps_scanned += vm_step.delta().size();

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


