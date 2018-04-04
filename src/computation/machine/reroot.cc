#include <iostream>
#include "graph_register.H"
#include <algorithm>
#include "util.H"

using std::string;
using std::vector;
using std::pair;

using std::cerr;
using std::endl;

long total_steps_pivoted = 0;
long total_results_pivoted = 0;
long total_reroot = 0;
long total_reroot_one = 0;
long total_invalidate = 0;
long total_steps_invalidated = 0;
long total_results_invalidated = 0;
long total_steps_scanned = 0;
long total_results_scanned = 0;

void merge_split_mapping_(mapping& vm1, mapping& vm2, vector<char>& prog_temp)
{
    for(auto p: vm2.delta())
    {
	int r = p.first;
	prog_temp[r] = 1;
    }

    for(int i=0;i<vm1.delta().size();)
    {
	int r = vm1.delta()[i].first;
	int v = vm1.delta()[i].second;

	if (not prog_temp[r])
	{
	    vm2.add_value(r,v);
	    vm1.erase_value_at(i);
	}
	else
	    i++;
    }

    for(auto p: vm2.delta())
    {
	int r = p.first;
	prog_temp[r] = 0;
    }
}

// This function splits handles the composition of deltas: Delta1 o Delta2
//   It is only ever used to remove knuckle tokens (see tokens.cc).
// Given mapping (m1,v1) followed by (m2,v2), compute a combined mapping for (m1,v1)+(m2,v2) -> (m2,v2)
// and a mapping (m1,v1)-(m2,v2)->(m1,v1) for things that now are unused.
void reg_heap::merge_split_mapping(int t1, int t2)
{
    // The second token need to the a child of the first token
    assert(tokens[t2].parent == t1);
    // The child token (t2) needs to be up-to-date with respect to the parent token.
    assert(tokens[t1].version <= tokens[t2].version);

    merge_split_mapping_(tokens[t1].vm_step, tokens[t2].vm_step, prog_temp);
    merge_split_mapping_(tokens[t1].vm_result, tokens[t2].vm_result, prog_temp);
}

// Given a mapping (m1,v1) at the root followed by the relative mapping (m2,v2), construct a new mapping
// where (m2,v2) is at the root and (m1,v1) is relative.
void pivot_mapping(vector<int>& prog1, mapping& vm2)
{
    for(int i=0;i<vm2.delta().size();i++)
    {
	int r = vm2.delta()[i].first;

	int& s1 = prog1[r];
	int& s2 = vm2.delta()[i].second;

	// switch from root/0 => root/-
	if (s1 == 0) s1 = -1;

	// switch root positions
	std::swap(s1,s2);

	// switch from root/0 => root/-
	if (s1 == -1) s1 = 0;
    }
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


    // 4. Clean up old root token if it became an unused tip
    while (path.size() > 1 and not tokens[path.back()].is_referenced() and tokens[path.back()].children.empty())
    {
	release_tip_token(path.back());
	path.pop_back();
    }

    assert(not path.empty());
    assert(path.back() == root_token or tokens[path.back()].is_referenced() or tokens[path.back()].children.size() > 0);

    // 5. Find sequences of knuckles

    // Only remove a knuckle if its child was part of the original path: do not consider the last element of the path as a valid knuckle.
    path.pop_back();

    vector<vector<int>> knuckle_paths;
    for(int i=1;i<path.size();)
    {
	if (not tokens[path[i]].is_referenced() and tokens[path[i]].children.size() == 1)
	{
	    knuckle_paths.push_back({});
	    for(;i<path.size() and not tokens[path[i]].is_referenced() and tokens[path[i]].children.size() == 1;i++)
		knuckle_paths.back().push_back(path[i]);
	}
	else
	    i++;
    }

    // 6. Actually release the knuckles
    for(auto& knuckle_path: knuckle_paths)
	release_knuckle_tokens(knuckle_path);
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
    total_steps_pivoted += tokens[t].delta_step().size();
    total_results_pivoted += tokens[t].delta_result().size();
    pivot_mapping(prog_steps, tokens[t].vm_step);
    std::swap(tokens[parent].vm_step, tokens[t].vm_step);
    pivot_mapping(prog_results, tokens[t].vm_result);
    std::swap(tokens[parent].vm_result, tokens[t].vm_result);

    // 4. Alter the inheritance tree
    tokens[parent].parent = t;
    int index = remove_element(tokens[parent].children, t);
    assert(index != -1);

    tokens[t].parent = -1;
    tokens[t].children.push_back(parent);

    root_token = t;
    assert(is_root_token(t));

    // 5. Remove probabilities for invalidated regs from the current probability

    for(auto p: tokens[parent].delta_result())
    {
	int rc = p.second;  
	if (rc > 0 and results[rc].flags.test(0))
	    dec_probability(rc);
    }

    total_reroot_one++;
  
    assert(tokens[parent].version == tokens[t].version);

    for(int t2: tokens[t].children)
	assert(tokens[t2].version <= tokens[t].version);

    assert(is_root_token(t));
}

void reg_heap::unshare_regs(int t)
{
    // parent_token(t) should be the root.
    assert(is_root_token(parent_token(t)));
    assert(tokens[root_token].version >= tokens[t].version);

    if (tokens[root_token].version <= tokens[t].version) return;

#if DEBUG_MACHINE >= 2
    check_used_regs();
#endif

    total_invalidate++;
  
    auto& vm_result = tokens[t].vm_result;
    auto& vm_step = tokens[t].vm_step;

    // find all regs in t that are not shared from the root
    const auto& delta_result = vm_result.delta();
    const auto& delta_step = vm_step.delta();
  
    int n_delta_result0 = delta_result.size();
    int n_delta_step0 = delta_step.size();
  
    // All the regs with delta_result set have results invalidated in t
    for(const auto& p: delta_result)
    {
	int r = p.first;
	prog_temp[r] |= 1;
    }

    // All the regs with delta_step set have steps (and results) invalidated in t
    for(const auto& p: delta_step)
    {
	int r = p.first;
	prog_temp[r] |= 2;
	assert(prog_temp[r] == 3);
    }

    // Scan regs with different result in t that are used/called by root steps/results
    for(int i=0;i<delta_result.size();i++)
    {
	int r = delta_result[i].first;

//    int result = result_index_for_reg(r);

	if (not has_result(r)) continue;

	const auto& Result = result_for_reg(r);

	// Look at results that call the root's result (that is overridden in t)
	for(int res2: Result.called_by)
	{
	    const auto& Result2 = results[res2];
	    int r2 = Result2.source_reg;

	    // This result is already unshared
	    if (prog_temp[r2] != 0) continue;

	    // The root program's result at r2 is res2, which calls the root program's result at r
	    if (prog_results[r2] == res2)
	    {
		prog_temp[r2] = 1;
		vm_result.add_value(r2,-1);
	    }
	}

	// Look at step that use the root's result (that is overridden in t)
	for(int s2: Result.used_by)
	{
	    auto& S2 = steps[s2];
	    int r2 = S2.source_reg;

	    // This step is already unshared
	    if (prog_temp[r2] == 3) continue;

	    // The root program's step at r2 is s2, which uses the root program's result at r
	    if (prog_steps[r2] == s2)
	    {
		if (prog_temp[r2] == 0)
		    vm_result.add_value(r2,-1);

		prog_temp[r2] = 3;
		vm_step.add_value(r2,-1);
	    }
	}
    }

    // Erase the marks that we made on prog_temp.
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


