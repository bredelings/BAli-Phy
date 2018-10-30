#include <iostream>
#include "graph_register.H"
#include "util/assert.hh"

using std::vector;
using std::pair;

using std::cerr;
using std::endl;

long total_destroy_token = 0;
long total_release_knuckle = 0;

void reg_heap::destroy_all_computations_in_token(int t)
{
    auto& delta_step = tokens[t].delta_step();
    auto& delta_result = tokens[t].delta_result();

    // Remove use back-edges
    for(auto p: delta_step)
    {
	int s = p.second;
	if (s > 0)
	{
	    for(int r: steps[s].created_regs)
	    {
		// Truncating access(r) here deallocates RAM used by (for example) cached conditional likelihoods.
		// Since we now ensure that any steps/result for these regs must be in this token, we can actually
		//   deallocate the regs here instead of just waiting for GC to eliminate them.
		access(r).created_by = {0,{}};
		reclaim_used(r);
	    }
	    steps[s].created_regs.clear();
	    clear_back_edges_for_step(s);
	}
    }

    // Remove call back-edges
    for(auto p: delta_result)
    {
	int rc = p.second;
	if (rc > 0)
	    clear_back_edges_for_result(rc);
    }

    for(auto p: delta_step)
    {
	int s = p.second;
	if (s > 0)
	    steps.reclaim_used(s);
    }
    tokens[t].vm_step.clear();

    for(auto p: delta_result)
    {
	int rc = p.second;
	if (rc > 0)
	    results.reclaim_used(rc);
    }
    tokens[t].vm_result.clear();
}

void reg_heap::release_tip_token(int t)
{
    assert(tokens[t].children.empty());
    assert(not tokens[t].is_referenced());
    assert(tokens[t].used);

    total_destroy_token++;

    // 1. Clear flags of results in the root token before destroying the root token!
    if (is_root_token(t))
	for(auto p: tokens[root_token].delta_result())
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

    // 5. Make sure the token is empty
    assert(tokens[t].vm_step.empty());
    assert(tokens[t].vm_result.empty());
}

// Given parent -> t1 -> t2 -> XXX, make t2 a child of parent instead of t1.
// If t1 was a knuckle, it will now be a leaf node.
void reg_heap::capture_parent_token(int t2)
{
    int t1 = parent_token(t2);
    assert(t1 != -1);

    int parent = parent_token(t1);
    assert(parent != -1);

    // disconnect t2 from t1
    tokens[t2].parent = -2;
    int index = remove_element(tokens[t1].children, t2);
    assert(index != -1);

    // connect t2 to parent
    tokens[parent].children.push_back(t2);
    tokens[t2].parent = parent;
}

void load_map(const mapping& vm, vector<char>& prog_temp)
{
    for(auto p: vm.delta())
    {
	int r = p.first;
	assert(prog_temp[r] == 0);
	prog_temp[r] = 1;
    }
}

void unload_map(const mapping& vm, vector<char>& prog_temp)
{
    for(auto p: vm.delta())
    {
	int r = p.first;
	assert(prog_temp[r] == 1);
	prog_temp[r] = 0;
    }
}

void merge_split_mapping_(mapping& vm1, mapping& vm2, vector<char>& prog_temp)
{
    for(int i=0;i<vm1.delta().size();)
    {
	int r = vm1.delta()[i].first;
	int v = vm1.delta()[i].second;

	if (not prog_temp[r])
	{
	    vm2.add_value(r,v);
	    prog_temp[r] = 1;
	    vm1.erase_value_at(i);
	}
	else
	    i++;
    }
}

// This function splits handles the composition of deltas: Delta1 o Delta2
//   It is only ever used to remove knuckle tokens (see tokens.cc).
// Given mapping (m1,v1) followed by (m2,v2), compute a combined mapping for (m1,v1)+(m2,v2) -> (m2,v2)
// and a mapping (m1,v1)-(m2,v2)->(m1,v1) for things that now are unused.
void reg_heap::merge_split_mappings(const vector<int>& knuckle_tokens)
{
    if (knuckle_tokens.empty()) return;

    int child_token = tokens[knuckle_tokens[0]].children[0];

    load_map(tokens[child_token].vm_result, prog_temp);
    for(int t: knuckle_tokens)
    {
	assert(token_is_used(t));
	assert(not tokens[t].is_referenced());
	assert(tokens[t].children.size() == 1);

	assert(tokens[t].version <= tokens[child_token].version);

	// The child token (t2) needs to be up-to-date with respect to the parent token.
	assert(tokens[t].version <= tokens[child_token].version);

	merge_split_mapping_(tokens[t].vm_result, tokens[child_token].vm_result, prog_temp);
    }
    unload_map(tokens[child_token].vm_result, prog_temp);

    load_map(tokens[child_token].vm_step, prog_temp);
    for(int t: knuckle_tokens)
    {
	assert(token_is_used(t));
	assert(not tokens[t].is_referenced());
	assert(tokens[t].children.size() == 1);

	assert(tokens[t].version <= tokens[child_token].version);

	// The child token (t2) needs to be up-to-date with respect to the parent token.
	assert(tokens[t].version <= tokens[child_token].version);

	merge_split_mapping_(tokens[t].vm_step, tokens[child_token].vm_step, prog_temp);
    }
    unload_map(tokens[child_token].vm_step, prog_temp);
}

// Release any knuckle tokens that are BEFORE the child token, and then return the parent of the child token.
int reg_heap::release_knuckle_tokens(int child_token)
{
    int t = child_token;

    vector<int> knuckle_path;

    while (true)
    {
	t = tokens[t].parent;
	if (t != root_token and not tokens[t].is_referenced() and tokens[t].children.size() == 1)
	    knuckle_path.push_back(t);
	else
	    break;
    }

    merge_split_mappings(knuckle_path);

    for(int t2: knuckle_path)
    {
	capture_parent_token(child_token);

	total_release_knuckle++;

	release_tip_token(t2);
    }

    return tokens[child_token].parent;
}

int reg_heap::release_unreferenced_tips(int t)
{
    assert(token_is_used(t));

    while(t != -1 and not tokens[t].is_referenced() and tokens[t].children.empty())
    {
	int parent = parent_token(t);

	// clear only the mappings that were actually updated here.
	release_tip_token(t);

	t = parent;
    }

    return t;
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

    // set parent relationship
    tokens[t2].parent = t;
    tokens[t2].children.clear();

    tokens[t].children.push_back(t2);

    tokens[t2].version = tokens[t].version;

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

    release_unreferenced_tips(t);

    // Mark the context as unused
    token_for_context_[c] = -1;
    unused_contexts.push_back(c);

    check_tokens();
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
	total_tokens = tokens.size();
    }

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

