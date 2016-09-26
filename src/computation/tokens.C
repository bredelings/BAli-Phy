#include <iostream>
#include "graph_register.H"

using std::vector;
using std::pair;

using std::cerr;
using std::endl;

int total_destroy_token = 0;
int total_release_knuckle = 0;

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

void reg_heap::release_knuckle_token(int t)
{
    assert(token_is_used(t));
    assert(not tokens[t].is_referenced());
    assert(tokens[t].children.size() == 1);

    int child_token = tokens[t].children[0];

    if (is_root_token(t))
    {
	reroot_at(child_token);
	release_tip_token(t);
	return;
    }

    total_release_knuckle++;
  
    merge_split_mapping(t, child_token);

    capture_parent_token(child_token);

    assert(tokens[t].version <= tokens[child_token].version);

    release_tip_token(t);
}

void reg_heap::release_tip_token_and_ancestors(int t)
{
    assert(token_is_used(t));
    assert(tokens[t].children.empty());
    assert(not tokens[t].is_referenced());

    while(t != -1 and (not tokens[t].is_referenced()) and tokens[t].children.empty())
    {
	int parent = parent_token(t);

	// clear only the mappings that were actually updated here.
	release_tip_token(t);

	t = parent;
    }
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

    tokens[t2].triggers = tokens[t].triggers;

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

    if ((not tokens[t].is_referenced()) and tokens[t].children.size() == 0)
	release_tip_token_and_ancestors(t);

    // Mark the context as unused
    token_for_context_[c] = -1;
    unused_contexts.push_back(c);

    check_tokens();
}

