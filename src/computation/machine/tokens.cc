#include <iostream>
#include "graph_register.H"
#include "util/range.H" // for remove_element( )
#include "range/v3/all.hpp"
#include "util/assert.hh"
#include "util/set.H"

namespace views = ranges::views;

using std::vector;
using std::pair;
using std::optional;

using std::cerr;
using std::endl;

std::vector<int> reg_heap::Token::neighbors() const
{
    auto nodes = children;
    if (not is_root())
	nodes.push_back(*parent);
    return nodes;
}

std::vector<int> reg_heap::younger_neighbors(int t) const
{
    vector<int> nodes;
    if (not is_root_token(t) and token_younger_than(parent_token(t), t))
	nodes.push_back(parent_token(t));

    for(int t2: children_of_token(t))
	if (token_younger_than(t2,t))
	    nodes.push_back(t2);

    return nodes;
}

std::optional<int> reg_heap::older_parent(int t) const
{
    assert(tokens[t].used);

    if (not is_root_token(t) and token_older_than(parent_token(t), t))
	return parent_token(t);

    return {};
}

std::optional<int> reg_heap::older_child(int t) const
{
    assert(tokens[t].used);
    for(int t2: children_of_token(t))
	if (token_older_than(t2,t))
	    return t2;

    return {};
}

std::optional<int> reg_heap::execution_neighbor(int t) const
{
    auto is_execution_neighbor = [&](int t2) {return token_younger_than(t2,t) and undirected_token_type(t2) == token_type::execute;};

    if (not is_root_token(t) and is_execution_neighbor(parent_token(t)))
	return parent_token(t);

    for(int t2: children_of_token(t))
	if (is_execution_neighbor(t2))
	    return t2;

    return {};
}

std::optional<int> reg_heap::older_neighbor(int t) const
{
    assert(tokens[t].used);

    if (auto t2 = older_parent(t))
	return t2;

    if (auto t2 = older_child(t))
	return t2;

    return {};
}

token_type reg_heap::undirected_token_type(int t) const
{
    return tokens[t].utype;
}

token_type reg_heap::directed_token_type(int t) const
{
    token_type type;
    if (is_root_token(t))
	type = token_type::root;
    else if (token_older_than(parent_token(t),t))
	type = tokens[t].utype;
    else
    {
	assert(token_younger_than(parent_token(t),t));
	type = reverse(tokens[parent_token(t)].utype);
    }

    return type;
}

int reg_heap::revert_token(int t) const
{
    assert(tokens[t].used);
    while(undirected_token_type(t) == token_type::execute)
    {
	// The oldest token should not be an execute token,
	// so this token should HAVE an older neighbor.
	// So its safe to access the value here.
	t = older_neighbor(t).value();
    }
    return t;
}

std::vector<int> reg_heap::equivalent_tokens(int t) const
{
    int tr = revert_token(t);
    std::vector<int> equiv_tokens = {tr};
    for(int i=0;i<equiv_tokens.size();i++)
    {
	int t2 = equiv_tokens[i];

	if (auto t3 = execution_neighbor(t2))
	    equiv_tokens.push_back(*t3);
    }
    return equiv_tokens;
}

std::vector<int> reg_heap::equivalent_contexts(int c) const
{
    std::vector<int> cs;
    for(int t: equivalent_tokens(token_for_context(c)))
	for(int c: tokens[t].context_refs)
	    cs.push_back(c);
    return cs;
}

long total_destroy_token = 0;
long total_release_knuckle = 0;

token_type reverse(token_type type)
{
    switch(type)
    {
    case token_type::none:                std::abort();
    case token_type::root:                std::abort();

    case token_type::set:                 return token_type::reverse_set;
    case token_type::set_unshare:         return token_type::reverse_set_unshare;
    case token_type::execute:             return token_type::reverse_execute;
    case token_type::execute2:            return token_type::reverse_execute2;

    case token_type::reverse_set:         return token_type::set;
    case token_type::reverse_set_unshare: return token_type::set_unshare;
    case token_type::reverse_execute:     return token_type::execute;
    case token_type::reverse_execute2:    return token_type::execute2;
    }
    std::abort();
}

std::ostream& operator<<(std::ostream& o, token_type type)
{
    switch(type)
    {
    case token_type::none:                o<<"none";break;
    case token_type::root:                o<<"root";break;

    case token_type::set:                 o<<"set";break;
    case token_type::set_unshare:         o<<"set_unshare";break;
    case token_type::execute:             o<<"execute";break;
    case token_type::execute2:            o<<"execute2";break;

    case token_type::reverse_set:         o<<"reverse_set";break;
    case token_type::reverse_set_unshare: o<<"reverse_set_unshare";break;
    case token_type::reverse_execute:     o<<"reverse_execute";break;
    case token_type::reverse_execute2:    o<<"reverse_execute2";break;
    default:                              std::abort();
    }
    return o;
}


void reg_heap::destroy_step_and_created_regs(int s)
{
    assert(s > 0);
    assert(not steps[s].has_pending_effect_registration());
    assert(not steps[s].has_pending_effect_unregistration());

    for(int r: steps[s].created_regs)
    {
        // Clearing the reg deallocates RAM used by (For example) cached conditional likelihoods.

        // This clears used_by edges pointing to r.  It does not clear used_reg edges pointing to r.
        clear_back_edges_for_reg(r,false);    // We don't need to adjust steps[s].created_regs, since we will destroy steps[s].

	// Unforgettable regs include modifiables with 1 argument.
	// Unlike normal steps, unforgettable steps are backward-shared (see machine.lyx).
	// So they have a step in the root, not the tip token being destroyed.
        if (reg_is_unforgettable(r))
        {
            assert(has_step1(r));
            int s2 = step_index_for_reg(r);
            destroy_step_and_created_regs(s2);

	    // Most steps that we destroy are in a tip token, not in the root.
	    // For the last step for an unforgettable reg will be in the root, not in the tip token.
	    // Therefore we need to unmap it from the root.
	    // See also reg_heap::clear_step( ).  It appears to be completely unused, though.
	    prog_steps[r] = non_computed_index;
	    assert(not has_step1(r));
        }
        reclaim_used(r);                      // This clears the reg.
    }
    clear_back_edges_for_step(s);             // This clears steps[s].created_regs.
    steps.reclaim_used(s);                    // This clears the step
}

void reg_heap::destroy_all_computations_in_token(int t)
{
    auto& delta_step = tokens[t].delta_step();

    // Remove use back-edges
    for(auto& [_,s]: delta_step)
    {
        // Unsharing created regs in unshare_regs( ) ensures that any steps for created regs must be in this token.
        // This allows us to reclaim allocated regs here instead of just waiting for GC to eliminate them.

        if (s > 0)
            destroy_step_and_created_regs(s);
    }

    tokens[t].vm_step.clear();

    tokens[t].vm_result.clear();

    tokens[t].vm_force_count.clear();

    tokens[t].n_modifiables_set = 0;

    tokens[t].interchanges.clear();
}

void reg_heap::release_tip_token(int t)
{
    assert(tokens[t].children.empty());
    assert(not tokens[t].is_referenced());
    assert(tokens[t].used);

    total_destroy_token++;

    // 0. Remove prev_prog edges to and from this token.

    // Clear link to prev prog token.
    // This doesn't remove a reference, since t has no context references.
    assert(tokens[t].context_refs.empty());
    unset_prev_prog_token(t);

    assert(tokens[t].prev_prog_active_refs.empty());

    // Mark inactive references to this token as dead
    for(int t2: tokens[t].prev_prog_inactive_refs)
    {
        assert(tokens[t2].used);
        assert(tokens[t2].prev_prog_token);
        tokens[t2].prev_prog_token->index = {};
    }
    // Mark active references to this token as dead
    for(int t2: tokens[t].prev_prog_active_refs)
    {
        assert(tokens[t2].used);
        assert(tokens[t2].prev_prog_token);
        tokens[t2].prev_prog_token->index = {};
    }
    tokens[t].prev_prog_inactive_refs.clear();
    tokens[t].prev_prog_active_refs.clear();

    // 1. Adjust the token tree
    unused_tokens.push_back(t);
  
    if (not is_root_token(t))
    {
        // mark token for this context unused
        assert(tokens.size() - unused_tokens.size() > 0);

        int index = remove_element(tokens[parent_token(t)].children, t);
        assert(index != -1);
    }
    else
    {
        assert(is_root_token(t));
        root_token = -1;
        steps_pending_effect_registration.clear();
        assert(tokens.size() - unused_tokens.size() == 0);
    }

    // 2. Set the token to unused

    tokens[t].parent = {};
    tokens[t].used = false;
    tokens[t].utype = token_type::none;
    tokens[t].creation_time = -1;

    // 3. Destroy the computations
    try
    {
        destroy_all_computations_in_token(t);
    }
    catch (std::exception& e)
    {
        std::cerr<<"Exception thrown while releasing tip token!\n";
        std::cerr<<e.what()<<"\n";
        std::abort();
    }
    catch (...)
    {
        std::cerr<<"Exception thrown while releasing tip token!\n";
        std::abort();
    }

    // 4. Make sure the token is empty
    assert(tokens[t].vm_step.empty());
    assert(tokens[t].vm_result.empty());
    assert(tokens[t].vm_force_count.empty());
    assert(tokens[t].n_modifiables_set == 0);
    assert(tokens[t].interchanges.empty());

    Token new_token;
    std::swap(tokens[t],new_token);
}

// Given parent -> t1 -> t2 -> XXX, make t2 a child of parent instead of t1.
// If t1 was a knuckle, it will now be a leaf node.
void reg_heap::capture_parent_token(int t2)
{
    int t1 = parent_token(t2);

    int parent = parent_token(t1);

    // disconnect t2 from t1
    tokens[t2].parent = {};
    int index = remove_element(tokens[t1].children, t2);
    assert(index != -1);

    // connect t2 to parent
    tokens[parent].children.push_back(t2);
    tokens[t2].parent = parent;
}

void load_map(const mapping& vm, vector<bitmask_8>& prog_temp)
{
    for(auto [r,_]: vm.delta())
    {
        assert(not prog_temp[r].test(0));
        prog_temp[r].set(0);
    }
}

void unload_map(const mapping& vm, vector<bitmask_8>& prog_temp)
{
    for(auto [r,_]: vm.delta())
    {
        assert(prog_temp[r].test(0));
        prog_temp[r].reset(0);
    }
}

void merge_split_mapping_(mapping& vm1, mapping& vm2, vector<bitmask_8>& prog_temp)
{
    for(int i=0;i<vm1.delta().size();)
    {
        auto [r,v] = vm1.delta()[i];

        if (not prog_temp[r].test(0))
        {
            vm2.add_value(r,v);
            prog_temp[r].set(0);
            vm1.erase_value_at(i);
        }
        else
            i++;
    }
}

// This function handles the composition of deltas: Delta1 o Delta2
//   It is only ever used to remove knuckle tokens (see tokens.cc).
// Given mapping (m1,v1) followed by (m2,v2), compute a combined mapping for (m1,v1)+(m2,v2) -> (m2,v2)
// and a mapping (m1,v1)-(m2,v2)->(m1,v1) for things that now are unused.

// Knuckle tokens is a list of tokens going toward the root.
void reg_heap::merge_split_mappings(const vector<int>& knuckle_tokens)
{
    if (knuckle_tokens.empty()) return;

    // This is the child that will survive, versus the knuckles that will be deleted.
    int child_token = tokens[knuckle_tokens[0]].children[0];

    // 1. Determine if we are merging a sequence of SET tokens.
    bool token_type_all_set = false;
    if (directed_token_type(child_token) == token_type::set)
    {
        token_type_all_set = true;
        for(int t: knuckle_tokens)
            if (directed_token_type(t) != token_type::set)
                token_type_all_set = false;
    }

    // 2. Rename the tokens to type MERGED if they are not all SET.
    if (not token_type_all_set)
    {
	std::abort();
/*
        tokens[child_token].type = token_type::merged;
        for(int t: knuckle_tokens)
        {
            assert(tokens[t].type != token_type::set);
            tokens[t].type = token_type::merged;
        }
*/
    }

    // 3. Merge and split the mappings.
    load_map(tokens[child_token].vm_force_count, prog_temp);
    for(int t: knuckle_tokens)
    {
        assert(token_is_used(t));
        assert(not tokens[t].is_referenced());
        assert(tokens[t].children.size() == 1);

        merge_split_mapping_(tokens[t].vm_force_count, tokens[child_token].vm_force_count, prog_temp);
    }
    unload_map(tokens[child_token].vm_force_count, prog_temp);

    load_map(tokens[child_token].vm_result, prog_temp);
    for(int t: knuckle_tokens)
    {
        assert(token_is_used(t));
        assert(not tokens[t].is_referenced());
        assert(tokens[t].children.size() == 1);

        merge_split_mapping_(tokens[t].vm_result, tokens[child_token].vm_result, prog_temp);
    }
    unload_map(tokens[child_token].vm_result, prog_temp);

    load_map(tokens[child_token].vm_step, prog_temp);
    for(int t: knuckle_tokens)
    {
        assert(token_is_used(t));
        assert(not tokens[t].is_referenced());
        assert(tokens[t].children.size() == 1);

        merge_split_mapping_(tokens[t].vm_step, tokens[child_token].vm_step, prog_temp);
    }
    unload_map(tokens[child_token].vm_step, prog_temp);

    vector<interchange_op> all_interchanges;
    for(int t: knuckle_tokens | views::reverse)
        all_interchanges.insert(all_interchanges.end(), tokens[t].interchanges.begin(), tokens[t].interchanges.end());
    std::swap( tokens[child_token].interchanges, all_interchanges );
}

// Release any knuckle tokens that are BEFORE the child token, and then return the parent of the child token.
int reg_heap::release_knuckle_tokens(int child_token)
{
    int t = child_token;

    vector<int> knuckle_path;

    while (not is_root_token(t))
    {
        t = parent_token(t);
        if (not is_root_token(t) and not tokens[t].is_referenced() and tokens[t].children.size() == 1)
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

    return parent_token(child_token);
}

int reg_heap::release_unreferenced_tips(int t)
{
    assert(t >= 0);
    assert(token_is_used(t));

    while(not tokens[t].is_referenced() and tokens[t].children.empty())
    {
	std::optional<int> parent;

	if (not is_root_token(t))
	    parent = parent_token(t);

        // clear only the mappings that were actually updated here.
        release_tip_token(t);

        if (not parent)
	    break;

	t = *parent;
    }

    return t;
}

bool reg_heap::is_terminal_token(int t) const
{
    assert(token_is_used(t));

    return tokens[t].children.empty();
}

int reg_heap::get_root_token() const
{
    return root_token;
}

bool reg_heap::is_root_token(int t) const
{
    assert(token_is_used(t));
    assert((t==root_token) == (not tokens[t].parent.has_value()));

    return not tokens[t].parent.has_value();
}

int reg_heap::parent_token(int t) const
{
    assert(not is_root_token(t));
    return tokens[t].parent.value();
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

vector<int> reg_heap::get_used_tokens() const
{
    vector<int> toks;
    for(int t=0;t<tokens.size();t++)
        if (token_is_used(t))
            toks.push_back(t);
    return toks;
}


int reg_heap::make_child_token(int t, token_type type)
{
//#ifdef DEBUG_MACHINE
//    check_used_regs();
//#endif

    assert(tokens[t].used);

    int t2 = get_unused_token(type, t);

    // assert(temp.empty());

    // set parent relationship
    tokens[t2].parent = t;
    tokens[t2].children.clear();

    tokens[t].children.push_back(t2);

//#ifdef DEBUG_MACHINE
//    check_used_regs();
//#endif

    return t2;
}

// This is the *safe* interface that should leave things in a valid state when it is done.
void reg_heap::set_token_for_context(int c, optional<int> t2)
{
    // Exit early if the context already points to the token.
    if (t2 and token_for_context(c) == *t2) return;

    // We can't do check_tokens() here because when we create the first token, it will
    // be an unreferenced tip until we finish this routine.

    auto [t1, p1] = unset_token_for_context_no_release_tips_(c);

    if (t2)
        set_token_for_unset_context_(c, *t2);
    else if (t1 != -1)
    {
        // Mark the context as unused
        token_for_context_[c] = -1;
        unused_contexts.push_back(c);
    }

    if (p1)
        release_unreferenced_tips(*p1);

    if (t1 != -1 and tokens[t1].used)
        release_unreferenced_tips(t1);

    check_tokens();
}

void reg_heap::switch_to_context(int c1, int c2)
{
    assert(token_for_context(c2) != -1);
    set_token_for_context(c1, token_for_context(c2));
}

int reg_heap::switch_to_child_token(int c, token_type type)
{
    check_tokens();

    int t1 = token_for_context(c);

    int t2 = make_child_token(t1, type);

    set_token_for_context(c, t2);

    check_tokens();

    return t2;
}

int reg_heap::token_for_context(int c) const
{
    assert(c >= 0);
    return token_for_context_[c];
}

// This could unreference a prev_prog_token.
pair<int,optional<int>> reg_heap::unset_token_for_context_no_release_tips_(int c)
{
    int t = token_for_context(c);

    if (t == -1) return {-1,{}};

    assert(t != -1);
    assert(tokens[t].is_referenced());
    assert(includes(tokens[t].context_refs, c));

    optional<prev_prog_token_t> p;
    optional<int> t2;

    // FIXME: instead of unsetting prev_prog_token, maybe just unset the index.

    // 1. Remove the link from the active list
    if (tokens[t].context_refs.size() == 1)
    {
        p = tokens[t].prev_prog_token;
        t2 = unset_prev_prog_token(t);
    }

    // 2. Decrement the reference count;
    token_for_context_[c] = -1;
    remove_unordered(tokens[t].context_refs, c);

    // 3. Add the link to the inactive list
    if (tokens[t].context_refs.empty())
        set_prev_prog_token(t,p);

    return {t,t2};
}

// The context token must original be unset.
// This cannot unreference a prev_prog_token.
void reg_heap::set_token_for_unset_context_(int c, int t)
{
    // The context token must original be unset.
    assert(token_for_context(c) == -1);

    auto p = tokens[t].prev_prog_token;

    // 1. Remove the link to the inactive list
    if (tokens[t].context_refs.empty())
        unset_prev_prog_token(t);

    // 2. Increment the reference count
    token_for_context_[c] = t;
    tokens[t].context_refs.push_back(c);
    assert(tokens[t].is_referenced());

    // 3. Add the link to the active list
    if (tokens[t].context_refs.size() == 1)
        set_prev_prog_token(t, p);
}

int reg_heap::copy_context(int c)
{
    check_tokens();

    int t = token_for_context(c);
    int c2 = get_new_context();
    set_token_for_unset_context_(c2,t);

    check_tokens();
    return c2;
}

long total_create_context1 = 0;
long total_create_context2 = 0;

int reg_heap::get_new_context()
{
    total_create_context2++;

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

int reg_heap::get_first_context(int r_prog, int r_log)
{
    int c = get_new_context();

    int t = get_unused_token(token_type::root, {});
    tokens[t].utype = token_type::execute2;
    set_prev_prog_token(t, prev_prog_token_t(t,0,true));

    set_token_for_context(c, t);

    check_tokens();

    first_evaluate_program(r_prog, r_log, c);

    return c;
}

void reg_heap::release_context(int c)
{
    set_token_for_context(c, {});
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


// This is only called from two places:
// - get_first_context():            here we DO NOT set the prev_prog_token
// - make_child_token(token, type):  here we DO     set the prov_prog_token
int reg_heap::get_unused_token(token_type type, optional<int> prev_token)
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
    tokens[t].utype = type;
    tokens[t].creation_time = total_tokens_created++;
    assert(total_tokens_created >= 0);

    if (root_token == -1)
    {
        assert(tokens.size() - unused_tokens.size() == 1);
        root_token = t;
    }
    else
        assert(tokens.size() - unused_tokens.size() > 1);

    assert(not tokens[t].parent.has_value());
    assert(tokens[t].children.empty());
    assert(tokens[t].vm_step.empty());
    assert(tokens[t].vm_result.empty());
    assert(tokens[t].interchanges.empty());
    assert(not tokens[t].is_referenced());

    assert(tokens[t].prev_prog_active_refs.empty());
    assert(tokens[t].prev_prog_inactive_refs.empty());
    assert(not tokens[t].prev_prog_token);

    if (prev_token)
    {
        set_prev_prog_token(t, tokens[*prev_token].prev_prog_token);
        if (type == token_type::set)
            tokens[t].prev_prog_token->can_revert = false;
    }

    return t;
}

void reg_heap::set_prev_prog_token(int t, optional<prev_prog_token_t> prev_prog_token)
{
    // The current prev_prog_token should be unset.
    assert(not tokens[t].prev_prog_token);

    // If we aren't trying to set it, quit now.
    if (not prev_prog_token) return;

    int t2 = prev_prog_token->token;

    // If the previous program token is dead, then we don't have to fiddle with any edges.
    if (not prev_prog_token->index)
    {
        tokens[t].prev_prog_token = prev_prog_token;
        return;
    }

    auto& prev_prog_refs = (tokens[t].context_refs.size() > 0)
        ? tokens[t2].prev_prog_active_refs
        : tokens[t2].prev_prog_inactive_refs;

    // Create the backward edge.
    prev_prog_token->index = prev_prog_refs.size();
    prev_prog_refs.push_back(t);

    // Finally create the forward edge.
    tokens[t].prev_prog_token = prev_prog_token;

#ifndef NDEBUG
    int j = *tokens[t].prev_prog_token->index;
    assert(j < prev_prog_refs.size());
    assert(prev_prog_refs[j] == t);

    for(int t3: tokens[t2].prev_prog_active_refs)
        assert(tokens[t3].used);
    for(int t3: tokens[t2].prev_prog_inactive_refs)
        assert(tokens[t3].used);
#endif
}

optional<int> reg_heap::unset_prev_prog_token(int t)
{
    assert(tokens[t].used);

    // Already unset
    if (not tokens[t].prev_prog_token) return {};

    // Set but dead.
    if (not tokens[t].prev_prog_token->index)
    {
        tokens[t].prev_prog_token = {};
        return {};
    }

    // Get the prev prog token and the index for t in its list
    auto t2 = tokens[t].prev_prog_token->token;
    assert(tokens[t2].used);
    auto j  = *tokens[t].prev_prog_token->index;

    auto& prev_prog_refs = (tokens[t].context_refs.size() > 0)
        ? tokens[t2].prev_prog_active_refs
        : tokens[t2].prev_prog_inactive_refs;

    // Check that the the back-edge we are adjust correctly points to token t.
    assert(j < prev_prog_refs.size());
    assert(prev_prog_refs[j] == t);

    if (j + 1 < prev_prog_refs.size())
    {
        // The token whose entry we are shifting should be used, and have a live back-edge.
        auto t3 = prev_prog_refs.back();
        assert(tokens[t3].used);
        assert(tokens[t3].prev_prog_token);
        assert(tokens[t3].prev_prog_token->index);

        // move the t3 entry over entry j
        prev_prog_refs[j] = prev_prog_refs.back();
        // update the t3 index to j
        tokens[t3].prev_prog_token->index = j;
    }
    prev_prog_refs.pop_back();

    tokens[t].prev_prog_token = {};

#ifndef NDEBUG
    for(int t3: tokens[t2].prev_prog_active_refs)
        assert(tokens[t3].used);
    for(int t3: tokens[t2].prev_prog_inactive_refs)
        assert(tokens[t3].used);
#endif

    return t2;
}

bool reg_heap::token_older_than(int t1, int t2) const
{
    assert(tokens[t1].used);
    assert(tokens[t2].used);

    return tokens[t1].creation_time < tokens[t2].creation_time;
}

bool reg_heap::token_younger_than(int t1, int t2) const
{
    assert(tokens[t1].used);
    assert(tokens[t2].used);

    return tokens[t1].creation_time > tokens[t2].creation_time;
}

optional<int> reg_heap::get_prev_prog_token_for_token(int t) const
{
    if (tokens[t].prev_prog_token)
        return tokens[t].prev_prog_token->token;
    else
        return {};
}

optional<int> reg_heap::get_prev_prog_token_for_context(int c) const
{
    return get_prev_prog_token_for_token(token_for_context(c));
}

bool reg_heap::is_program_execution_token(int t) const
{
    auto t2 = get_prev_prog_token_for_token(t);
    return (t2 and *t2 == t);
}

void reg_heap::check_tokens() const
{
#ifndef NDEBUG
    for(int c=0;c<get_n_contexts();c++)
    {
        int t = token_for_context(c);
        if (t >= 0)
        {
            assert(tokens[t].used);
            assert(tokens[t].is_referenced());
            assert(includes(tokens[t].context_refs, c));

            // Check that forward prev_prog_token edges are right.
            if (tokens[t].prev_prog_token and tokens[t].prev_prog_token->index)
            {
                int t2 = tokens[t].prev_prog_token->token;
                int j = *tokens[t].prev_prog_token->index;
                assert(tokens[t2].used);

                auto& prev_prog_refs = (tokens[t].context_refs.size() > 0)
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
    for(int t=0;t<tokens.size();t++)
    {
	if (not tokens[t].used) continue;

	if (not tokens[t].parent)
	    assert(t == root_token);

	// Only one younger neighbor should be an execute token.
	{
	    int total = 0;
	    for(int t2: younger_neighbors(t))
	    {
		if (undirected_token_type(t2) == token_type::execute)
		    total++;
	    }

	    int total2 = 0;
	    if (not is_root_token(t) and token_younger_than(parent_token(t),t) and undirected_token_type(parent_token(t)) == token_type::execute)
		total2++;

	    for(int t2: tokens[t].children)
		if (token_younger_than(t2,t) and directed_token_type(t2) == token_type::execute)
		    total2++;

	    assert(total == total2);
	    assert(total <= 1);
	}

	// At most 1 neighboring node can be older.
	auto older = [&](int t2){ return token_older_than(t2,t); };
	assert(ranges::count_if(tokens[t].neighbors(), older) <= 1);

	for(int c: tokens[t].context_refs)
	    assert(token_for_context(c) == t);
    }
#endif
}
