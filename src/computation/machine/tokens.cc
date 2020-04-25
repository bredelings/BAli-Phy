#include <iostream>
#include "graph_register.H"
#include "util/assert.hh"
#include "util/range.H" // for remove_element( )

using std::vector;
using std::pair;

using std::cerr;
using std::endl;

long total_destroy_token = 0;
long total_release_knuckle = 0;

token_type reverse(token_type type)
{
    switch(type)
    {
    case token_type::none:                std::abort();
    case token_type::root:                std::abort();
    case token_type::merged:              return token_type::merged;
    case token_type::set:                 return token_type::reverse_set;
    case token_type::set_unshare:         return token_type::reverse_set_unshare;
    case token_type::execute:             return token_type::reverse_execute;
    case token_type::reverse_set:         return token_type::set;
    case token_type::reverse_set_unshare: return token_type::set_unshare;
    case token_type::reverse_execute:     return token_type::execute;
    case token_type::unmap:               return token_type::reverse_unmap;
    case token_type::reverse_unmap:       return token_type::unmap;
    }
    std::abort();
}

std::ostream& operator<<(std::ostream& o, token_type type)
{
    switch(type)
    {
    case token_type::none:                o<<"none";break;
    case token_type::root:                o<<"root";break;
    case token_type::merged:              o<<"merged";break;
    case token_type::set:                 o<<"set";break;
    case token_type::set_unshare:         o<<"set_unshare";break;
    case token_type::execute:             o<<"execute";break;
    case token_type::reverse_set:         o<<"reverse_set";break;
    case token_type::reverse_set_unshare: o<<"reverse_set_unshare";break;
    case token_type::reverse_execute:     o<<"reverse_execute";break;
    case token_type::unmap:               o<<"unmap";break;
    case token_type::reverse_unmap:       o<<"reverse_unmap";break;
    default:                              std::abort();
    }
    return o;
}


void reg_heap::destroy_all_computations_in_token(int t)
{
    auto& delta_step = tokens[t].delta_step();

    // Remove use back-edges
    for(auto& [_,s]: delta_step)
    {
        if (s > 0)
        {
            assert(not steps[s].has_pending_effect_registration());
            assert(not steps[s].has_pending_effect_unregistration());

            for(int r: steps[s].created_regs)
            {
                // Unsharing created regs in unshare_regs( ) ensures that any steps for created regs must be in this token.
                // This allows us to reclaim allocated regs here instead of just waiting for GC to eliminate them.

                // Clearing the reg deallocates RAM used by (For example) cached conditional likelihoods.

                clear_back_edges_for_reg(r,false);    // We don't need to adjust steps[s].created_regs, since we will destroy steps[s].
                reclaim_used(r);                      // This clears the reg.
            }
            clear_back_edges_for_step(s);             // This clears steps[s].created_regs.
            steps.reclaim_used(s);                    // This clears the step
        }
    }

    tokens[t].vm_step.clear();

    tokens[t].vm_result.clear();

    tokens[t].vm_force.clear();
}

void reg_heap::release_tip_token(int t)
{
    assert(tokens[t].children.empty());
    assert(not tokens[t].is_referenced());
    assert(tokens[t].used);

    total_destroy_token++;

    // 1. Adjust the token tree
    int parent = parent_token(t);

    unused_tokens.push_back(t);
  
    if (parent != -1)
    {
        // mark token for this context unused
        assert(not is_root_token(t));
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

    tokens[t].parent = -1;
    tokens[t].used = false;
    tokens[t].type = token_type::none;

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
    assert(tokens[t].vm_force.empty());
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

template <typename T>
void load_map(const mapping<T>& vm, vector<std::bitset<8>>& prog_temp)
{
    for(auto [r,_]: vm.delta())
    {
        assert(not prog_temp[r].test(0));
        prog_temp[r].set(0);
    }
}

template<typename T>
void unload_map(const mapping<T>& vm, vector<std::bitset<8>>& prog_temp)
{
    for(auto [r,_]: vm.delta())
    {
        assert(prog_temp[r].test(0));
        prog_temp[r].reset(0);
    }
}

template <typename T>
void merge_split_mapping_(mapping<T>& vm1, mapping<T>& vm2, vector<std::bitset<8>>& prog_temp)
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
void reg_heap::merge_split_mappings(const vector<int>& knuckle_tokens)
{
    if (knuckle_tokens.empty()) return;

    // This is the child that will survive, versus the knuckles that will be deleted.
    int child_token = tokens[knuckle_tokens[0]].children[0];

    tokens[child_token].type = token_type::merged;
    for(int t: knuckle_tokens)
    {
        assert(tokens[t].type != token_type::set);
        tokens[t].type = token_type::merged;
    }

    load_map(tokens[child_token].vm_force, prog_temp);
    for(int t: knuckle_tokens)
    {
        assert(token_is_used(t));
        assert(not tokens[t].is_referenced());
        assert(tokens[t].children.size() == 1);

        merge_split_mapping_(tokens[t].vm_force, tokens[child_token].vm_force, prog_temp);
    }
    unload_map(tokens[child_token].vm_force, prog_temp);

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

int reg_heap::get_root_token() const
{
    return root_token;
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
#ifdef DEBUG_MACHINE
    check_used_regs();
#endif

    assert(tokens[t].used);

    int t2 = get_unused_token(type);

    // assert(temp.empty());

    // set parent relationship
    tokens[t2].parent = t;
    tokens[t2].children.clear();

    tokens[t].children.push_back(t2);

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif

    return t2;
}

void reg_heap::switch_to_token(int c, int t2)
{
    int t1 = unset_token_for_context(c);
    set_token_for_unset_context_(c,t2);

    release_unreferenced_tips(t1);
}

void reg_heap::switch_to_context(int c1, int c2)
{
    check_tokens();

    switch_to_token(c1, token_for_context(c2));

    check_tokens();
}

int reg_heap::switch_to_child_token(int c, token_type type)
{
    check_tokens();

    int t1 = token_for_context(c);
    int t2 = make_child_token(t1, type);
    switch_to_token(c, t2);

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

void reg_heap::set_token_for_unset_context_(int c, int t)
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

int reg_heap::get_first_context()
{
    int c = get_new_context();
  
    set_token_for_unset_context_(c, get_unused_token(token_type::root));

    check_tokens();

    evaluate_program(c);

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

int reg_heap::get_unused_token(token_type type)
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
    tokens[t].type = type;

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
    assert(tokens[t].vm_force.empty());
    assert(not tokens[t].is_referenced());

    return t;
}

