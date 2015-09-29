#include "graph_register.H"

using std::string;
using std::vector;

// Find regs in t1 that are shared into t2, and depend on regs in t1 that are modified in t2.
void reg_heap::find_callers(int t1, int r1, vector<int>& modified, int mark)
{
  const auto& RC1 = computation_for_reg_(t1,r1);

  // Look at computations in t2 that call the old value in t1.
  for(int rc2: RC1.called_by)
  {
    computation& RC2 = computations[rc2];
    int r2 = RC2.source_reg;

    // All back-edges should be in w/in t1.
    assert(RC2.source_token == t1);

    // Skip this one if its been marked high enough already
    if (RC2.temp >= mark) continue;

    // There shouldn't be a back edge to r2, if r2 has no result.
    // When WOULDN'T there be a back edge?
    assert(RC2.result);
    // If the computation has no result, then its called-by edge is out-of-date
    //      if (not RC2.result) continue;

    assert(computation_index_for_reg_(t1,r2) == rc2);

    // If the reg is completely unmarked, then its not on the modified list yet.
    if (RC2.temp < 0)
      modified.push_back(r2);

    RC2.temp = mark;
  }
}

// Find regs in t1 that are shared into t2, and depend on regs in t1 that are modified in t2.
void reg_heap::find_users(int t1, int r, vector<int>& modified, int mark)
{
  const auto& RC1 = computation_for_reg_(t1, r);

  // Look at computations in t2 that call the old value in t1.
  for(int rc2: RC1.used_by)
  {
    computation& RC2 = computations[rc2];
    int r2 = RC2.source_reg;

    // All back-edges should be in w/in t1.
    assert(RC2.source_token == t1);

    // Skip this one if its been marked high enough already
    if (RC2.temp >= mark) continue;

    assert(not is_modifiable(access(r2).C.exp));

    // r2 need not necessarily have a result -- there's just a use edge from r2->r1, not a call edge.

    assert(computation_index_for_reg_(t1,r2) == rc2);

    if (RC2.temp < 0)
      modified.push_back(r2);

    RC2.temp = mark;
  }
}

// Find regs in t1 that are shared into t2, and depend on regs in t1 that are modified in t2.
void reg_heap::find_shared_callers(int t1, int t2, int r1, vector<int>& modified, int mark)
{
  const auto& RC1 = computation_for_reg_(t1,r1);

  // Look at computations in t2 that call the old value in t1.
  for(int rc2: RC1.called_by)
  {
    computation& RC2 = computations[rc2];
    int r2 = RC2.source_reg;

    // All back-edges should be in w/in t1.
    assert(RC2.source_token == t1);

    // Skip this one if its been marked high enough already
    if (RC2.temp >= mark) continue;

    // If this computation is not shared into t2, then we don't need to unshare it.
    if (computation_index_for_reg_(t2,r2)) continue;

    // There shouldn't be a back edge to r2, if r2 has no result.
    // When WOULDN'T there be a back edge?
    assert(RC2.result);
    // If the computation has no result, then its called-by edge is out-of-date
    //      if (not RC2.result) continue;

    assert(computation_index_for_reg_(t1,r2) == rc2);

    // If the reg is completely unmarked, then its not on the modified list yet.
    if (RC2.temp < 0)
      modified.push_back(r2);

    RC2.temp = mark;
  }
}

// Find regs in t1 that are shared into t2, and depend on regs in t1 that are modified in t2.
void reg_heap::find_shared_users(int t1, int t2, int r, vector<int>& modified, int mark)
{
  const auto& RC1 = computation_for_reg_(t1, r);

  // Look at computations in t2 that call the old value in t1.
  for(int rc2: RC1.used_by)
  {
    computation& RC2 = computations[rc2];
    int r2 = RC2.source_reg;

    // All back-edges should be in w/in t1.
    assert(RC2.source_token == t1);

    // Skip this one if its been marked high enough already
    if (RC2.temp >= mark) continue;

    // If this computation is not shared into t2, then we don't need to unshare it.
    if (computation_index_for_reg_(t2,r2)) continue;

    assert(not is_modifiable(access(r2).C.exp));

    // r2 need not necessarily have a result -- there's just a use edge from r2->r1, not a call edge.

    assert(computation_index_for_reg_(t1,r2) == rc2);

    if (RC2.temp < 0)
      modified.push_back(r2);

    RC2.temp = mark;
  }
}

void reg_heap::unshare_regs(int t1, int t2)
{
  assert(t1 == parent_token(t2));
  assert(tokens[t1].version >= tokens[t2].version);

  if (tokens[t1].version <= tokens[t2].version) return;

  // find all regs in t1 that are overriden in t2
  vector<int>& modified = get_scratch_list();
  for(int r: tokens[t2].vm_relative.modified())
    if (tokens[t1].vm_relative[r] > 0)
      modified.push_back(r);

  vector<int>& unshared = get_scratch_list();

  // First find all users or callers of regs where the result is out of date.
  for(int r: modified)
  {
    find_shared_callers(t1, t2, r, unshared, mark_result);
    find_shared_users(t1, t2, r, unshared, mark_call_result);
  }

  // Then propagate the unsharedness
  for(int i=0;i<unshared.size();i++)
  {
    int r = unshared[i];
    find_shared_callers(t1, t2, r, unshared, mark_result);
    find_shared_users(t1, t2, r, unshared, mark_call_result);
  }

#ifndef NDEBUG
  for(int r:modified)
  {
    int rc = tokens[t1].vm_relative[r];
    assert(computations[rc].temp == -1);
  }
#endif

  vector< int >& regs_to_re_evaluate = tokens[t2].regs_to_re_evaluate;

  // Unshare the regs that should be unshared
  for(int r: unshared)
  {
    int rc1 = computation_index_for_reg_(t1,r);
    auto& RC = computations[rc1];

    assert(not is_modifiable(access(r).C.exp));
    assert(not tokens[t2].vm_relative[r]);

    if (RC.temp == mark_result)
    {
      // Make a new computation in t2.
      int rc2 = add_shared_computation(t2,r);

      // Copy the computation-step part from t1->t2, but not the result
      duplicate_computation(rc1,rc2);
    }
    else
    {
      assert(RC.temp == mark_call_result);
      clear_computation(t2,r);
    }

    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(r).re_evaluate)
      regs_to_re_evaluate.push_back(r);

    RC.temp = -1;
  }

  release_scratch_list();
  release_scratch_list();
  assert(n_active_scratch_lists == 0);

#ifndef NDEBUG
  // Check that regs shared from t1 into t2 don't used regs that will be overridden
  for(int r1: tokens[t1].vm_relative.modified())
  {
    int rc1 = tokens[t1].vm_relative[r1];

    // Need an actual computation in t1
    if (rc1 < 0) continue;

    // Consider only regs t1 -> t2
    if (tokens[t2].vm_relative[r1]) continue;

    for(const auto& rcp2: computations[rc1].used_inputs)
    {
      int rc2 = rcp2.first;
      int r2 = computations[rc2].source_reg;

      // Edges from t1 -> t1 should have have their target computation in t2.
      if (computations[rc2].source_token == t1)
	assert(not tokens[t2].vm_relative[r2]);
    }

    if (int rc2 = computations[rc1].call_edge.first)
    {
      int r2 = computations[rc2].source_reg;
      assert(r2 == computations[rc1].call);

      // Edges from t1 -> t1 should have have their target computation in t2.
      if (computations[rc2].source_token == t1)
	assert(not tokens[t2].vm_relative[r2]);
    }
  }
#endif
}


// Find all unshared regs in t2 that use an unshared reg in t1
void reg_heap::invalidate_regs(int token, vector<int>& invalid)
{
  // First find all users or callers of regs where the result is out of date.
  for(int i=0; i<invalid.size(); i++)
  {
    find_callers(token, invalid[i], invalid, mark_result);
    find_users(token, invalid[i], invalid, mark_call_result);
  }

  // Remove out-of-date probabilities from total probability
  if (token == root_token)
    for(int r: invalid)
      dec_probability_for_reg(r);

  vector< int >& regs_to_re_evaluate = tokens[token].regs_to_re_evaluate;

  // First pass: clear back-edges to these computations
  for(int R: invalid)
  {
    int rc = computation_index_for_reg_(token,R);
    int mark = computations[rc].temp;

    if (mark == mark_result)
    {
      assert(computations[rc].call_edge.first);
      clear_call_back_edge(rc);
    }
    else if (mark == mark_call_result)
      clear_back_edges(rc);
  }

  // Second pass: clear the computations
  for(int R: invalid)
  {
    assert(has_computation_(token,R));

    // Put this back when we stop making spurious used_by edges
    //    assert(reg_has_call(token,R));

    auto& RC = computation_for_reg_(token,R);
    int mark = RC.temp;
    RC.temp = mark_unmarked;
    
    if (mark == mark_result)
    {
      assert(not is_modifiable(access(R).C.exp));
      assert(RC.result);
      RC.result = 0;
      RC.call_edge = {};
    }
    else if (mark == mark_call_result)
    {
      assert(not is_modifiable(access(R).C.exp));
      clear_computation(token,R);
    }
    else
      assert(mark == mark_modified);
    
    // Mark this reg for re_evaluation if it is flagged and hasn't been seen before.
    if (access(R).re_evaluate)
      regs_to_re_evaluate.push_back(R);
  }
 
}

// Find all unshared regs in t2 that use an unshared reg in t1
void reg_heap::invalidate_cross_regs(int t1, int t2)
{
  assert(t1 == parent_token(t2));
  assert(tokens[t1].version >= tokens[t2].version);

  if (tokens[t1].version <= tokens[t2].version) return;

  vector< int >& invalid = get_scratch_list();

  // Find regs (r1) from t2 that use/call unshared computations (rc2) in t1
  const auto& m2 = tokens[t2].vm_relative;
  for(int r1: m2.modified())
  {
    int rc1 = m2[r1];
    if (rc1 <= 0) continue;

    auto& RC1 = computations[rc1];
    assert(RC1.temp == mark_unmarked);
    
    for(const auto& rcp: RC1.used_inputs)
    {
      int rc2 = rcp.first;
      const auto& RC2 = computations[rc2];
      int r2 = RC2.source_reg;

      if (RC2.source_token == t1 and m2[r2])
      {
	RC1.temp = mark_call_result;
	break;
      }
    }

    if (RC1.temp != mark_call_result and RC1.call_edge.first)
    {
      int rc2 = RC1.call_edge.first;
      const auto& RC2 = computations[rc2];
      int r2 = RC2.source_reg;

      if (RC2.source_token == t1 and m2[r2])
	RC1.temp = mark_result;
    }

    if (RC1.temp != mark_unmarked)
      invalid.push_back(r1);
  }

  invalidate_regs(t2, invalid);

  release_scratch_list();
  assert(n_active_scratch_lists == 0);

#ifndef NDEBUG
  for(int r1: m2.modified())
    if (m2[r1] > 0)
    {
      int rc1 = computation_index_for_reg_(t2,r1);
      auto& RC1 = computations[rc1];
      for(const auto& rcp: RC1.used_inputs)
      {
	int rc2 = rcp.first;
	const auto& RC2 = computations[rc2];
	int r2 = RC2.source_reg;

	if (RC2.source_token == t1)
	  assert(not m2[r2]);
      }

      if (RC1.call_edge.first)
      {
	int rc2 = RC1.call_edge.first;
	const auto& RC2 = computations[rc2];
	int r2 = RC2.source_reg;

	if (RC2.source_token == t1)
	  assert(not m2[r2]);
      }
    }
#endif  
}

