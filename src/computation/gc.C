#include "graph_register.H"

using std::vector;

template<typename V>
void shrink(V& v)
{
  if (v.capacity() < 4*(v.size()+1)) return;
  V v2 = v;
  v.swap(v2);
}

long total_gc = 0;
long total_regs = 0;
long total_steps = 0;
long total_comps = 0;
void reg_heap::collect_garbage()
{
  total_gc++;
  total_regs = size();
  total_steps = steps.size();
  total_comps = results.size();
#ifdef DEBUG_MACHINE
  std::cerr<<"***********Garbage Collection******************"<<std::endl;
  check_used_regs();
#endif
  assert(size() == n_used() + n_free() + n_null());

  trace_and_reclaim_unreachable();

#ifdef DEBUG_MACHINE
  cerr<<"Regs: "<<n_used()<<"/"<<size()<<endl;
  check_used_regs();
#endif
}

void do_remap(const reg_heap& M, vector<int>& remap, int r)
{
  if (remap[r]) return;

  const closure& C = M.access(r).C;

  if (not C.exp or C.exp.head().type() != index_var_type)
  {
    remap[r] = r;
    return;
  }

  int index = C.exp.as_index_var();

  int r2 = C.lookup_in_env( index );

  do_remap(M, remap, r2);

  remap[r] = remap[r2];

  assert(remap[r] != r);
  assert(remap[remap[r]] == remap[r]);
}

/*
void reg_heap::trace_token(int token, vector<int>& remap)
{
  assert(token_is_used(token));

  vector<int>& scan1 = get_scratch_list();
  vector<int>& next_scan1 = get_scratch_list();
  vector<int>& scan2 = get_scratch_list();
  vector<int>& next_scan2 = get_scratch_list();

  // Find results for marked regs
  const auto& m = tokens[t].vm_result;
  {
    scan2.resize(m.modified().size());
    int i=0;
    for(int r: m.vm_result.modified())
      if (is_marked(r))
      {
	int rc = m[r];
	if (rc > 0)
	{
	  scan2[i++] = rc;
	  assert(not results.is_marked(rc));
	}
      }
    scan2.resize(i);
  }

  while (not scan1.empty() or not scan2.empty())
  {
    for(int r: scan1)
    {
      assert(not is_free(r));
      if (is_marked(r)) continue;
      
      set_mark(r);
      do_remap(*this, remap, r);
      
      reg& R = access(r);
      
      // Count the references from E
      next_scan1.insert(next_scan1.end(), R.C.Env.begin(), R.C.Env.end());
      
      int rc = result_index_for_reg_(t,r);
      if (rc > 0)
      {
	assert(not results.is_free(rc));
	if (results.is_marked(rc)) continue;
	
	results.set_mark(rc);
	
	const result& RC = results[rc];
	
	// Count the reg that references us
	assert(RC.source_reg);
	assert(is_marked(RC.source_reg));
	//      scan1.push_back(RC.source_reg);
	
	// Count also the result we call
	if (RC.call) 
	  scan1.push_back(RC.call);
      }
    }
    std::swap(scan1,next_scan1);
    next_scan1.clear();
  }
}
*/

void reg_heap::trace(vector<int>& remap)
{
  // 1. Set up lists for used/marked regs, steps, and results.
  vector<int>& used_regs = get_scratch_list();
  vector<int>& used_steps = get_scratch_list();
  vector<int>& used_results = get_scratch_list();

  auto mark_reg = [this,&used_regs](int r) {
    assert(r > 0);
    if (not is_marked(r))
    {
      set_mark(r);
      used_regs.push_back(r);
    }
  };

  auto mark_step = [this,&used_steps](int s) {
    assert(s > 0);
    if (not steps.is_marked(s))
    {
      steps.set_mark(s);
      used_steps.push_back(s);
    }
  };

  auto mark_result = [this,&used_results](int r) {
    assert(r > 0);
    if (not results.is_marked(r))
    {
      results.set_mark(r);
      used_results.push_back(r);
    }
  };

  // 2. Get the list of root regs
  vector<int>& roots = get_scratch_list();
  get_roots(roots);


  // 3. Mark all of these regs used
  for(int reg:roots)
    mark_reg(reg);

  // 4. Mark all steps/results at heads in the root token.
  if (get_n_tokens())
  {
    for(int reg:roots)
    {
      int step = step_index_for_reg(reg);
      if (step > 0)
	mark_step(step);
      int result = result_index_for_reg(reg);
      if (result > 0)
	mark_result(result);
    }
  }
  
  // 5. Mark all steps/results at heads in non-root tokens
  for(int t=0;t<get_n_tokens();t++)
  {
    if (not token_is_used(t)) continue;

    if (is_root_token(t)) continue;

    // 5.1 Mark all steps at heads in non-root tokens.
    // FIXME - We can remove this after we maintain references to invalidated computations.
    //         Then there should always be a result for every step, valid or invalid.
    for(auto p: tokens[t].delta_step())
    {
      int r = p.first;
      if (access(r).n_heads)
      {
	assert(is_marked(r));
	int step = p.second;
	if (step > 0)
	  mark_step(step);
      }
    }

    // 5.2 Mark all results at heads in non-root tokens.
    // NOTE - The corresponding head, whatever it is, must ALSO be marked,
    //        since we mark all steps at heads.
    for(const auto& p: tokens[t].delta_result())
    {
      int r = p.first;
      if (access(r).n_heads)
      {
	assert(is_marked(r));
	int result = p.second;
	if (result > 0)
	  mark_result(result);
      }
    }
  }

  // 6. Trace unwalked steps and results
  int step_index = 0, result_index = 0;

  while(step_index < used_steps.size() or result_index < used_results.size())
  {
    // 6.1 Trace unwalked steps
    for(;step_index < used_steps.size();step_index++)
    {
      const auto& S = steps[used_steps[step_index]];
      assert(S.source_reg);

      // 6.1.1 Visit called reg
      if (S.call > 0)
	mark_reg(S.call);

      // 6.1.2 Visit used results
      for(const auto& rc: S.used_inputs)
	mark_result(rc.first);
    }

    // 6.2 Trace unwalked results
    for(;result_index < used_results.size();result_index++)
    {
      const auto& R = results[used_results[result_index]];
      assert(R.source_reg);

      // 6.2.1 Visit associated step
      mark_step(R.source_step);

      // 6.2.2 Visit called_result
      if (R.call_edge.first > 0)
	mark_result(R.call_edge.first);
    }
  }

  // 7. Mark all regs with steps or results as used.
  for(int s:used_steps)
    mark_reg(steps[s].source_reg);

  for(int r:used_results)
  {
    const auto& result = results[r];
    assert(steps.is_marked(result.source_step));
    assert(result.source_reg == steps[result.source_step].source_reg);
    assert(is_marked(results[r].source_reg));
  }

  // 8. Mark regs referenced only by regs as used.
  for(int reg_index = 0;reg_index < used_regs.size();reg_index++)
  {
    int r = used_regs[reg_index];
    do_remap(*this, remap, r);
    const auto& R = access(r);
    for(int r : R.C.Env)
      mark_reg(r);
  }

  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
}

template <typename Obj>
void unmap_unused(mapping& vm, pool<Obj>& Objs, pool<reg>& regs)
{
  for(int i=0;i < vm.modified().size();)
  {
    int reg = vm.modified()[i];
    int obj = vm[reg];
    assert(obj != 0);
    // if there's a step mapped that is going to be destroyed, then remove the mapping.
    if (obj > 0 and not Objs.is_marked(obj))
      vm.erase_value(reg);
    // if the reg is going to be destroy, and the step is unshared, remove the unsharing mark.
    else if (obj < 0 and not regs.is_marked(reg))
      vm.erase_value(reg);
    else
    {
      // if there's a step mapped and its not going to be destroyed, then we should know that the reg is used.
      if (obj > 0) assert(regs.is_marked(reg));
      // advance to the next modified reg, if the previous one 
      i++;
    }
  }
}

void reg_heap::trace_and_reclaim_unreachable()
{
#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  //  vector<int>& tokens = get_scratch_list();

  vector<int>& remap = get_scratch_list();
  remap.resize(size());
  for(int i=0;i<remap.size();i++)
    remap[i] = 0;

  trace(remap);

#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  // unmap all the unused results
  for(int t=0; t < get_n_tokens(); t++)
    if (token_is_used(t))
    {
      unmap_unused(tokens[t].vm_step, steps, *this);
      unmap_unused(tokens[t].vm_result, results, *this);
    }

  // remove all back-edges
  for(auto i = steps.begin();i != steps.end(); i++)
    if (not steps.is_marked(i.addr()))
      clear_back_edges_for_step(i.addr());

  for(auto i = begin();i != end(); i++)
    if (not is_marked(i.addr()))
      clear_back_edges_for_reg(i.addr());

  reclaim_unmarked();

  steps.reclaim_unmarked();

  // remove all back-edges
  for(auto i = results.begin();i != results.end(); i++)
    if (not results.is_marked(i.addr()))
      clear_back_edges_for_result(i.addr());

  // check that no freed computations are mapped?
  
  results.reclaim_unmarked();

#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  // remap closures not to point through index_vars
  for(reg& R: *this)
    for(int& r2: R.C.Env)
    {
      assert(is_used(r2));
      r2 = remap[r2];
      assert(is_used(r2));
    }

  //  release_scratch_list();
  release_scratch_list();
}
