#include "graph_register.H"

using std::vector;


template<typename V>
void shrink(V& v)
{
  if (v.capacity() < 4*(v.size()+1)) return;
  V v2 = v;
  v.swap(v2);
}

void reg_heap::collect_garbage()
{
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

  // Find computations for marked regs
  const auto& m = tokens[t].vm_relative;
  {
    scan2.resize(m.modified().size());
    int i=0;
    for(int r: m.vm_relative.modified())
      if (is_marked(r))
      {
	int rc = m[r];
	if (rc > 0)
	{
	  scan2[i++] = rc;
	  assert(not computations.is_marked(rc));
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
      
      int rc = computation_index_for_reg_(t,r);
      if (rc > 0)
      {
	assert(not computations.is_free(rc));
	if (computations.is_marked(rc)) continue;
	
	computations.set_mark(rc);
	
	const computation& RC = computations[rc];
	
	// Count the reg that references us
	assert(RC.source_reg);
	assert(is_marked(RC.source_reg));
	//      scan1.push_back(RC.source_reg);
	
	// Count also the computation we call
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
  vector<int>& scan1 = get_scratch_list();
  vector<int>& next_scan1 = get_scratch_list();

  get_roots(scan1);
  
  while (not scan1.empty())
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
      
      // Count all computations
      for(int t=0;t<get_n_tokens();t++)
      {
	if (not token_is_used(t)) continue;
	
	int s = step_index_for_reg_(t,r);
	int rc = computation_index_for_reg_(t,r);

	if (s > 0)
	{
	  assert(not steps.is_free(s));
	  if (steps.is_marked(s)) continue;

	  steps.set_mark(s);

	  const Step& S = steps[s];

	  // Count the reg that references us
	  assert(S.source_reg);
	  assert(is_marked(S.source_reg));

	  // Count also the computation we call
	  if (S.call) 
	    next_scan1.push_back(S.call);
	}

	if (rc > 0)
	{
	  assert(not computations.is_free(rc));
	  if (computations.is_marked(rc)) continue;

	  computations.set_mark(rc);
      
	  const computation& RC = computations[rc];
      
	  // Count the reg that references us
	  assert(RC.source_reg);
	  assert(is_marked(RC.source_reg));
	}
      }
    }
    std::swap(scan1,next_scan1);
    next_scan1.clear();
  }

  release_scratch_list();
  release_scratch_list();
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
  reclaim_unmarked();

  // remove all back-edges
  for(auto i = steps.begin();i != steps.end(); i++)
    if (not steps.is_marked(i.addr()))
      clear_back_edges_for_step(i.addr());

  steps.reclaim_unmarked();

  // remove all back-edges
  for(auto i = computations.begin();i != computations.end(); i++)
    if (not computations.is_marked(i.addr()))
      clear_back_edges_for_computation(i.addr());

  computations.reclaim_unmarked();

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
