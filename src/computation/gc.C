#include "graph_register.H"

using std::vector;


template<typename T>
void shrink(vector<T>& v)
{
  if (v.capacity() < 4*(v.size()+1)) return;
  vector<T> v2 = v;
  v.swap(v2);
}

void reg_heap::collect_garbage()
{
  // Make sure weak references to anything freed here are invalidated.
  computations.inc_version();
  inc_version();

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

  if (not C.exp or C.exp->head->type() != index_var_type)
  {
    remap[r] = r;
    return;
  }

  int index = assert_is_a<index_var>(C.exp)->index;

  int r2 = C.lookup_in_env( index );

  do_remap(M, remap, r2);

  remap[r] = remap[r2];

  assert(remap[r] != r);
  assert(remap[remap[r]] == remap[r]);
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

  vector<int>& scan1 = get_scratch_list();
  vector<int>& next_scan1 = get_scratch_list();
  vector<int>& scan2 = get_scratch_list();
  vector<int>& next_scan2 = get_scratch_list();

  //  assert(root_token != -1);
  //  tokens.push_back(root_token);

  get_roots(scan1);
  
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
      
      // Count all computations
      for(int t=0;t<get_n_tokens();t++)
      {
	if (not token_is_used(t)) continue;
	
	if (not has_computation_(t,r)) continue;

	int rc = computation_index_for_reg_(t,r);
	scan2.push_back(rc);
      }
    }
    std::swap(scan1,next_scan1);
    next_scan1.clear();

    for(int rc: scan2)
    {
      assert(not computations.is_free(rc));
      if (computations.is_marked(rc)) continue;
      
      computations.set_mark(rc);
      
      const computation& RC = computations[rc];
      
      // Count the reg that references us
      assert(RC.source_reg);
      scan1.push_back(RC.source_reg);
      
      // Count also the computation we call
      if (RC.call) 
	scan1.push_back(RC.call);
    }
    std::swap(scan2,next_scan2);
    next_scan2.clear();
  }

  // Avoid memory leaks.
  for(auto& rc: computations)
  {
    clean_weak_refs(rc.used_by, computations);
    shrink(rc.used_by);
    clean_weak_refs(rc.called_by, computations);
    shrink(rc.called_by);
  }

#ifdef DEBUG_MACHINE
  check_used_regs();
#endif
  reclaim_unmarked();
  computations.reclaim_unmarked();
#ifdef DEBUG_MACHINE
  check_used_regs();
#endif

  //  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
  release_scratch_list();
}
