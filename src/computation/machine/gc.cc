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

    // Avoid memory leaks.  But can we do this faster?
    {
	vector<Token> new_tokens = tokens;
	std::swap(new_tokens, tokens);
    }
#ifdef DEBUG_MACHINE
    std::cerr<<"***********Garbage Collection******************"<<std::endl;
    check_used_regs();
#endif
    assert(regs.size() == regs.n_used() + regs.n_free() + regs.n_null());

    trace_and_reclaim_unreachable();

#ifdef DEBUG_MACHINE
    std::cerr<<"Regs: "<<n_used()<<"/"<<size()<<std::endl;
    check_used_regs();
#endif
}

void do_remap(const reg_heap& M, vector<int>& remap, int r)
{
    if (remap[r]) return;

    const closure& C = M[r];

    if (not C.exp or C.exp.head().type() != index_var_type)
    {
	remap[r] = r;
	return;
    }

    int r2 = C.reg_for_index_var();

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
      
  reg& R = regs.access(r);
      
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

    auto mark_reg = [this,&used_regs](int r) {
	assert(r > 0);
	if (not regs.is_marked(r))
	{
	    regs.set_mark(r);
	    used_regs.push_back(r);
	}
    };

    // 2. Get the list of root regs
    vector<int>& roots = get_scratch_list();
    get_roots(roots);

    // 3. Mark all of these regs used
    for(int r:roots)
	mark_reg(r);

    // There shouldn't be any steps if there are no tokens.
    if (not get_n_tokens()) assert(steps.size() == 0);

    // 4. Mark all regs with steps
    for(auto& S: steps)
    {
	mark_reg(S.source_reg);

	if (S.call > 0)
	    mark_reg(S.call);
    }

    // 5. Mark regs referenced only by regs as used.
    for(int reg_index = 0;reg_index < used_regs.size();reg_index++)
    {
	int r = used_regs[reg_index];
	do_remap(*this, remap, r);
	const auto& R = regs.access(r);
	for(int r : R.C.Env)
	    mark_reg(r);
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

    // Would it be faster to register a clearing callback?
    for(auto i = regs.begin();i != regs.end(); i++)
	if (not regs.is_marked(i.addr()))
	{
	    clear_back_edges_for_reg(i.addr());
	    regs.access(i.addr()).clear();
	}

    regs.reclaim_unmarked();

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif

    // remap closures not to point through index_vars
    for(reg& R: regs)
	for(int& r2: R.C.Env)
	{
	    assert(regs.is_used(r2));
	    r2 = remap[r2];
	    assert(regs.is_used(r2));
	}

    for(auto& C: closure_stack)
	for(int& r2: C.Env)
	{
	    assert(regs.is_used(r2));
	    r2 = remap[r2];
	    assert(regs.is_used(r2));
	}

    //  release_scratch_list();
    release_scratch_list();
}
