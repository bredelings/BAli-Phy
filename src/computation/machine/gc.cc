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
void reg_heap::collect_garbage()
{
    total_gc++;
    total_regs = size();
    total_steps = steps.size();

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
    std::cerr<<"Regs: "<<regs.n_used()<<"/"<<regs.size()<<std::endl;
    check_used_regs();
#endif
}

void do_remap(const reg_heap& M, vector<int>& remap, int r)
{
    if (remap[r]) return;

    const closure& C = M[r];

    if (C.exp and C.exp.is_index_var() and (M.reg_is_unevaluated(r) or M.reg_is_index_var_no_force(r)))
    {
        int r2 = C.reg_for_index_var();

        do_remap(M, remap, r2);

        remap[r] = remap[r2];

        assert(remap[r] != r);
        assert(remap[remap[r]] == remap[r]);
    }
    else
	remap[r] = r;
}

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
    if (not get_n_tokens()) assert(steps.size() == steps.n_null());

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
        for(auto [r,_] : R.forced_regs)
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
    for(auto i = regs.begin();i != regs.end();)
    {
        int r = i.addr();
        i++;
	if (not regs.is_marked(r))
	{
            clear_back_edges_for_reg(r);
            regs.reclaim_used(r);
	}
        else
            regs.unmark(r);
    }

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
