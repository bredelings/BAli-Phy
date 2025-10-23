#include "graph_register.H"
#include "gcobject.H"
#include "util/log-level.H"

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

    if (log_verbose >= 4)
        std::cerr<<"***********Garbage Collection******************"<<std::endl;

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif
    assert(regs.size() == regs.n_used() + regs.n_free() + regs.n_null());

    trace_and_reclaim_unreachable();

    if (log_verbose >= 4)
    {
        std::cerr<<"Regs: "<<regs.n_used()<<"/"<<regs.size()<<std::endl;
        std::cerr<<"Steps: "<<steps.n_used()<<"/"<<steps.size()<<std::endl;
        std::cerr<<"n_heads: "<<heads.size()<<std::endl;
    }

#ifdef DEBUG_MACHINE
    check_used_regs();
#endif
}

void do_remap(const reg_heap& M, vector<int>& remap, int r)
{
    if (remap[r]) return;

    const closure& C = M[r];

    // If we are currently evaluating a reg (i.e. its on the stack), it could have state UNEVALUTED but still have forces.
    if (C.exp and C.exp.is_index_var() and
        (not M.reg_is_on_stack(r)) and
        (M.reg_is_unevaluated(r) or M.reg_is_index_var_no_force(r)))
    {
        assert(not M.has_result1(r));
        assert(not M.reg_has_forces(r));

        int r2 = C.reg_for_index_var();

        do_remap(M, remap, r2);

        remap[r] = remap[r2];

        assert(remap[r] != r);
        assert(remap[remap[r]] == remap[r]);
    }
    else
	remap[r] = r;
}

template <typename T>
void insert_at_end(vector<int>& v, const T& t)
{
    v.insert(v.end(), t.begin(), t.end());
}

void reg_heap::get_roots(vector<int>& scan, bool keep_identifiers) const
{
    insert_at_end(scan, stack); // inc_heads = yes
    insert_at_end(scan, temp); // yes
    insert_at_end(scan, heads); // yes

    if (keep_identifiers)
        for(const auto& [name,reg]: identifiers) // no
            scan.push_back(reg);
}

void reg_heap::trace(vector<int>& remap)
{
    /*
     * Roots are:
     * (i) roots
     * (ii) regs with steps
     * (iii) regs called from steps
     * (iv) regs with results -- why? Is this for index-vars-with-force?
     *
     * We then extend this with:
     * (v) regs in the environment of a referenced reg.
     * (vi) regs that are forced by a referenced reg.
     *
     * We don't follow use edges, because anything that is used should also be in the
     * environment.
     *
     * If something is only forced, then:
     * - if it is a constant (with force), we should be able to ignore its references.
     * - otherwise, the references will count.
     *
     * Thus, when we mark something forced, we can put it on a list of things that might be
     * only forced.  But if it is later referenced, how would we take it off that list?
     *
     */

    /*
     * Perhaps we have a problem mixing force-effects with values.  If we split a constant-with-force (i.e. a pair)
     * into a constant and force-thing that only forced other things, then we might be able to eliminate
     * the constant (which references two things) from the force-thing that has no references.
     *
     * We also have a problem with an index-var-with-force that evaluates to a constant.  Its not easy to
     * eliminate the index-var-with-force ... because it has a result?
     */

    /*
     * We have also replaced `seq` with `withEffect` to avoid forcing computation of the alignment prior
     * when we compute conditional likelihoods.
     */

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

    // 2. Get the list of root regs -- stack, temp, heads, and maybe identifiers.
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

    // 5. Mark regs with results in any token as used.
    for(auto i = regs.begin(); i != regs.end(); i++)
    {
        int r = i.addr();
        if (has_result1(r))
            mark_reg(r);
    }

    for(int t=0; t< tokens.size(); t++)
        if (token_is_used(t))
            for(auto [r,result]: tokens[t].vm_result.delta())
                if (result > 0)
                    mark_reg(r);

    // 6. Mark regs referenced only by regs as used.
    vector<int> tmp;
    for(int reg_index = 0;reg_index < used_regs.size();reg_index++)
    {
	int r = used_regs[reg_index];
	do_remap(*this, remap, r);
	const auto& R = regs.access(r);
	for(int r : R.C.Env)
	    mark_reg(r);

        if (auto& obj = R.C.exp; is_gcable_type(obj.type()))
        {
            auto gco = convert<GCObject>(obj.ptr());
            gco->get_regs(tmp);
            for(int r: tmp)
                mark_reg(r);
        }

        for(auto r : R.forced_regs)
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
    // 0. Count the number of regs/steps in the root program.
    if (log_verbose >= 4)
        trace_root();

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
            assert(not has_result1(r));
    }

    // Would it be faster to register a clearing callback?
    for(auto i = regs.begin();i != regs.end();)
    {
        int r = i.addr();
        i++;
	if (not regs.is_marked(r))
	{
            clear_back_edges_for_reg(r);
            assert(not has_result1(r));
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
    {
	for(int& r2: R.C.Env)
	{
	    assert(regs.is_used(r2));
	    r2 = remap[r2];
	    assert(regs.is_used(r2));
	}

        if (auto& obj = R.C.exp; is_gcable_type(obj.type()))
        {
            auto gco = convert<GCObject>(obj.ptr());
            gco->update_regs(remap);
        }
    }
    //  release_scratch_list();
    release_scratch_list();
}

void reg_heap::trace_root()
{
    // 1. Set up lists for used/marked regs, steps, and results.
    vector<int>& used_regs = get_scratch_list();
    int n_steps = 0;
    int n_regs = 0;

    auto mark_reg = [&,this](int r) {
	assert(r > 0);
	if (not regs.is_marked(r))
	{
	    regs.set_mark(r);
	    used_regs.push_back(r);
            if (has_step1(r)) n_steps++;
            n_regs++;
	}
    };

    // 2. Get the list of root regs -- stack, temp, heads, and maybe identifiers.
    for(int r: heads)
        mark_reg(r);

    // There shouldn't be any steps if there are no tokens.
    if (not get_n_tokens()) assert(steps.size() == steps.n_null());

    // 6. Mark regs referenced only by regs as used.
    vector<int> tmp;
    for(int reg_index = 0;reg_index < used_regs.size();reg_index++)
    {
	int r = used_regs[reg_index];
	const auto& R = regs.access(r);
	for(int r : R.C.Env)
	    mark_reg(r);

        if (auto& obj = R.C.exp; is_gcable_type(obj.type()))
        {
            auto gco = convert<GCObject>(obj.ptr());
            gco->get_regs(tmp);
            for(int r: tmp)
                mark_reg(r);
        }

        for(auto r : R.forced_regs)
	    mark_reg(r);

        // If there's a step, then the call should be marked.
        if (has_step1(r))
            mark_reg( call_for_reg(r) );
    }

    release_scratch_list();

    // Unmark all the regs.
    for(auto i = regs.begin();i != regs.end();)
    {
        int r = i.addr();
        i++;
	if (regs.is_marked(r))
            regs.unmark(r);
    }

    std::cerr<<"Root regs: "<<n_regs<<"\n";
    std::cerr<<"Root steps: "<<n_steps<<"\n";
    std::cerr<<"n_tokens = "<<get_n_active_tokens()<<"\n";
    std::cerr<<"n_contexts = "<<get_n_active_contexts()<<"\n";
}
