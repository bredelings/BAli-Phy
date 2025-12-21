#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/context.H"
#include "computation/expression/index_var.H"
#include "computation/expression/interchangeable.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/effects.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "probability/choose.H"
#include "computation/expression/constructor.H"
#include "computation/expression/modifiable.H"
#include "mcmc/slice-sampling.H"
#include "computation/operations.H"      // for is_seq( )
#include "models/TreeInterface.H"
#include "mcmc/sample.H"

#include "dp/dp-matrix.H"

using boost::dynamic_pointer_cast;

using std::pair;
using std::optional;
using std::vector;
using std::shared_ptr;

extern "C" closure builtin_function_registerTransitionKernelRaw(OperationArgs& Args)
{
    double rate = Args.evaluate(0).as_double();

    int r_transition_kernel = Args.evaluate_reg_use(Args.reg_for_slot(1));

    expression_ref E(constructor("Effect.TransitionKernel",2),{rate, index_var(0)});

    int r_effect = Args.allocate(closure{E,{r_transition_kernel}});

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

extern "C" closure builtin_function_registerLogger(OperationArgs& Args)
{
    int r_logger = Args.evaluate_reg_use(Args.reg_for_slot(0));

    expression_ref E(constructor("Effect.Logger",2),{index_var(0)});

    int r_effect = Args.allocate(closure{E,{r_logger}});

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

log_double_t get_multiplier(context_ref& C1, const vector<int>& I_regs)
{
    // So, why is this sum allowed?
    log_double_t multiplier(1);

    for(int r : I_regs)
    {
        int i = C1.get_reg_value(r).as_int();

        context C2 = C1;
        C2.set_reg_value(r, expression_ref(1-i));
        auto ratio = C2.probability_ratios(C1).total_ratio();
        if (uniform() < ratio/(1.0+ratio))
        {
            C1 = C2;
            multiplier *= 1.0+(1.0/ratio);
        }
        else
            multiplier *= 1.0+ratio;
    }
    return multiplier;
}

extern "C" closure builtin_function_sum_out_coals(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    if (log_verbose >= 3) std::cerr<<"\n\n[sum_out_coals]\n";

    int c1 = Args.evaluate(2).as_int();

    reg_heap& M = Args.memory();
    context_ref C1(M, c1);

    //------------- 1a. Get argument X -----------------
    int t_reg = -1;
    if (auto time_mod = context_ptr(C1, Args.reg_for_slot(0)).modifiable())
        t_reg = time_mod->get_reg();
    else
        throw myexception()<<"sum_out_coals: time variable is not modifiable!";

    //------------- 1b. Get arguments Y_i  -----------------
    vector<int> I_regs;

    auto I_ptrs = context_ptr(C1, Args.reg_for_slot(1)).list_elements();
    for(auto& I_ptr: I_ptrs)
    {
        if (auto I_mod = I_ptr.modifiable())
            I_regs.push_back( I_mod->get_reg() );
        else
            throw myexception()<<"sum_out_coals: indicator variable is not modifiable!";
    }

    //------------- 2. For t1, sample Is and sum over the Is ------------//
    int t1 = C1.evaluate_reg(t_reg).as_int();

    // The sum is this multiplier times the probability of the current Is.
    auto multiplier1 = get_multiplier(C1, I_regs);

    //------------- 3. Figure out the multiplier for summing over all the Is for t2 ------------//
    int t2 = t1 + 1;
    if (uniform() < 0.5)
    {
	t2 = t1 - 1;
	if (t2 < 0)
	    t2 = 0;
    }

    context C2 = C1;
    C2.set_reg_value(t_reg, expression_ref(t2));

    auto multiplier2 = get_multiplier(C2, I_regs);

    //------------- 5. Choose to accept or not, depending on the relative probabilities.
    auto ratio = C2.probability_ratios(C1).total_ratio();

    int choice = choose2(multiplier1, ratio*multiplier2);

    //------------- 6. Set x depending on the choice
    if (choice == 1)
	C1 = C2;
    
    return constructor("()",0);
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_gibbsSampleCategoricalRaw(OperationArgs& Args)
{
    if (log_verbose >= 3) std::cerr<<"\n\n[gibbs_sample_categorical]\n";

    assert(not Args.evaluate_changeables());

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_unchangeable(0);

    //------------- 1b. Get range [0,n) for X ----------
    int n_values_reg = Args.evaluate_slot_unchangeable(1);

    //------------- 1c. Get context index --------------
    int c1 = Args.evaluate(2).as_int();

    //------------- 2. Find the location of the variable -------------//
    auto& M = Args.memory();
    auto x_mod_reg = M.find_modifiable_reg(x_reg);
    if (not x_mod_reg)
        throw myexception()<<"gibbs_sample_categorical: reg "<<x_reg<<" not modifiable!";

    //------------- 3. Get initial value x1 for variable -------------//
    context_ref C1(M, c1);
    // This  (i) drops references to previous execution contexts and
    //      (ii) ensures that all computations will be shared forwards.
    C1.evaluate_program();

    int x1 = C1.get_reg_value(*x_mod_reg).as_int();

    int n_values = C1.get_reg_value(n_values_reg).as_int();
    if (log_verbose >= 3) std::cerr<<"   gibbs_sample_categorical: <"<<x_reg<<">   [0, "<<n_values-1<<"]\n";

    //------------- 4. Figure out probability of each value ----------//

    context C2 = C1;
    optional<int> c2_value;
    vector<log_double_t> pr_x(n_values, 1.0);
    for(int i=0; i<n_values; i++)
        // For i == x1 we already know that the ratio is 1.0
	if (i != x1)
	{
            C2 = C1;
            c2_value = i;

	    C2.set_reg_value(*x_mod_reg, expression_ref(i));

	    pr_x[i] = C2.probability_ratios(C1).total_ratio();
	}

    //------------- 5. Get new value x2 for variable -----------------//
    int x2 = choose(pr_x);

    if (log_verbose >= 3) std::cerr<<"   gibbs_sample_categorical: <"<<x_reg<<">   "<<x1<<" -> "<<x2<<"\n";

    if (x2 == x1)
        ;
    else if (c2_value and x2 == *c2_value)
        C1 = C2;
    else
        C1.set_reg_value(*x_mod_reg, expression_ref(x2));

    return constructor("()",0);
}

Proposal uniform_avoid_mh_proposal(int a, int b, int x_reg)
{
    return [=](context_ref& C)
           {
               // 1. Find the modifiable
               auto x_mod_reg = C.find_modifiable_reg(x_reg);
               if (not x_mod_reg)
                   throw myexception()<<"discreteUniformAvoidMH: reg "<<x_reg<<" not modifiable!";

               // 2. Get the current value
               int x1 = C.get_reg_value(*x_mod_reg).as_int();
               if (x1 < a or x1 > b)
                   throw myexception()<<"discreteUniformAvoidMH: value "<<x1<<" not in range ["<<a<<", "<<b<<"]";

               // 3. Propose a new value
               int x2 = uniform_int(a,b-1);
               if (x2 >= x1) x2++;

               // 4. Set the new value
               C.set_reg_value(*x_mod_reg, expression_ref(x2));

               // 5. Return the proposal ratio
               return 1.0;
           };
}

bool perform_MH_(reg_heap& M, int context_index, const Proposal& proposal)
{
    // 1. Construct a reference to the context
    context_ref C1(M, context_index);

    // 2. Make a duplicate context to modify
    context C2 = C1;

    // 3. Propose a new state
    auto proposal_ratio = proposal(C2);

    // 4. Accept or reject the proposal
    return perform_MH(C1, C2, proposal_ratio);
}


// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_discreteUniformAvoidMHRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    //------------- 1a. Get the proposal ---------------
    int x_reg = Args.evaluate_slot_unchangeable(0);

    int a = Args.evaluate(1).as_int();
    int b = Args.evaluate(2).as_int();

    if (log_verbose >= 3) std::cerr<<"\n\n[discrete_uniform_avoid_mh] <"<<x_reg<<"> in ["<<a<<", "<<b<<"]\n";

    //------------- 1d. Get context index --------------
    int c1 = Args.evaluate(3).as_int();

    //------------- 2. Perform the proposal ------------
    auto& M = Args.memory();

    auto proposal = uniform_avoid_mh_proposal(a, b, x_reg);

    perform_MH_(M, c1, proposal);

    //------------- 4. Return () -----------------------
    return constructor("()",0);
}



Proposal inc_dec_mh_proposal(int x_reg, int n, const bounds<int>& range)
{
    return [=](context_ref& C)
           {
               // 1. Find the modifiable
               auto x_mod_reg = C.find_modifiable_reg(x_reg);
               if (not x_mod_reg)
                   throw myexception()<<"discreteUniformAvoidMH: reg "<<x_reg<<" not modifiable!";

               // 2. Get the current value
               int x1 = C.get_reg_value(*x_mod_reg).as_int();

               // 3. Propose a new value
               int x2 = x1;
	       int delta = uniform_int(1,n);
               if (uniform() > 0.5)
                   x2 += delta;
               else
                   x2 -= delta;

               // 4. Cancel move if its outside of the bounds
               if (not range.in_range(x2))
                   x2 = x1;
               // This retains symmetry between an item on the edge of the range, and its neighbor.

               // 5. Set the new value
               if (x1 != x2)
                   C.set_reg_value(*x_mod_reg, expression_ref(x2));

               // 6. Return the proposal ratio
               return 1.0;
           };
}

template <typename T>
using Bounds = Box<bounds<T>>;

// inc_dec_mh x bounds context state
extern "C" closure builtin_function_incDecMHRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get the proposal ---------------
    int x_reg = Args.evaluate_slot_unchangeable(0);

    if (log_verbose >= 3) std::cerr<<"\n\n[incDecMH] <"<<x_reg<<">\n";

    //------------- 1b. Get context index --------------
    auto range = Args.evaluate(1).as_<Bounds<int>>();

    //------------- 1c. Get context index --------------
    int c1 = Args.evaluate(2).as_int();
    context_ref C1(M, c1);

    //------------- 2. Perform the proposal ------------
    auto proposal = inc_dec_mh_proposal(x_reg, 2, range);

    if (log_verbose >= 3) std::cerr<<C1.get_logged_parameters()<<"\n";

    perform_MH_(M, c1, proposal);

    if (log_verbose >= 3) std::cerr<<C1.get_logged_parameters()<<"\n";

    //------------- 4. Return () -----------------------
    return constructor("()",0);
}

// slice_sample_real_random_variable x context state
extern "C" closure builtin_function_sliceSampleRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    // 2. context index = int
    int context_index = Args.evaluate(2).as_int();
    context_ref C(M,context_index);

    auto evaluate_slot = [&](int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    // 0. x = reg to sample
    int x_reg = Args.reg_for_slot(0);
    if (auto x_mod_reg = C.find_modifiable_reg(x_reg))
        x_reg = *x_mod_reg;
    else
        throw myexception()<<"sliceSample: reg "<<x_reg<<" is not a modifiable!";

    if (log_verbose >= 3) std::cerr<<"\n\n[sliceSample] <"<<x_reg<<">\n";

    // 1. bounds
    auto bnds = evaluate_slot(1);

    //------------- 3. Get initial value x1 for variable -------------//
    if (not bnds.is_a<Bounds<double>>())
        throw myexception()<<"random variable doesn't have a range that is bounds<double>";

    random_variable_slice_function logp(C, bnds.as_<Bounds<double>>(), x_reg);

    // Tuning this would be better.
    // However, we now find slice boundaries by doubling instead of stepping out.
    // Therefore the cost of not being tuned is more limited.
    double w = 1.0;

    // NOTE: Although this function returns a value, we are expecting the
    //       slice_sample routine to the variable to the final value.
    slice_sample(logp, w, 50);

    if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times.\n";

    return constructor("()",0);
}

// slice_sample_integer_random_variable x context state
extern "C" closure builtin_function_sliceSampleIntegerRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    // 2. context index = int
    int context_index = Args.evaluate(2).as_int();
    context_ref C(M,context_index);

    auto evaluate_slot = [&](int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    // 0. x = reg to sample
    int x_reg = Args.reg_for_slot(0);
    if (auto x_mod_reg = C.find_modifiable_reg(x_reg))
        x_reg = *x_mod_reg;
    else
        throw myexception()<<"slice_sample_integer_random_variable: reg "<<x_reg<<" is not a modifiable!";

    if (log_verbose >= 3) std::cerr<<"\n\n[slice_sample_integer_random_variable] <"<<x_reg<<">\n";

    // 1. bounds
    auto bnds = evaluate_slot(1);

    //------------- 3. Get initial value x1 for variable -------------//
    if (not bnds.is_a<Bounds<int>>())
        throw myexception()<<"random variable doesn't have a range that is bounds<int>";

    integer_random_variable_slice_function logp(C, bnds.as_<Bounds<int>>(), x_reg);

    // OK, this is a bit problematic.
    double w = 1.0;

    double v1 = logp.current_value() + uniform();
    slice_sample(v1, logp, w, 50);

    if (log_verbose >= 3) std::cerr<<C.get_logged_parameters()<<"\n";
    if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times.\n";

    return constructor("()",0);
}

// slice_sample_real_random_variable x context state
extern "C" closure builtin_function_scaleGroupsSliceRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    if (log_verbose >= 3) std::cerr<<"\n\n[scaleGroupsSlice]\n";

    auto& M = Args.memory();

    // 2. context index = int
    int context_index = Args.evaluate(2).as_int();
    context_ref C(M,context_index);

    // 0. scales
    auto scales = context_ptr(C, Args.reg_for_slot(0)).list_elements();

    // 1. branch_lengths
    auto branch_lengths = context_ptr(C, Args.reg_for_slot(1)).list_elements();

    vector<int> r_scales;
    for(auto scale: scales)
    {
        if (auto m = scale.modifiable())
            r_scales.push_back(m->get_reg());
        else
            return constructor("()",0);
    }

    vector<int> r_branch_lengths;
    for(auto branch_length: branch_lengths)
    {
        if (auto m = branch_length.modifiable())
            r_branch_lengths.push_back(m->get_reg());
        else
            return constructor("()",0);
    }


    scale_groups_slice_function logp(C, r_scales, r_branch_lengths);

    // Tuning this would be better.
    // However, we now find slice boundaries by doubling instead of stepping out.
    // Therefore the cost of not being tuned is more limited.
    double w = 0.6;

    // NOTE: Although this function returns a value, we are expecting the
    //       slice_sample routine to set the variables to the final value.
    slice_sample(logp, w, 50);

    if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times.\n";

    return constructor("()",0);
}

extern "C" closure builtin_function_scaleGroupsProposalRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    if (log_verbose >= 3) std::cerr<<"\n\n[scaleGroupsProposalRaw]\n";

    auto& M = Args.memory();

    // 2. context index = int
    int context_index = Args.evaluate(2).as_int();
    context_ref C(M,context_index);

    // 0. scales
    auto scales = context_ptr(C, Args.reg_for_slot(0)).list_elements();

    // 1. branch_lengths
    auto branch_lengths = context_ptr(C, Args.reg_for_slot(1)).list_elements();

    bool ok = true;
    for(auto& scale: scales)
    {
        if (not scale.move_to_modifiable())
            ok = false;
    }

    for(auto branch_length: branch_lengths)
    {
        if (not branch_length.move_to_modifiable())
            ok = false;
    }

    log_double_t ratio = 1;

    if (ok)
    {
        // Could we parameterize by the proposal distribution on factor?
        // What should we do if this is asymmetric?  How does that modify the ratio?
        double sigma = 0.5;
        double factor = gaussian(0,sigma);
        factor = exp(factor);
        if (log_verbose >= 3) std::cerr<<"\n\n  factor = "<<log(factor)<<"\n";

        for(auto& scale: scales)
            scale.set_value( scale.value().as_double() * factor );

        for(auto& branch_length: branch_lengths)
            branch_length.set_value( branch_length.value().as_double() / factor );

        ratio = pow(log_double_t(factor), (int)scales.size() - (int)branch_lengths.size() );
    }

    if (log_verbose >= 3) std::cerr<<"\n\n  ratio = "<<log(ratio)<<"\n";

    return { ratio };
}




void sample_tri_one(context_ref& /*C1*/, TreeInterface& /*t*/, int /*b*/)
{
}

void sample_alignments_one(context_ref& C, TreeInterface& t, int b)
{
    //select_root(b)

    if (t.is_leaf_node(t.target(b)))
	b = t.reverse(b);

#if !defined(NDEBUG_DP) || !defined(NDEBUG)
    const context C0 = C;
#endif
    vector<context> c;
    c.push_back(C);

    vector< vector< shared_ptr<DPmatrixSimple> > > Matrices(1);
    std::abort();
    // log_double_t total_ratio = 1.0;
    for(int i=0;i<c.size();i++)
    {

/*
	for(int j=0;j<c[i].n_data_partitions();j++)
	    if (c[i][j].variable_alignment())
	    {
                auto [M, ratio] = sample_alignment_base(c[i][j], b);
                total_ratio *= ratio;
		Matrices[i].push_back(M);
		// If Pr_sum_all_paths() == 0, then the alignment for this partition will be unchanged.
#ifndef NDEBUG
		c[i][j].likelihood();  // check the likelihood calculation
#endif
	    }
	    else
		Matrices[i].push_back(shared_ptr<DPmatrixSimple>());
*/

    }
}

/*
  So, we have

      a --x-> b ---> c --w-> d
        <-y--   <---   <-z--

And we want to change to

      d --z-> b ---> c --y-> a
        <-w--   <---   <-x--

So, target(z) changes from c to b
    source(w) changes from c to b
    target(x) changes from b to c
    source(y) changes from b to c

    out(b,i) changes from y to w
    out(c,j) changes from w to y

    we need to find out the i and j.
 */


void NNI(context_ref& c, int tree_reg, int b1, int b2)
{
    ModifiablesTreeInterface T(c,tree_reg);

    tryNNI(T, b1, b2);
}

void NNI_move(context_ref& C1, int tree_reg, int b)
{
    ModifiablesTreeInterface T(C1,tree_reg);

    if (T.is_leaf_branch(b)) return;

    // set subst_root(s)

    // get the name of neighboring branches.
    vector<int> branches;
    T.append_branches_after(T.reverse(b), branches);
    T.append_branches_after(b, branches);

    vector<context> c(3,C1);

    NNI(c[1], tree_reg, branches[0], branches[2]);
    NNI(c[2], tree_reg, branches[0], branches[3]);

    //------------ Select the topology --------------//
    vector<log_double_t> pr(3);
    for(int i=0;i<3;i++)
        pr[i] = c[i].heated_probability();

    int j = choose_MH(0,pr);

    //---------- Set the selected topology ----------//
    C1 = c[j];
}


/* Like NNI, except
 * + we can only swap branches pointing away from the root
 * + we need to avoid children being older than their parents.
 * + there is a degree 2 node.
 *
 * Specificially, we need u to be younger than y.
 *
 *
 *                       |
 *                    ---x---
 *                    |     |
 *                  --y--   |
 *                  |   |   |
 *                  z   w   u
 *
 *
 * Here, branches[0] = (y,z)
 *       branches[1] = (y,w)
 *       branches[2] = (x,u)
 *       b           = (x,y)
 */


void TT_NNI_move(context_ref& C1, int tree_reg, int b)
{
    ModifiablesTreeInterface T(C1,tree_reg);

    //------------ Skip if invalid branch -------------//

    if (T.is_leaf_branch(b)) return;

    if (not T.away_from_root(b)) return;

    //-------------- Set subst_root(s)----------------//

    // * FIXME! set subst root!

    //--  get neighboring branches pointing TIPWARD --//
    vector<int> branches;
    T.append_branches_after(b, branches);
    assert(T.away_from_root(branches[0]));
    assert(T.away_from_root(branches[1]));

    T.append_branches_after(T.reverse(b), branches);
    if (branches.size() == 4)
    {
        if (not T.away_from_root(branches[2]))
            branches.erase(branches.begin()+2);
        else
            branches.erase(branches.begin()+3);
    }
    assert(T.away_from_root(branches[2]));

    //----------- Check node u < node y -------------//
    int y = T.target(b);
    double t_y = T.node_time(y);

    int u = T.target(branches[2]);
    double t_u = T.node_time(u);

    if (t_u > t_y) return;

    //------------ Perform the NNIs -----------------//
    vector<context> c(3,C1);

    NNI(c[1], tree_reg, branches[0], branches[2]);
    NNI(c[2], tree_reg, branches[1], branches[2]);

    //------------ Select the topology --------------//
    vector<log_double_t> pr(3);
    for(int i=0;i<3;i++)
        pr[i] = c[i].heated_probability();

    int j = choose_MH(0,pr);

    //---------- Set the selected topology ----------//
    C1 = c[j];
}


/*
 *        gp
 *        |           np
 *        |           |
 *        p . . . . . |
 *        |\          |
 *        | \         n2
 *        n  s
 */


void FNPR_move(context_ref& c, int tree_reg, int n)
{
    ModifiablesTreeInterface T(c, tree_reg);

    // Return if n is the root.
    int p;
    if (auto pp = T.parent_of_node(n))
        p = *pp;
    else
        return;

    // Return if parent(n) is the root.
    int gp;
    if (auto gpp = T.parent_of_node(p))
        gp = *gpp;
    else
        return;

    int sibling;
    {
        auto cs = T.children_of_node(p);
        assert(n == cs[0] or n == cs[1]);
        sibling = (n == cs[1]) ? cs[0] : cs[1];
        assert(sibling != n);
    }

    double p_t = T.node_time(p);

    vector<int> branch_targets;
    for(int n2: T.nodes())
    {
        auto b = T.parent_branch_for_node(n2);
        if (not b) continue;

        int p2 = T.source(*b);

        // We're looking for branches other than
        // (p,n), (p,s) and (gp,p)
        if (n2 == n or n2 == p or n2 == sibling) continue;

        auto n2_t = T.node_time(n2);
        auto p2_t = T.node_time(p2);

        if (n2_t < p_t and p_t < p2_t)
            branch_targets.push_back(*b);
    }

    if (not branch_targets.empty())
    {
        int i = uniform_int(0, branch_targets.size()-1);
        int b = branch_targets[i];
        int n2 = T.target(b);
        int p2 = T.source(b);

        T.reconnect_branch(gp, p, n);
        T.reconnect_branch(p, n, n2);
        T.reconnect_branch(p2, n2, p);
    }
}


extern "C" closure builtin_function_walkTreePathRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.evaluate_slot_unchangeable(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);
    ModifiablesTreeInterface T(C1,tree_reg);

    //------------ 3. Get the substitution root --------//

    // FIXME: encode the subst_root in the tree somehow?
    int subst_root = T.n_nodes()-1;

    //------------ 4. Walk the tree and realign --------//
    auto branches = walk_tree_path(T, subst_root);

    object_ptr<EVector> v (new EVector);
    for(int branch: branches)
        v->push_back(branch);

    return v;
}

extern "C" closure builtin_function_fnprUnsafeProposalRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.evaluate_slot_unchangeable(0);

    int n = Args.evaluate(1).as_int();

    int context_index = Args.evaluate(2).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C(M, context_index);

    FNPR_move(C, tree_reg, n);

    return {log_double_t(1.0)};
}

extern "C" closure builtin_function_walkTreeSampleAlignmentsRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int as_reg  = Args.reg_for_slot(1);

    int c1 = Args.evaluate(2).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg, {as_reg})));
    walk_tree_sample_alignments(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

extern "C" closure builtin_function_realignFromTipsRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int as_reg  = Args.reg_for_slot(1);

    int c1 = Args.evaluate(2).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg, {as_reg})));
    realign_from_tips(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

/// sample_node

/// sample_two_nodes

extern "C" closure builtin_function_sampleSPRFlatRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    sample_SPR_flat(P,Stats);
    C1 = *P;

    return constructor("()",0);
}


extern "C" closure builtin_function_sampleSPRNodesRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    sample_SPR_nodes(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

extern "C" closure builtin_function_sampleSPRAllRaw(OperationArgs& Args)
{
    if (log_verbose >= 3) std::cerr<<"\n\n[sample_SPR_all]\n";

    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    sample_SPR_all(P,Stats);
    C1 = *P;

    return constructor("()",0);
}


extern "C" closure builtin_function_walkTreeSampleBranchLengthsRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    walk_tree_sample_branch_lengths(P,Stats);
    C1 = *P;

    return constructor("()",0);
}


extern "C" closure builtin_function_walkTreeSampleNNIandBranchLengthsRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    walk_tree_sample_NNI_and_branch_lengths(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

extern "C" closure builtin_function_walkTimeTreeSampleNNIandNodeTimesRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    walk_time_tree_sample_NNI_and_node_times(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

extern "C" closure builtin_function_walkTreeSampleNNIRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    walk_tree_sample_NNI(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

extern "C" closure builtin_function_walkTreeSampleNNIandARaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------//
    int tree_reg = Args.reg_for_slot(0);

    int c1 = Args.evaluate(1).as_int();

    //------------ 2. Make a TreeInterface -------------//
    context_ref C1(M, c1);

    MCMC::MoveStats Stats;
    owned_ptr<context> P(claim(new Parameters(C1, tree_reg)));
    walk_tree_sample_NNI_and_A(P,Stats);
    C1 = *P;

    return constructor("()",0);
}

/// scale_scales_only (MH)

/// scale_scales_only (slice)


// gibbs_sample_categorical x n pr

extern "C" closure builtin_function_getAtomicModifiableValueInContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto& M = Args.memory();

    int x_reg = Args.evaluate_slot_unchangeable(0);

    int context_index = Args.evaluate(1).as_int();

    context_ref C(M, context_index);

    auto x_mod_reg = C.find_modifiable_reg(x_reg);
    if (not x_mod_reg)
	throw myexception()<<"getValueInContext: reg "<<x_reg<<" not modifiable!";

    expression_ref x_value = C.get_reg_value(*x_mod_reg);
    if (x_value.is_expression())
	throw myexception()<<"getValueInContext got non-atomic value '"<<x_value<<"'";

    return x_value;
}

extern "C" closure builtin_function_setAtomicModifiableValueInContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto& M = Args.memory();

    int x_reg = Args.evaluate_slot_unchangeable(0);

    auto value = Args.evaluate(1);

    int context_index = Args.evaluate(2).as_int();

    context_ref C(M, context_index);

    auto x_mod_reg = C.find_modifiable_reg(x_reg);
    if (not x_mod_reg)
	throw myexception()<<"getValueInContext: reg "<<x_reg<<" not modifiable!";

    if (value.is_expression())
	throw myexception()<<"getValueInContext got non-atomic value '"<<value<<"'";

    C.set_reg_value(*x_mod_reg, value);

    return constructor("()",0);
}

extern "C" closure builtin_function_copyContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    int c1 = Args.evaluate(0).as_int();

    int c2 = M.copy_context(c1);

    return {c2};
}


extern "C" closure builtin_function_releaseContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    int c = Args.evaluate(0).as_int();

    M.release_context(c);

    return constructor("()",0);
}

extern "C" closure builtin_function_switchToContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    int c1 = Args.evaluate(0).as_int();

    int c2 = Args.evaluate(1).as_int();

    M.switch_to_context(c1,c2);

    return constructor("()",0);
}


extern "C" closure builtin_function_acceptMH(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    int c1 = Args.evaluate(0).as_int();

    int c2 = Args.evaluate(1).as_int();

    log_double_t ratio = Args.evaluate(2).as_log_double();

    context_ref C1(M,c1);

    context_ref C2(M,c2);

    bool accept = accept_MH(C1, C2, ratio);

    return {accept};
}

extern "C" closure builtin_function_getInterchangeableId(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    int id = M.next_interchangeable_id++;

    return {id};
}


extern "C" closure builtin_function_registerInterchangeable(OperationArgs& Args)
{
    // 1. Get the interchangeable id.
    int id = Args.evaluate(0).as_int();

    // 2. Force the interchangeable and get its address
    Args.evaluate_(1);

    int r_ix = Args.reg_for_slot(1);

    auto& M = Args.memory();

    r_ix = M.follow_index_var_no_force(r_ix);

    assert(M.expression_at(r_ix).head().is_a<interchangeable>());

    // 3. Create the effect
    object_ptr<RegisterInterchangeable> e(new RegisterInterchangeable{id, r_ix});

    int r_effect = Args.allocate(closure(e));

    Args.set_effect(r_effect);

    return {index_var(0), {r_effect}};
}

Proposal interchange_regs_proposal(int r1, int r2)
{
    return [=](context_ref& C)
           {
               // 1. Interchange the regs
               C.interchange_regs(r1, r2);

               // 2. Return the proposal ratio
               return 1.0;
           };
}

pair<int,int> random_different_int_pair(int n)
{
    assert(n > 1);
    int i1 = uniform_int(0,n-1);
    int i2 = uniform_int(0,n-2);
    if (i2 >= i1)
        i2++;
    return {i1,i2};
}


pair<int,int> random_different_element_pair(const vector<int>& v)
{
    auto [i1,i2] = random_different_int_pair(v.size());

    return {v[i1],v[i2]};
}


extern "C" closure builtin_function_interchangeEntriesRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1. Get list arguments --------------//
    int c1 = Args.evaluate(1).as_int();

    context_ref C1(M, c1);

    // int id = Args.evaluate(0).as_int();
    int id = C1.get_reg_value(Args.reg_for_slot(0)).as_int();

    //------------ 2. Find interchangeable regs ----------//
    if (M.interchangeables.count(id))
    {
        vector<int> interchange_regs;
        for(int r: M.interchangeables.at(id))
            interchange_regs.push_back(r);

        if (log_verbose >= 3)
        {
            std::cerr<<"\n\n[interchange_list_entries] id = <"<<id<<">    interchangeable entries = ";
            for(auto& r: interchange_regs)
                std::cerr<<"<"<<r<<"> ";
            std::cerr<<"\n";
        }

        int n = 0;
        if (interchange_regs.size() >= 2)
            n = sqrt( interchange_regs.size() );

        for(int i=0;i<n;i++)
        {
            auto [r1, r2] = random_different_element_pair(interchange_regs);
            if (log_verbose >= 3) std::cerr<<"\n\n[interchange_list_entries] interchanging = <"<<r1<<"> and <"<<r2<<">\n";
            perform_MH_(M, c1, interchange_regs_proposal(r1, r2));
        }
    }

    return constructor("()",0);
}

extern "C" closure builtin_function_createContext(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int r_prog = Args.reg_for_slot(0);

    int r_log = Args.reg_for_slot(1);

    int c = M.get_first_context(r_prog, r_log);

    return {c};
}

extern "C" closure builtin_function_runMCMC(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int max_iterations = Args.evaluate(0).as_int();

    int c = Args.evaluate(1).as_int();
    context_ref C(M, c);

    //---------------- Run the MCMC chain -------------------//
    for(int iterations=0; iterations < max_iterations; iterations++)
    {
	if (log_verbose>=1)
	    std::cerr<<"iterations = "<<iterations<<"\n";
        C.run_loggers(iterations);
        C.run_transition_kernels();
    }

    C.run_loggers(max_iterations);

    return constructor("()",0);
}

// Really this should return a Text
extern "C" closure builtin_function_writeTraceGraph(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();
    context_ref C(M, c);

    C.evaluate_program();
    C.show_graph_for_root_token();

    return constructor("()",0);
}

// These are defined in models/model.cc
void simplify(json::object& j);
json::object flatten_me(const json::object& j);

// Probably we should put this whole thing into the machine.
extern "C" closure builtin_function_logJSONRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();
    context_ref C(M, c);

    int t = Args.evaluate(1).as_int();

    json::object j;

    j["iter"] = t;
    j["prior"] = log(C.prior());
    j["likelihood"] = log(C.likelihood());
    j["posterior"] = log(C.probability());
    // Probably we should put this into the machine.
    j["parameters/"] = C.get_logged_parameters();

    object_ptr<Box<json::value>> result(new Box<json::value>(j));

    return result;
}

extern "C" closure builtin_function_jsonToTableLineRaw(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto j = Args.evaluate(0).as_<Box<json::value>>().as_object();

    simplify(j);
    j = flatten_me(j);

    std::ostringstream line;
    line.precision(17);
    for(auto& [key,j2]: j)
        line<<"   "<<key<<" = "<<j2;

    return String(line.str());
}

extern "C" closure builtin_function_prior(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();
    context_ref C(M, c);

    return {C.prior()};
}

extern "C" closure builtin_function_likelihood(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();
    context_ref C(M, c);

    return {C.likelihood()};
}

extern "C" closure builtin_function_posterior(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    int c = Args.evaluate(0).as_int();
    context_ref C(M, c);

    return {C.probability()};
}

