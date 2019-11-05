#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/context.H"
#include "computation/expression/index_var.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/effects.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "probability/choose.H"
#include "computation/expression/constructor.H"
#include "computation/expression/random_variable.H"
#include "computation/expression/modifiable.H"
#include "mcmc/slice-sampling.H"
#include "computation/operations.H"      // for is_seq( )

using boost::dynamic_pointer_cast;

using std::optional;
using std::vector;

optional<int> find_modifiable_in_root_token(reg_heap& M, int r)
{
    // Warning: ABOMINATION!
    // FIXME: This should be forced by a `seq` inside the program.
    r = M.incremental_evaluate(r).first;

    // r should not be unknown or an index_var
    assert(M.reg_is_constant(r) or (M.reg_is_changeable(r) and M.reg_has_call(r)));

    while (not M.reg_is_constant(r))
    {
        assert(M.reg_is_changeable(r));
        assert(M.reg_has_call(r));

        if (is_modifiable(M[r].exp))
            return r;
        else
            r = M.call_for_reg(r);
    };

    // r is (now) a constant.
    // There is therefore no modifiable.
    return {};
}

extern "C" closure builtin_function_register_transition_kernel(OperationArgs& Args)
{
    int r_rate = Args.reg_for_slot(0);

    int r_transition_kernel = Args.reg_for_slot(1);

    int r_effect = Args.allocate(new register_transition_kernel(r_rate, r_transition_kernel));

    Args.set_effect(r_effect);

    // Return a reference to the effect.
    return {index_var(0),{r_effect}};
}

// The idea here is to propose new values of X, and evaluate them by summing over each Y_i \in {True,False}.
// Then the Y_i are resampled.  

double log1pexp(double x)
{
    if (x < 18.0)
	return log1p(exp(x));
    else if (x < 33.3)
	return x + exp(-x);
    else
	return x;
}

log_double_t get_multiplier(reg_heap& M, const vector<int>& I_regs, int c1)
{
    // So, why is this sum allowed?
    log_double_t multiplier(1);

    for(int r : I_regs)
    {
	int i = M.get_reg_value_in_context(r, c1).as_int();

	int c2 = M.copy_context(c1);
	M.set_reg_value_in_context(r, expression_ref(1-i), c2);
	auto ratio = M.probability_ratios(c1,c2).total_ratio();
	if (uniform() < ratio/(1.0+ratio))
	{
	    M.switch_to_context(c1, c2);
	    multiplier *= 1.0+(1.0/ratio);
	}
	else
	    multiplier *= 1.0+ratio;
	M.release_context(c2);
    }
    return multiplier;
}

extern "C" closure builtin_function_sum_out_coals(OperationArgs& Args)
{
    int state = Args.evaluate(3).as_int();

    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    if (log_verbose >= 3) std::cerr<<"\n\n[sum_out_coals]\n";

    //------------- 1a. Get argument X -----------------
    int t_reg = Args.evaluate_slot_to_reg(0);
    if (auto t_mod_reg = find_modifiable_in_root_token(M, t_reg))
        t_reg = *t_mod_reg;
    else
        throw myexception()<<"sum_out_coals: time variable is not modifiable!";

    int c1 = Args.evaluate(2).as_int();

    //------------- 1b. Get arguments Y_i  -----------------
    vector<int> I_regs;

    int next_reg = Args.reg_for_slot(1);
    const closure* top = &M.lazy_evaluate(next_reg, c1);
    while(top->exp.size())
    {
	assert(has_constructor(top->exp,":"));
	assert(top->exp.size() == 2);

	int element_reg = top->reg_for_slot(0);

	next_reg = top->reg_for_slot(1);

	// evaluate the list element in token 0
	element_reg = Args.evaluate_reg_to_reg(element_reg);
        if (auto element_mod_reg = find_modifiable_in_root_token(M, element_reg))
            element_reg = *element_mod_reg;
        else
            throw myexception()<<"sum_out_coals: indicator variable is not modifiable!";

	// Add the element to the list.
	I_regs.push_back( element_reg );
	// Move to the next element or end
	top = &M.lazy_evaluate(next_reg, c1);
    }
    assert(has_constructor(top->exp,"[]"));

    //------------- 2. For t1, sample Is and sum over the Is ------------//

    int t1 = M.lazy_evaluate(t_reg, c1).exp.as_int();

    // The sum is this multiplier times the probability of the current Is.
    auto multiplier1 = get_multiplier(M, I_regs, c1);

    //------------- 3. Figure out the multiplier for summing over all the Is for t2 ------------//
    int t2 = t1 + 1;
    if (uniform() < 0.5)
    {
	t2 = t1 - 1;
	if (t2 < 0)
	    t2 = 0;
    }

    int c2 = M.copy_context(c1);
    M.set_reg_value_in_context(t_reg, expression_ref(t2), c2);

    auto multiplier2 = get_multiplier(M, I_regs, c2);

    //------------- 5. Choose to accept or not, depending on the relative probabilities.
    auto ratio = M.probability_ratios(c1,c2).total_ratio();

    int choice = choose2(multiplier1, ratio*multiplier2);

    //------------- 6. Set x depending on the choice
    if (choice == 1)
	M.switch_to_context(c1,c2);
    M.release_context(c2);
    
    return EPair(state+1,constructor("()",0));
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_gibbs_sample_categorical(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_to_reg(0);

    //------------- 1b. Get range [0,n) for X ----------
    int n_values = Args.evaluate(1).as_int();

    if (log_verbose >= 3) std::cerr<<"\n\n[gibbs sample_categorical] <"<<x_reg<<"> in [0, "<<n_values-1<<"]\n";

    //------------- 1c. Get context index --------------
    int c1 = Args.evaluate(2).as_int();

    //------------- 1d. Get monad thread state ---------
    int state = Args.evaluate(3).as_int();

    //------------- 2. Find the location of the variable -------------//
    auto& M = Args.memory();
    auto x_mod_reg = find_modifiable_in_root_token(M, x_reg);
    if (not x_mod_reg)
        throw myexception()<<"gibbs_sample_categorical: reg "<<x_reg<<" not modifiable!";

    //------------- 3. Get initial value x1 for variable -------------//
    context_ref C1(M, c1);

    int x1 = C1.get_reg_value(*x_mod_reg).as_int();

    //------------- 4. Figure out probability of each value ----------//
    vector<log_double_t> pr_x(n_values, 1.0);
    for(int i=0; i<n_values; i++)
        // For i == x1 we already know that the ratio is 1.0
	if (i != x1)
	{
            context C2 = C1;

	    C2.set_reg_value(*x_mod_reg, expression_ref(i));

	    pr_x[i] = C2.probability_ratios(C1).total_ratio();
	}

    //------------- 5. Get new value x2 for variable -----------------//
    int x2 = choose(pr_x);

    if (x2 != x1)
	C1.set_reg_value(*x_mod_reg, expression_ref(x2));

    return EPair(state+1,constructor("()",0));
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_discrete_uniform_avoid_mh(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_to_reg(0);

    //------------- 1bc. Get range [a,a+n-1] for X -----
    int a = Args.evaluate(1).as_int();
    int b = Args.evaluate(2).as_int();

    if (log_verbose >= 3) std::cerr<<"\n\n[discrete_uniform_avoid_mh] <"<<x_reg<<"> in ["<<a<<", "<<b<<"]\n";

    //------------- 1d. Get context index --------------
    int c1 = Args.evaluate(3).as_int();

    //------------- 1e. Get monad thread state ---------
    int state = Args.evaluate(4).as_int();

    //------------- 2. Find the location of the variable -----------//
    auto& M = Args.memory();
    auto x_mod_reg = find_modifiable_in_root_token(M, x_reg);
    if (not x_mod_reg)
        throw myexception()<<"discrete_uniform_avoid_mh: reg "<<x_reg<<" not modifiable!";

    //------------- 3. Get initial value x1 for variable -----------//
    context_ref C1(M, c1);

    int x1 = C1.get_reg_value(*x_mod_reg).as_int();
    if (x1 < a or x1 > b)
        throw myexception()<<"discrete_uniform_avoid_mh: value "<<x1<<" not in range ["<<a<<", "<<b<<"]";

    //------------- 4. Propose new value avoiding x1 ---------------//
    int x2 = uniform(a,b-1);
    if (x2 >= x1) x2++;

    //------------- 5. Create a context with the new value----------//
    context C2 = C1;
    C2.set_reg_value(*x_mod_reg, expression_ref(x2));

    //------------- 6. Move if MH rule is satisfied ----------------//
    if (accept_MH(C1,C2,1.0))
        C1 = C2;

    return EPair(state+1,constructor("()",0));
}

template <typename T>
using Bounds = Box<bounds<T>>;

// slice_sample_real_random_variable x context state
extern "C" closure builtin_function_slice_sample_real_random_variable(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_to_reg(0);

    if (log_verbose >= 3) std::cerr<<"\n\n[slice_sample_real_random_variable] <"<<x_reg<<">\n";

    //------------- 1b. Get context index --------------
    int c1 = Args.evaluate(1).as_int();
    context_ref C1(M, c1);

    //------------- 1c. Get monad thread state ---------
    int state = Args.evaluate(2).as_int();

    //------------- 2. Find the location of the variable -------------//
    if (auto r = M.find_random_variable(x_reg))
        x_reg = *r;
    else
        throw myexception()<<"slice_sample_real_random_variable: reg "<<x_reg<<" is not a random variable!";

    //------------- 3. Get initial value x1 for variable -------------//
    auto bnds = M.get_range_for_random_variable(c1, x_reg);
    if (not bnds.is_a<Bounds<double>>())
        throw myexception()<<"random variable doesn't have a range that is bounds<double>";

    random_variable_slice_function logp(C1, bnds.as_<Bounds<double>>(), x_reg);

    // OK, this is a bit problematic.
    double w = 1.0;

    slice_sample(logp, w, 100);

    if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times.\n";

    return EPair(state+1,constructor("()",0));
}

// slice_sample_integer_random_variable x context state
extern "C" closure builtin_function_slice_sample_integer_random_variable(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());
    auto& M = Args.memory();

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_to_reg(0);

    if (log_verbose >= 3) std::cerr<<"\n\n[slice_sample_integer_random_variable] <"<<x_reg<<">\n";

    //------------- 1b. Get context index --------------
    int c1 = Args.evaluate(1).as_int();
    context_ref C1(M, c1);

    //------------- 1c. Get monad thread state ---------
    int state = Args.evaluate(2).as_int();

    //------------- 2. Find the location of the variable -------------//
    if (auto r = M.find_random_variable(x_reg))
        x_reg = *r;
    else
        throw myexception()<<"slice_sample_integer_random_variable: reg "<<x_reg<<" is not a random variable!";

    //------------- 3. Get initial value x1 for variable -------------//
    auto bnds = M.get_range_for_random_variable(c1, x_reg);
    if (not bnds.is_a<Bounds<int>>())
        throw myexception()<<"random variable doesn't have a range that is bounds<int>";

    integer_random_variable_slice_function logp(C1, bnds.as_<Bounds<int>>(), x_reg);

    // OK, this is a bit problematic.
    double w = 1.0;

    double v1 = logp.current_value() + uniform();
    slice_sample(v1, logp, w, 100);

    if (log_verbose >= 3) std::cerr<<"   - Posterior evaluated "<<logp.count<<" times.\n";

    return EPair(state+1,constructor("()",0));
}
