#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/context.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"
#include "probability/choose.H"
#include "computation/expression/constructor.H"

using boost::dynamic_pointer_cast;
using namespace std;

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
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    //------------- 1a. Get argument X -----------------
    int t_reg = Args.evaluate_slot_to_reg(0);
    if (auto t_mod_reg = M.find_modifiable_reg(t_reg))
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
        if (auto element_mod_reg = M.find_modifiable_reg(element_reg))
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
    
    return constructor("()",0);
}

// gibbs_sample_categorical x n pr
extern "C" closure builtin_function_gibbs_sample_categorical(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    //------------- 1a. Get argument X -----------------
    int x_reg = Args.evaluate_slot_to_reg(0);

    //------------- 1b. Get range [0,n) for X ----------
    int n_values = Args.evaluate(1).as_int();

    //------------- 1c. Get context index --------------
    int c1 = Args.evaluate(2).as_int();

    //------------- 2. Find the location of the variable -------------//
    auto& M = Args.memory();
    auto x_mod_reg = M.find_modifiable_reg(x_reg);
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

    return constructor("()",0);
}

extern "C" closure builtin_function_register_transition_kernel(OperationArgs& Args)
{
//    assert(not Args.evaluate_changeables());

    int R = Args.reg_for_slot(0);

    int state = Args.evaluate(1).as_int();

    auto& M = Args.memory();

    M.add_transition_kernel(R);

    return EPair(state+1,constructor("()",0));
}

