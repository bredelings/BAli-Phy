#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <span>
#include <valarray>
#include <string>
#include "builtins/native-vector-view.H"
#include "Vector.H"
#include "computation/operation.H"
#include "computation/machine/args.H"
#include "computation/module.H"
#include "probability/probability.H"
#include "util/bounds.H"
#include "util/rng.H"
#include "util/log-level.H"
#include "util/dense-matrix.H"
#include "util/myexception.H"

#include <boost/math/distributions.hpp>
#include <boost/math/special_functions/gamma.hpp>

using std::vector;
using std::string;
using std::valarray;

extern "C" closure builtin_function_gamma_density(OperationArgs& Args)
{
    double a1    = Args.evaluate_slot_to_value(0).as_double();
    double a2    = Args.evaluate_slot_to_value(1).as_double();
    double x     = Args.evaluate_slot_to_value(2).as_double();
  
    return { gamma_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_gamma(OperationArgs& Args)
{
    double a1    = Args.evaluate_slot_to_value_(0).as_double();
    double a2    = Args.evaluate_slot_to_value_(1).as_double();
  
    return { gamma(a1, a2) };
}

extern "C" closure builtin_function_gamma_quantile(OperationArgs& Args)
{
    double a1     = Args.evaluate_slot_to_value(0).as_double();
    double a2     = Args.evaluate_slot_to_value(1).as_double();
    double p      = Args.evaluate_slot_to_value(2).as_double();

    return { gamma_quantile(p, a1, a2) };
}

extern "C" closure builtin_function_gamma_cdf(OperationArgs& Args)
{
    double a      = Args.evaluate_slot_to_value(0).as_double();
    double b      = Args.evaluate_slot_to_value(1).as_double();
    double x      = Args.evaluate_slot_to_value(2).as_double();
    assert(a >= 0);
    assert(b >= 0);

    try
    {
        double p = boost::math::gamma_p(a, x/b);
        return {p};
    }
    catch(const std::exception& e)
    {
	if (log_verbose >= 2) std::cerr<<"Warning: gamma_cdf (x="<<x<<", a="<<a<<", b="<<b<<"), "<<e.what()<<std::endl;
        return { 0.0 };
    }
}

extern "C" closure builtin_function_beta_density(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
  
    return { beta_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_beta(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value_(0).as_double();
    double a2 = Args.evaluate_slot_to_value_(1).as_double();
  
    return { beta(a1, a2) };
}

extern "C" closure builtin_function_beta_cdf(OperationArgs& Args)
{
    double a = Args.evaluate_slot_to_value(0).as_double();
    double b = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
    assert(a >= 0);
    assert(b >= 0);

    using boost::math::beta_distribution;

    try
    {
        return { cdf(beta_distribution<>(a,b),x) };
    }
    catch(const std::exception& e)
    {
        if (log_verbose >= 2) std::cerr<<"Warning: beta_cdf (x="<<x<<", a="<<a<<", b="<<b<<"), "<<e.what()<<std::endl;
        return { 0.0 };
    }
}

extern "C" closure builtin_function_beta_quantile(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double p  = Args.evaluate_slot_to_value(2).as_double();
  
    return { beta_quantile(p, a1, a2) };
}

extern "C" closure builtin_function_normal_density(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
  
    return { normal_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_normal_cdf(OperationArgs& Args)
{
    double mu = Args.evaluate_slot_to_value(0).as_double();
    double sigma = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
    assert(sigma >= 0);

    using boost::math::normal_distribution;

    try
    {
        return { cdf(normal_distribution<>(mu,sigma),x) };
    }
    catch(const std::exception& e)
    {
        if (log_verbose >= 2) std::cerr<<"Warning: normal_cdf (x="<<x<<", mu="<<mu<<", sigma="<<sigma<<"), "<<e.what()<<std::endl;
        return { 0.0 };
    }
}

extern "C" closure builtin_function_sample_normal(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value_(0).as_double();
    double a2 = Args.evaluate_slot_to_value_(1).as_double();
  
    return { gaussian(a1, a2) };
}
 
extern "C" closure builtin_function_normal_quantile(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double p  = Args.evaluate_slot_to_value(2).as_double();

    return { normal_quantile(p, a1 ,a2) };
}

extern "C" closure builtin_function_cauchy_density(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();

    return { cauchy_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_cauchy(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value_(0).as_double();
    double a2 = Args.evaluate_slot_to_value_(1).as_double();

    return { cauchy(a1, a2) };
}

extern "C" closure builtin_function_cauchy_quantile(OperationArgs& Args)
{
    double m = Args.evaluate_slot_to_value(0).as_double();
    double s = Args.evaluate_slot_to_value(1).as_double();
    double p = Args.evaluate_slot_to_value(2).as_double();

    return { cauchy_quantile(p, m ,s) };
}

// First convert N from tuple to list...
// Second convert this builtin routine to just take two Vector<double> arguments.
// Third convert all value arguments here to "var" and use Distribution_Functions()
extern "C" closure builtin_function_dirichlet_density(OperationArgs& Args)
{
    auto n_value = Args.evaluate_slot_to_value(0);
    const auto& native_n = n_value.as_<Box<DenseVector<double>>>();
    auto x_value = Args.evaluate_slot_to_value(1);
    const auto& native_x = x_value.as_<Box<DenseVector<double>>>();

    // NOTE: probability.cc still accepts std::vector; remove these copies
    // when its Dirichlet kernel accepts a contiguous numeric view.
    vector<double> n(native_n.data(), native_n.data() + native_n.size());
    vector<double> x(native_x.data(), native_x.data() + native_x.size());
  
    return { ::dirichlet_pdf(x, n) };
}

extern "C" closure builtin_function_laplace_density(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
  
    return { ::laplace_pdf(x, a1, a2) };
}

extern "C" closure builtin_function_sample_laplace(OperationArgs& Args)
{
    double m = Args.evaluate_slot_to_value_(0).as_double();
    double s = Args.evaluate_slot_to_value_(1).as_double();
  
    return { laplace(m,s) };
}

extern "C" closure builtin_function_uniform_density(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value(0).as_double();
    double a2 = Args.evaluate_slot_to_value(1).as_double();
    double x  = Args.evaluate_slot_to_value(2).as_double();
  
    return { ::uniform_pdf(x,a1,a2) };
}

extern "C" closure builtin_function_sample_uniform(OperationArgs& Args)
{
    double a1 = Args.evaluate_slot_to_value_(0).as_double();
    double a2 = Args.evaluate_slot_to_value_(1).as_double();

    assert(a1 < a2);

    return { a1 + (a2-a1)*uniform() };
}

extern "C" closure builtin_function_uniform_int_density(OperationArgs& Args)
{
    int a1 = Args.evaluate_slot_to_value(0).as_int();
    int a2 = Args.evaluate_slot_to_value(1).as_int();
    double x  = Args.evaluate_slot_to_value(2).as_int();

    return { ::uniform_int_pdf(x,a1,a2) };
}

extern "C" closure builtin_function_sample_uniform_int(OperationArgs& Args)
{
    int a1 = Args.evaluate_slot_to_value_(0).as_int();
    int a2 = Args.evaluate_slot_to_value_(1).as_int();

    assert(a1 <= a2);

    int w = a2-a1+1;

    return { a1 + int(w*uniform()) };
}

extern "C" closure builtin_function_negative_binomial_density(OperationArgs& Args)
{
    int r = Args.evaluate_slot_to_value(0).as_int();
    double p = Args.evaluate_slot_to_value(1).as_double();
    int k = Args.evaluate_slot_to_value(2).as_int();

    return { ::binomial_pdf(r,p,k) };
}

extern "C" closure builtin_function_sample_negative_binomial(OperationArgs& Args)
{
    int r = Args.evaluate_slot_to_value_(0).as_int();
    double p = Args.evaluate_slot_to_value_(1).as_double();

    return { negative_binomial(r,p) };
}

extern "C" closure builtin_function_binomial_density(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value(0).as_int();
    double p = Args.evaluate_slot_to_value(1).as_double();
    int k = Args.evaluate_slot_to_value(2).as_int();
  
    return { ::binomial_pdf(n,p,k) };
}

extern "C" closure builtin_function_multinomial_density(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value(0).as_int();
    int ps_offset = Args.evaluate_slot_to_value(1).as_int();
    int ps_count = Args.evaluate_slot_to_value(2).as_int();
    auto ps_value = Args.evaluate_slot_to_value(3);
    const auto& ps_owner = ps_value.as_<Box<DenseVector<double>>>();
    auto ps = checked_native_vector_view(
        ps_owner, ps_offset, ps_count, "multinomial probabilities");

    int ks_offset = Args.evaluate_slot_to_value(4).as_int();
    int ks_count = Args.evaluate_slot_to_value(5).as_int();
    auto ks_value = Args.evaluate_slot_to_value(6);
    const auto& ks_owner = ks_value.as_<Box<DenseVector<int>>>();
    auto ks = checked_native_vector_view(
        ks_owner, ks_offset, ks_count, "multinomial counts");

    if (ps.size() != ks.size()) throw myexception()<<"multinomial_density: |ps| != |ks|";
    return { ::multinomial_pdf(n, ps, ks) };
}

extern "C" closure builtin_function_sample_binomial(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value_(0).as_int();
    double p = Args.evaluate_slot_to_value_(1).as_double();

    return { (int)binomial(n,p) };
}

extern "C" closure builtin_function_sample_bernoulli(OperationArgs& Args)
{
    double p = Args.evaluate_slot_to_value_(0).as_double();

    return { (int)bernoulli(p) };
}

extern "C" closure builtin_function_geometric_density(OperationArgs& Args)
{
    double p_fail = Args.evaluate_slot_to_value(0).as_double();
    double p_success = Args.evaluate_slot_to_value(1).as_double();
    int n = Args.evaluate_slot_to_value(2).as_int();
  
    return { ::geometric_pdf(p_fail, p_success, n) };
}

extern "C" closure builtin_function_sample_geometric(OperationArgs& Args)
{
    double p = Args.evaluate_slot_to_value_(0).as_double();

    return { (int)geometric(p) };
}

extern "C" closure builtin_function_poisson_density(OperationArgs& Args)
{
    double mu = Args.evaluate_slot_to_value(0).as_double();
    int n = Args.evaluate_slot_to_value(1).as_int();
  
    return { poisson_pdf(mu,n) };
}

extern "C" closure builtin_function_sample_poisson(OperationArgs& Args)
{
    double mu = Args.evaluate_slot_to_value_(0).as_double();

    return { (int)poisson(mu) };
}


log_double_t CRP_pdf(const double alpha, int N, int D, std::span<const int> z)
{
    if (z.size() != N) return 0.0;

    log_double_t Pr = 1;

    // 1. Determine probability of the unlabelled pattern
    vector<int> counts(N+D,0);
    int n_types = 0;
    for(int i=0;i<z.size();i++)
    {
	assert(z[i] >=0 and z[i] < N+D);
	int& count = counts[z[i]];
	if (count > 0)
	    Pr *= double(count)/(i+alpha);
	else
	{
	    if (i > 0)
		Pr *= (alpha/(i+alpha));
	    n_types++;
	}
	count++;
    }

    // 2. Determine the probability of the labelling
    for(int i=0;i<n_types;i++)
	Pr /= double(N+D-i);

    return Pr;
}

// This is the Chinese Restaurant Process density for N observations, N+Delta values, and parameter alpha.
// CRP(alpha,N,Delta)
// The final argument is z, which is a list of N integers.

extern "C" closure builtin_function_CRP_density(OperationArgs& Args)
{
    // ?? assert(not Args.evaluate_changeables());

    //------------- 1. Get arguments alpha, N, D -----------------
    double alpha = Args.evaluate_slot_to_value(0).as_double();
    int N = Args.evaluate_slot_to_value(1).as_int();
    int D = Args.evaluate_slot_to_value(2).as_int();

    //------------- 2. Get argument Z -----------------
    int offset = Args.evaluate_slot_to_value(3).as_int();
    int count = Args.evaluate_slot_to_value(4).as_int();
    auto owner_value = Args.evaluate_slot_to_value(5);
    const auto& owner = owner_value.as_<Box<DenseVector<int>>>();
    auto z = checked_native_vector_view(
        owner, offset, count, "Distribution.CRP_density");

    return { ::CRP_pdf(alpha,N,D,z) };
}

#include "probability/choose.H"

extern "C" closure builtin_function_sample_CRP(OperationArgs& Args)
{
    // ?? assert(not Args.evaluate_changeables());

    //------------- 1. Get arguments alpha, N, D -----------------
    double alpha = Args.evaluate_slot_to_value_(0).as_double();
    int N = Args.evaluate_slot_to_value_(1).as_int();
    int D = Args.evaluate_slot_to_value_(2).as_int();

    // The entries in [0,n_seen) are the categories we've seen
    vector<int> categories = iota(N+D);

    // These are the counts of the seen categories, followed by alpha
    vector<double> counts = {alpha};

    // The number of categories we've seen so far.
    int n_seen=0;

    // The series of sampled categories
    object_ptr<Box<DenseVector<int>>> S(new Box<DenseVector<int>>(N));

    for(int i=0;i<N;i++)
    {
	// Choose from the seen categories, or the possibilty we should pick a new category
	int index = choose(counts);

	// If we've chosen a category not seen before..
	if (index == n_seen)
	{
	    // .. then select an unsend category.
	    int a_index = n_seen + uniform_int(0,N+D-n_seen-1);

	    // Put the chosen category next in our list of seen categories
	    if (n_seen < a_index)
		std::swap(categories[n_seen], categories[a_index]);
	    n_seen++;

	    // Give the category 0 counts, and mark the next unseen one with weight alpha
	    counts.back() = 0.0;
	    counts.push_back(alpha);
	}

	assert(index < n_seen);

	// The index we've chosen is now valid.
	counts[index] += 1.0;
	(*S)(i) = categories[index];
    }

    return S;
}

// Read boxed probabilities through dependent FORCE edges and sample without
// constructing intermediate Haskell lists or native Eigen vectors.
extern "C" closure builtin_function_sample_categorical(OperationArgs& Args)
{
    int probabilities_reg = Args.evaluate_slot_force(0);
    std::size_t count = boxed_vector_element_regs(
        Args.memory().closure_at(probabilities_reg)).size();
    if (count == 0)
        throw myexception()<<"Probability.Distribution.Categorical: cannot sample "
                           <<"from an empty probability vector";

    // NOTE: choose_scratch overwrites its input with cumulative sums; remove
    // this mutable buffer when it accepts separate probabilities and scratch.
    vector<double> probabilities;
    probabilities.reserve(count);
    for(std::size_t i = 0; i < count; i++)
    {
        // Copy the register before evaluation, which may grow the machine heap
        // and invalidate the environment reference used to discover it.
        int element_reg = boxed_vector_element_regs(
            Args.memory().closure_at(probabilities_reg))[i];
        int value_reg = Args.evaluate_reg_dependent_force(element_reg);
        probabilities.push_back(
            Args.memory().closure_at(value_reg).get_code().as_double());
    }

    return { choose_scratch(probabilities) };
}
