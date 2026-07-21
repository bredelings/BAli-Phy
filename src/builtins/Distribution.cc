#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <span>
#include <valarray>
#include <string>
#include <tuple>
#include <utility>
#include "builtins/native-vector-input.H"
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
#include <Eigen/Eigenvalues>
#include <Eigen/SVD>

#include <cmath>
#include <limits>

using std::vector;
using std::string;
using std::valarray;

namespace
{

// Derive Gaussian quadrature nodes and weights from a symmetric Jacobi matrix.
std::pair<DenseVector<double>, DenseVector<double>> quadrature_from_jacobi(const DenseMatrix<double>& jacobi)
{
    if (!jacobi.allFinite())
        throw myexception()<<"quadrature: Jacobi matrix contains a non-finite value";

    Eigen::SelfAdjointEigenSolver<DenseMatrix<double>> solver(jacobi);
    if (solver.info() != Eigen::Success)
        throw myexception()<<"quadrature: eigensolver failed";

    DenseVector<double> nodes = solver.eigenvalues();
    DenseVector<double> weights = solver.eigenvectors().row(0).array().square().transpose();
    if (!nodes.allFinite() || !weights.allFinite())
        throw myexception()<<"quadrature: eigensolver returned a non-finite rule";

    return {std::move(nodes), std::move(weights)};
}

// For Gamma(alpha, scale=1), factor*factor^T is the generalized-Laguerre Jacobi matrix. Its
// squared singular values and the first row of its left singular vectors therefore give the nodes
// and weights. Dividing the singular values by sqrt(alpha) changes the scale to 1/alpha, giving a
// unit-mean rule.
//
// As alpha becomes small, the normalized Jacobi matrix has one O(1) node beside count-1 nodes of
// O(1/alpha). Computing the unscaled factor's singular values avoids recovering that small node as
// an eigenvalue of a matrix dominated by the escaping nodes.
std::pair<DenseVector<double>, DenseVector<double>> gamma_quadrature_from_factor(double alpha, int count)
{
    DenseMatrix<double> factor = DenseMatrix<double>::Zero(count, count);
    for (int k = 0; k < count; k++)
        factor(k, k) = std::sqrt(alpha + k);
    for (int k = 1; k < count; k++)
        factor(k, k - 1) = std::sqrt(double(k));

    Eigen::JacobiSVD<DenseMatrix<double>> solver(factor, Eigen::ComputeFullU);
    if (solver.info() != Eigen::Success)
        throw myexception()<<"gammaQuadrature: singular value decomposition failed";

    DenseVector<double> nodes(count);
    DenseVector<double> weights(count);
    double sqrt_alpha = std::sqrt(alpha);
    for (int i = 0; i < count; i++)
    {
        int j = count - i - 1;
        double scaled_singular_value = solver.singularValues()[j] / sqrt_alpha;
        nodes[i] = scaled_singular_value * scaled_singular_value;
        weights[i] = solver.matrixU()(0, j) * solver.matrixU()(0, j);
    }
    return {std::move(nodes), std::move(weights)};
}

// Use the alpha -> 0 limit after alpha becomes too small to affect terms such as alpha+k in double
// precision. For X ~ Gamma(alpha, scale=1/alpha) and j >= 1, E[X^j] is asymptotic to
// (j-1)!/alpha^(j-1). Transforming a Gamma(2,1) rule (y_i,v_i) to nodes y_i/alpha and weights
// alpha*v_i/y_i^2 makes its j-th moment alpha^(1-j) sum_i v_i*y_i^(j-2). For j >= 2, Gaussian
// quadrature gives sum_i v_i*y_i^(j-2) = (j-1)! through the supported polynomial degree. The
// remaining finite node and weight make the zeroth and first moments exactly one.
std::pair<DenseVector<double>, DenseVector<double>> small_alpha_gamma_quadrature(double alpha, int count)
{
    DenseVector<double> nodes(count);
    DenseVector<double> weights(count);
    if (count == 1)
    {
        nodes[0] = 1.0;
        weights[0] = 1.0;
        return {std::move(nodes), std::move(weights)};
    }

    int escaping_count = count - 1;
    DenseMatrix<double> jacobi = DenseMatrix<double>::Zero(escaping_count, escaping_count);
    for (int k = 0; k < escaping_count; k++)
        jacobi(k, k) = 2.0 + 2.0 * k;
    for (int k = 1; k < escaping_count; k++)
        jacobi(k - 1, k) = jacobi(k, k - 1) = std::sqrt(double(k) * (k + 1));
    auto [gamma_two_nodes, gamma_two_weights] = quadrature_from_jacobi(jacobi);

    double escaping_weight = 0.0;
    double escaping_mean = 0.0;
    for (int i = 0; i < escaping_count; i++)
    {
        nodes[i + 1] = gamma_two_nodes[i] / alpha;
        weights[i + 1] = alpha * gamma_two_weights[i] / (gamma_two_nodes[i] * gamma_two_nodes[i]);
        escaping_weight += weights[i + 1];
        escaping_mean += gamma_two_weights[i] / gamma_two_nodes[i];
    }
    weights[0] = 1.0 - escaping_weight;
    nodes[0] = (1.0 - escaping_mean) / weights[0];
    return {std::move(nodes), std::move(weights)};
}

}

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

// Construct an n-point Gaussian quadrature rule for a unit-mean Gamma distribution.
extern "C" closure builtin_function_gammaQuadratureNative(OperationArgs& Args)
{
    double alpha = Args.evaluate_slot_to_value(0).as_double();
    int count = Args.evaluate_slot_to_value(1).as_int();
    if (count <= 0)
        throw myexception()<<"gammaQuadrature: the number of nodes must be positive";
    if (!(alpha > 0))
        throw math_error()<<"gammaQuadrature: alpha must be positive, but is "<<alpha;

    DenseVector<double> nodes;
    DenseVector<double> weights;
    if (std::isinf(alpha))
    {
        // Gamma(alpha, 1/alpha) converges to a point mass at one. Repeated nodes with equal weights
        // represent that point mass while preserving the requested number of components.
        nodes = DenseVector<double>::Ones(count);
        weights = DenseVector<double>::Constant(count, 1.0 / count);
    }
    else if (alpha <= std::numeric_limits<double>::epsilon())
        // At this scale the finite-alpha corrections used by the factorization round away.
        std::tie(nodes, weights) = small_alpha_gamma_quadrature(alpha, count);
    else if (alpha < 1.0)
        // The factorization resolves the finite node accurately among the escaping nodes.
        std::tie(nodes, weights) = gamma_quadrature_from_factor(alpha, count);
    else
    {
        // For alpha >= 1 the normalized Jacobi matrix is sufficiently well scaled for a direct
        // symmetric eigensolve.
        DenseMatrix<double> jacobi = DenseMatrix<double>::Zero(count, count);
        for (int k = 0; k < count; k++)
            jacobi(k, k) = 1.0 + 2.0 * k / alpha;
        for (int k = 1; k < count; k++)
        {
            double off_diagonal = std::sqrt(k / alpha) * std::sqrt(1.0 + (k - 1.0) / alpha);
            jacobi(k - 1, k) = jacobi(k, k - 1) = off_diagonal;
        }
        std::tie(nodes, weights) = quadrature_from_jacobi(jacobi);
    }
    for (int i = 0; i < count; i++)
    {
        if (!std::isfinite(nodes[i]) || nodes[i] < 0)
            throw math_error()<<"gammaQuadrature: node "<<i
                              <<" must be finite and nonnegative, but is "<<nodes[i];
        if (!std::isfinite(weights[i]) || weights[i] < 0)
            throw math_error()<<"gammaQuadrature: weight "<<i
                              <<" must be finite and nonnegative, but is "<<weights[i];
    }

    object_ptr<Box<DenseVector<double>>> node_result = new Box<DenseVector<double>>(std::move(nodes));
    object_ptr<Box<DenseVector<double>>> weight_result = new Box<DenseVector<double>>(std::move(weights));
    return R::RPair(node_result, weight_result);
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

// Construct an n-point Gaussian quadrature rule for a log-normal distribution.
extern "C" closure builtin_function_logNormalQuadratureNative(OperationArgs& Args)
{
    double log_mean = Args.evaluate_slot_to_value(0).as_double();
    double log_sigma = Args.evaluate_slot_to_value(1).as_double();
    int count = Args.evaluate_slot_to_value(2).as_int();
    if (count <= 0)
        throw myexception()<<"logNormalQuadrature: the number of nodes must be positive";
    if (!std::isfinite(log_mean))
        throw math_error()<<"logNormalQuadrature: logMean must be finite, but is "<<log_mean;
    if (!(log_sigma >= 0) || !std::isfinite(log_sigma))
        throw math_error()<<"logNormalQuadrature: logSigma must be finite and nonnegative, but is "<<log_sigma;

    DenseMatrix<double> jacobi = DenseMatrix<double>::Zero(count, count);
    for (int k = 1; k < count; k++)
    {
        double off_diagonal = std::sqrt(double(k));
        jacobi(k - 1, k) = jacobi(k, k - 1) = off_diagonal;
    }
    auto [nodes, weights] = quadrature_from_jacobi(jacobi);
    nodes.array() = (log_mean + log_sigma * nodes.array()).exp();
    for (int i = 0; i < count; i++)
        if (!std::isfinite(nodes[i]) || !(nodes[i] > 0))
            throw math_error()<<"logNormalQuadrature: node "<<i
                              <<" must be finite and positive, but is "<<nodes[i];

    object_ptr<Box<DenseVector<double>>> node_result = new Box<DenseVector<double>>(std::move(nodes));
    object_ptr<Box<DenseVector<double>>> weight_result = new Box<DenseVector<double>>(std::move(weights));
    return R::RPair(node_result, weight_result);
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

    return { ::negative_binomial_pdf(r,p,k) };
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
    auto probabilities =
        read_native_vector_input<double, ForeignDemand::use>(
            Args, 1, "multinomial probabilities");
    auto counts = read_native_vector_input<int, ForeignDemand::use>(
        Args, 4, "multinomial counts");

    if (probabilities.view().size() != counts.view().size())
        throw myexception()<<"multinomial_density: |ps| != |ks|";
    return { ::multinomial_pdf(n, probabilities.view(), counts.view()) };
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
    auto z = read_native_vector_input<int, ForeignDemand::use>(
        Args, 3, "Distribution.CRP_density");

    return { ::CRP_pdf(alpha,N,D,z.view()) };
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

// Read boxed probabilities through vector-contingent FORCE edges and sample
// without constructing intermediate Haskell lists or native Eigen vectors.
extern "C" closure builtin_function_sample_categorical(OperationArgs& Args)
{
    auto probabilities_arg = Args.evaluate_slot_use_with_contingency(0);
    int probabilities_reg = probabilities_arg.value_reg;
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
        int value_reg = Args.evaluate_reg_force(
            element_reg, probabilities_arg.edge_contingency);
        probabilities.push_back(
            Args.memory().closure_at(value_reg).get_code().as_double());
    }

    return { choose_scratch(probabilities) };
}
