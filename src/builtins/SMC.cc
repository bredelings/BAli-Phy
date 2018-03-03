#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

#include <vector>

#include "matrix.H"
#include "math/log-double.H"
#include "alignment/alignment.H"
#include "computation/computation.H"

#include <Eigen/Dense>
#include <unsupported/Eigen/MatrixFunctions>

using std::vector;
typedef Eigen::MatrixXd EMatrix;

double cdf(double eta, double t)
{
    return 1.0 - exp(-t*eta);
}

double quantile(double eta, double p)
{
    return -log1p(-p)/eta;
}

vector<double> get_bin_boundaries(int n, double eta)
{
    vector<double> b(n+1);
    for(int i=0;i<n;i++)
	b[i] = quantile(eta, double(i)/n);
    b[n] = 10000;
    return b;
}

vector<double> get_bin_centers(int n, double eta)
{
    vector<double> t(n);
    for(int i=0;i<n;i++)
	t[i] = quantile(eta, (2.0*i+1)/(2*n));
    return t;
}

vector<double> get_equilibrium(const vector<double>& B, double eta)
{
    int n_bins = B.size() - 1;
    vector<double> pi(n_bins);
    for(int i=0;i<n_bins-1;i++)
	pi[i] = cdf(eta,B[i+1])-cdf(eta,B[i]);
    pi[n_bins-1] = 1.0 - cdf(eta, B.back());

    // The equilibrium distribution should sum to 1.
    assert((sum(pi) - 1.0) < 1.0e-9);

    return pi;
}

// initially assume JC model?

Matrix JC_transition_p(double t)
{
    Matrix P(4,4);
    double a = (1-exp(-4*t/3))/4;

    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    if (i == j)
		P(i,j) = 1.0 - 3*a;
	    else
		P(i,j) = a;
	
    return P;
}

vector<Matrix> get_emission_probabilities(const vector<double>& t)
{
    vector<Matrix> E(t.size());
    for(int i=0;i<E.size();i++)
	E[i] = JC_transition_p(t[i]);
    return E;
}

// SMC model.
//
// State 0 = {{A,B},{A,B}}   = two chromosomes (initial state)
// State 1 = {{A,B},{A},{B}} = after recombining
// State 2 = {{A,B},{A}}     = locus B coalesces before locus A
//
// The matrix is [-rho  rho   0 ]    rho   [ -2  2  0]          [0   0  0]
//               [ 0   -eta  eta]  = --- * [  0  0  0]  + eta * [0  -1  1]
//               [ 0      0    0]     2    [  0  0  0]          [0   0  0]
//
//  On the substitution timescale we replace:
//    * rho/2 -> (rho/2) / (theta/2) == rho/theta
//    * eta   -> 1       / (theta/2) ==   2/theta

EMatrix smc_recombination()
{
    // [ -2 2 0 ]
    // [  0 0 0 ]
    // [  0 0 0 ]
    EMatrix R(3,3);
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++)
	    R(i,j) = 0;
    R(0,0) = -2;
    R(0,1) = 2;
    return R;
}

EMatrix smc_coalescence()
{
    // [ 0  0 0 ]
    // [ 1 -2 1 ]
    // [ 0  0 0 ]
    EMatrix C(3,3);
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++)
	    C(i,j) = 0;
    C(1,0) = 1;
    C(1,1) = -2;
    C(1,2) = 1;
    return C;
}

EMatrix smc_rates(double theta, double rho)
{
    double recombination_rate = rho/theta;
    double coalescence_rate = 2/theta;
    return recombination_rate * smc_recombination() + coalescence_rate * smc_coalescence();
}

// end SMC model

EMatrix smc_prime_recombination()
{
    // [ -2 2 0 ]
    // [  0 0 0 ]
    // [  0 0 0 ]
    EMatrix R(3,3);
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++)
	    R(i,j) = 0;
    R(0,0) = -2;
    R(0,1) = 2;
    return R;
}

// SMC' Model
//
// State 0 = {{A,B},{A,B}}   = two chromosomes (initial state)
// State 1 = {{A,B},{A},{B}} = after recombining
// State 2 = {{A,B},{A}}     = locus B coalesces before locus A
//
// The matrix is [-rho    rho   0 ]    rho   [ -2  2  0]          [0   0  0]
//               [ eta -2*eta  eta]  = --- * [  0  0  0]  + eta * [1  -2  1]
//               [ 0        0    0]     2    [  0  0  0]          [0   0  0]
//
//  On the substitution timescale we replace:
//    * rho/2 -> (rho/2) / (theta/2) == rho/theta
//    * eta   -> 1       / (theta/2) ==   2/theta

EMatrix smc_prime_coalescence()
{
    // [ 0  0 0 ]
    // [ 1 -2 1 ]
    // [ 0  0 0 ]
    EMatrix C(3,3);
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++)
	    C(i,j) = 0;
    C(1,0) = 1;
    C(1,1) = -2;
    C(1,2) = 1;
    return C;
}

EMatrix smc_prime_rates(double theta, double rho)
{
    double recombination_rate = rho/theta;
    double coalescence_rate = 2/theta;
    return recombination_rate * smc_prime_recombination() + coalescence_rate * smc_prime_coalescence();
}

// End SMC' model


// Finite Markov approximation: adds state 3 to smc_prime.
//
// State 0 = {{A,B},{A,B}}      = two chromosomes (initial state)
// State 1 = {{A,B},{A},{B}}    = after recombining
// State 2 = {{A},{B},{A},{B}}  = four chromosomes, after at least two recombinations
// State 3 = {{A,B},{A}}        = locus B coalesces before locus A  [ = State 2 in SMC and SMC' ]
//
// The matrix is [-rho    rho   0        0]    rho   [ -2  2  0  0]          [0  0  0  0]
//               [ eta -2*eta   rho/2  eta]  = --- * [  0 -1  1  0]  + eta * [1 -2  0  1]
//               [ 0    4*eta -5*eta   eta]     2    [  0  0  0  0]          [0  4 -5  1]
//               [ 0        0    0       0]          [  0  0  0  0]          [0  0  0  0]
//
//  On the substitution timescale we replace:
//    * rho/2 -> (rho/2) / (theta/2) == rho/theta
//    * eta   -> 1       / (theta/2) ==   2/theta

EMatrix finite_markov_recombination()
{
    // [ -2  2  0  0 ]
    // [  0 -1  1  0 ]
    // [  0  0  0  0 ]
    // [  0  0  0  0 ]
    EMatrix R(4,4);
    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    R(i,j) = 0;

    R(0,0) = -2;
    R(0,1) = 2;

    R(1,1) = -1;
    R(1,2) = 1;

    return R;
}

EMatrix finite_markov_coalescence()
{
    // [ 0  0  0  0 ]
    // [ 1 -2  0  1 ]
    // [ 0  4 -5  1 ]
    // [ 0  0  0  0 ]
    EMatrix C(4,4);
    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    C(i,j) = 0;
    C(1,0) = 1;
    C(1,1) = -2;
    C(1,3) = 1;

    C(2,1) = 4;
    C(2,2) = -5;
    C(2,3) = 1;

    return C;
}

EMatrix finite_markov_rates(double theta, double rho)
{
    double recombination_rate = rho/theta;
    double coalescence_rate = 2/theta;
    return recombination_rate * finite_markov_recombination() + coalescence_rate * finite_markov_coalescence();
}

Matrix get_transition_probabilities(const vector<double>& B, const vector<double>& T, double theta, double rho)
{
    const int n = T.size();
    assert(B.size() == n+1);

    auto Omega = finite_markov_rates(theta,rho);

    // exp(Omega*t) for bin boundaries
    vector<EMatrix> expB(n);
    for(int i=0;i<n;i++)
	expB[i] = (Omega*B[i]).exp();

    // exp(Omega*t) for bin centers
    vector<EMatrix> expT;
    for(auto t: T)
	expT.push_back((Omega*t).exp());

    // Coalescence rate
    double eta = 2.0/theta;

    Matrix P(n,n);
    for(int j=0;j<n; j++)
	for(int k=0;k<n; k++)
	{
	    if (k < j)
	    {
		P(j,k) = expB[k+1](0,3) - expB[k](0,3);
	    }
	    else if (k > j)
	    {
		assert(B[k] - T[j] > 0);
		P(j,k) = (expT[j](0,1) + expT[j](0,2)) * exp(-eta * (B[k] - T[j])) * (1.0 - exp(-eta*(B[k+1] - B[k])));
	    }
	    else if (k == j)
	    {
		// t = t_j
		double p = expT[j](0,0);
		// t \in [b_j, t_j)
		p += (expT[j](0,3) - expB[j](0,3));
		// t \in (t_j, b_j+1]
		p += (expT[j](0,1) + expT[j](0,2)) * (1.0 - exp(-eta*(B[j+1] - T[j])));
		P(j,k) = p;
	    }
	}
#ifndef NDEBUG
    for(int j=0;j<n; j++)
    {
	double total = 0;
	for(int k=0;k<n; k++)
	    total += P(j,k);
	assert(std::abs(1.0-total) < 1.0e-9);
    }

#endif
    return P;
}

constexpr double scale_factor = 115792089237316195423570985008687907853269984665640564039457584007913129639936e0;
constexpr double scale_min = 1.0/scale_factor;
constexpr double log_scale_min = -177.445678223345999210811423093293201427328034396225345054e0;

log_double_t smc(double theta, double rho, const alignment& A)
{
    assert(rho >= 0);
    assert(theta > 0);
    assert(A.n_sequences() == 2);

    // How many bins
    const int n_bins = 100;

    // Lower end of each bin. boundaries[0] = 0. The upper end of the last bin is \infty
    auto bin_boundaries = get_bin_boundaries(n_bins, 2.0/theta);

    auto bin_times = get_bin_centers(n_bins, 2.0/theta);

    auto emission_probabilities = get_emission_probabilities(bin_times);

    // # Compute the likelihoods for the first column
    auto pi = get_equilibrium(bin_boundaries, 2.0/theta);
    vector<double> L(n_bins);
    vector<double> L2(n_bins);
    int scale = 0;

    for(int i=0;i< n_bins; i++)
	L[i] = pi[i] * emission_probabilities[i](A(0,0), A(0,1));

    // # Iteratively compute likelihoods for remaining columns
    auto transition = get_transition_probabilities(bin_boundaries, bin_times, theta, rho);

    for(int l=1; l < A.length(); l++)
    {
	bool need_scale = true;
	for(int k=0; k < n_bins; k++)
	{
	    double temp = 1;
	    for(int j=0;j<n_bins; j++)
		temp += L[j] * transition(j,k);
	    temp *= emission_probabilities[k](A(l,0), A(l,1));
	    L2[k] = temp;

	    need_scale = need_scale and (temp < scale_min);
	}
	if (need_scale)
	{
	    scale++;
	    for(int k=0; k < n_bins; k++)
		L2[k] *= scale_factor;
	}
	std::swap(L, L2);
    }

    // # Compute the total likelihood
    log_double_t Pr = 1;
    for(int i=0; i < n_bins; i++)
	Pr += L[i];
    Pr.log() += log_scale_min * scale;
    return Pr;
}

extern "C" closure builtin_function_smc_density(OperationArgs& Args)
{
    double theta = Args.evaluate(0).as_double();

    double rho = Args.evaluate(1).as_double();

    auto a = Args.evaluate(2);
    auto& A = a.as_<alignment>();

    return { smc(theta, rho, A) };
}
