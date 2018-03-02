#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include "matrix.H"
#include "math/log-double.H"

#include <Eigen/Dense>

using std::vector;
typedef Eigen::MatrixXd EMatrix;

double cdf(double eta, double t)
{
    return eta * exp(-t*eta);
}

double quantile(double eta, double p)
{
    return -log1p(-p)/eta;
}

vector<double> get_bin_boundaries(int n, double eta)
{
    vector<double> b(n);
    for(int i=0;i<n;i++)
	b[i] = quantile(eta, double(i)/n);
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
    vector<double> pi(B.size());
    for(int i=0;i<B.size()-1;i++)
	pi[i] = cdf(eta,B[i+1])-cdf(eta,B[i]);
    pi.back() = 1.0 - cdf(eta, B.back());
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

EMatrix smc_rates(double rho, double theta)
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

EMatrix smc_prime_rates(double rho, double theta)
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
    EMatrix R(3,3);
    for(int i=0;i<3;i++)
	for(int j=0;j<3;j++)
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

EMatrix finite_markov_rates(double rho, double theta)
{
    double recombination_rate = rho/theta;
    double coalescence_rate = 2/theta;
    return recombination_rate * finite_markov_recombination() + coalescence_rate * finite_markov_coalescence();
}

Matrix get_transition_probabilities(const vector<double>& B, const vector<double>& T, double rho)
{
    Matrix Omega(4,4);

    assert(B.size() == T.size());
    const int n = B.size();
    Matrix P(n,n);
    for(int i=0;i<n; i++)
	for(int j=0;j<n; j++)
	{
	    P(i,j) = rho/n;
	}
    return P;
}

log_double_t smc(double rho, double theta, const matrix<int>& data)
{
    assert(rho >= 0);
    assert(theta > 0);
    assert(data.size1() == 2);

    // How many bins
    const int n_bins = 100;

    // Lower end of each bin. boundaries[0] = 0. The upper end of the last bin is \infty
    auto bin_boundaries = get_bin_boundaries(n_bins, 2.0/theta);

    auto bin_times = get_bin_centers(n_bins, 2.0/theta);

    auto emission_probabilities = get_emission_probabilities(bin_times);

    // # Compute the likelihoods for the first column
    auto pi = get_equilibrium(bin_boundaries, 2.0/theta);
    vector<log_double_t> L(n_bins);
    vector<log_double_t> L2(n_bins);
    for(int i=0;i< n_bins; i++)
	L[i] = pi[i] * emission_probabilities[i](data(0,0), data(1,0));

    // # Iteratively compute likelihoods for remaining columns
    auto transition = get_transition_probabilities(bin_boundaries, bin_times, rho);

    for(int l=1; l < data.size2(); l++)
    {
	for(int j=0; j < n_bins; j++)
	{
	    double Pr = 1;
	    for(int i=0;i<n_bins; i++)
		Pr += L[i] * transition(i,j);
	    L2[j] = Pr * emission_probabilities[j](data(0,l), data(1,l));
	}	    
	std::swap(L, L2);
    }

    // # Compute the total likelihood
    log_double_t Pr = 1;
    for(int i=0; i < n_bins; i++)
	Pr += L[i];
    return Pr;
}
