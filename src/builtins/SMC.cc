#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

#include <vector>

#include "math/pow2.H"
#include "math/logprod.H"
#include "util/matrix.H"
#include "util/log-level.H"
#include "util/range.H"
#include "util/math/log-double.H"
#include "util/assert.hh"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "probability/choose.H"
#include "probability/probability.H"
#include "computation/machine/args.H"
#include "computation/expression/constructor.H"
#include "computation/machine/graph_register.H"
#include "computation/context.H"
#include "computation/param.H"

#include <Eigen/Dense>
#include <unsupported/Eigen/MatrixFunctions>

inline int ipow(int i, int n)
{
    assert(n >= 0);
    if (n==0) return 1;
    if (n==1) return i;
    int n1 = n/2;
    int n2 = n-n1;
    return ipow(i,n1) * ipow(i,n2);
}

using std::vector;
using std::pair;
typedef Eigen::MatrixXd EMatrix;

double cdf(double eta, double t)
{
    assert(eta > 0);
    assert(t >= 0);
    return 1.0 - exp(-t*eta);
}

double quantile(double eta, double p)
{
    assert(0 <= p and p <= 1);
    assert(eta > 0);
    return -log1p(-p)/eta;
}

double reverse_quantile(double eta, double p)
{
    return quantile(eta,1.0 - p);
}

struct demography
{
    vector<double> coalescent_rates;
    vector<double> level_boundaries;

    EMatrix Pr_X_at(double t, double rho_over_theta);

    demography(const vector<double>& c, const vector<double>& l)
	:coalescent_rates(c),
	 level_boundaries(l)
	{
	    assert(coalescent_rates.size() == level_boundaries.size());
	}
};


vector<double> get_quantiles(const vector<double>& P, const vector<double>& coalescent_rates, const vector<double>& level_boundaries)
{
    assert(coalescent_rates.size() == level_boundaries.size());
    assert(level_boundaries[0] == 0.0);

    vector<double> quantiles(P.size());
    int level = 0;
    double t = 0;
    double q1 = 1.0;
    for(int i = 0; i < P.size(); i++)
    {
	double q2 = 1.0 - P[i];

        // We have that Pr(X > t = q1)
        // We are trying to find the t2 such that Pr(X > t2 = q2)

	// Pr(X > t1) = q1
	// Pr(X > t2) = q2
	// Pr(X > t2|X>t1) = Pr(X > t2)/Pr(X > t1) = q2/q1
	
	for(;;level++)
	{
	    assert(level < level_boundaries.size());

	    double t2 = t + reverse_quantile(coalescent_rates[level], q2/q1);

	    // If t+delta_t is within the current level then we are done.
	    if (level+1 >= level_boundaries.size() or t2 < level_boundaries[level+1])
	    {
		quantiles[i] = t2;
		break;
	    }
	    // If t+delta_t is not within the current level, then we need to continue to the next level.
	    else
	    {
		assert(level + 1 < level_boundaries.size());
		assert(level_boundaries[level + 1] > t);

		// Pr(X > t1 + delta) = Pr (X > t) * Pr(X > t1 + delta| X > t1)

		q1 *= 1.0 - cdf(coalescent_rates[level], level_boundaries[level+1] - t);
		t   = level_boundaries[level + 1];
	    }
	}

	// Reset (t1,p1) for the next loop iteration
	t = quantiles[i];
	q1 = 1.0 - P[i];
    }

    return quantiles;
}

vector<double> get_equilibrium(const vector<double>& beta)
{
    int n_bins = beta.size() - 1;
    vector<double> pi(n_bins);

    for(int i=0; i<pi.size(); i++)
	pi[i] = beta[i+1] - beta[i];

    // The equilibrium distribution should sum to 1.
    assert(std::abs(sum(pi) - 1.0) < 1.0e-9);

    return pi;
}

// initially assume JC model?

EMatrix JC_transition_p(double t)
{
    EMatrix P(4,4);
    double a = (1-exp(-4*t/3))/4;

    for(int i=0;i<4;i++)
	for(int j=0;j<4;j++)
	    if (i == j)
		P(i,j) = 1.0 - 3*a;
	    else
		P(i,j) = a;
	
    return P;
}

EMatrix error_matrix(double error_rate)
{
    EMatrix E(4,4);
    for(int i=0;i<4;i++)
        for(int j=0;j<4;j++)
            if (i==j)
                E(i,j) = 1.0 - error_rate;
            else
                E(i,j) = error_rate/3.0;
    return E;
}

// We need emission probabilities for 2*t, since t is the depth of the tree,
// but the distance between the tips is 2*t.

// So... this should be 0.25*JC_transition_p( ), but maybe leaving out the
// 0.25 doesn't hurt, since its a constant factor.

vector<EMatrix> get_emission_probabilities(const vector<double>& t, double error_rate)
{
    vector<EMatrix> emission(t.size(),EMatrix(4,4));
    auto error = error_matrix(error_rate);
    for(int i=0;i<t.size();i++)
    {
        emission[i] = error.transpose() * JC_transition_p(2.0 * t[i]) * error;
    }
    return emission;
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

EMatrix finite_markov_rates(double rho_over_theta, double coalescence_rate)
{
    double recombination_rate = rho_over_theta;
    return recombination_rate * finite_markov_recombination() + coalescence_rate * finite_markov_coalescence();
}

EMatrix demography::Pr_X_at(double t, double rho_over_theta)
{
    // Start off with the identity
    EMatrix Pr_X = (finite_markov_rates(rho_over_theta, coalescent_rates[0])*0.0).exp();

    for(int l=0;l<level_boundaries.size() and level_boundaries[l] <= t;l++)
    {
	double t2 = t;

	// Only go to the end of the level with this rate matrix
	if (l+1 < level_boundaries.size() and level_boundaries[l+1] < t)
	    t2 = level_boundaries[l+1];

	double dt = t2-level_boundaries[l];

	EMatrix Omega = finite_markov_rates(rho_over_theta, coalescent_rates[l]);
	Pr_X = (Omega*dt).exp() * Pr_X;
    }

    return Pr_X;
}

Matrix get_transition_probabilities(const vector<double>& B, const vector<double>& T, const vector<double>& beta, const vector<double>& alpha,
				    const vector<double>& coalescent_rates, const vector<double>& level_boundaries, double rho_over_theta)
{
    assert(level_boundaries.size() >= 1);
    assert(level_boundaries[0] == 0.0);

    assert(rho_over_theta >= 0);

    assert(coalescent_rates.size() > 0);

    assert(rho_over_theta >= 0);

    const int n = T.size();
    assert(B.size() == n+1);

    demography demo(coalescent_rates, level_boundaries);

    // exp(Omega*t) for bin boundaries
    vector<EMatrix> Pr_X_at_B(n);
    for(int i=0;i<n;i++)
	Pr_X_at_B[i] = demo.Pr_X_at(B[i], rho_over_theta);

    // exp(Omega*t) for bin centers
    vector<EMatrix> Pr_X_at_T(n);
    for(int i=0;i<n;i++)
	Pr_X_at_T[i] = demo.Pr_X_at(T[i], rho_over_theta);

    Matrix P(n,n);
    for(int j=0;j<n; j++)
	for(int k=0;k<n; k++)
	{
	    if (k < j)
	    {
		P(j,k) = Pr_X_at_B[k+1](0,3) - Pr_X_at_B[k](0,3);
	    }
	    else if (k > j)
	    {
		assert(beta[k+1] >= beta[k]);
		P(j,k) = (Pr_X_at_T[j](0,1) + Pr_X_at_T[j](0,2)) * (beta[k+1] - beta[k])/(1.0-alpha[j]);
	    }
	    else if (k == j)
	    {
		// t = t_j
		double p = Pr_X_at_T[j](0,0);
		// t \in [b_j, t_j)
		p += (Pr_X_at_T[j](0,3) - Pr_X_at_B[j](0,3));
		// t \in (t_j, b_j+1]
		p += (Pr_X_at_T[j](0,1) + Pr_X_at_T[j](0,2)) * (beta[j+1] - alpha[j])/(1.0-alpha[j]);
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

EMatrix get_no_snp_matrix(const Matrix& T, const vector<EMatrix>& emission)
{
    // This is only valid for Jukes-Cantor
    int n_bins = T.size1();
    EMatrix M(n_bins,n_bins);
    for(int j=0;j<n_bins;j++)
	for(int k=0; k<n_bins; k++)
	    M(j,k) = T(j,k) * emission[k](0,0);
    return M;
}

EMatrix get_snp_matrix(const Matrix& T, const vector<EMatrix>& emission)
{
    // This is only valid for Jukes-Cantor
    int n_bins = T.size1();
    EMatrix M(n_bins,n_bins);
    for(int j=0;j<n_bins;j++)
	for(int k=0; k<n_bins; k++)
	    M(j,k) = T(j,k) * emission[k](0,1);
    return M;
}

EMatrix get_missing_matrix(const Matrix& T)
{
    // This is only valid for Jukes-Cantor
    int n_bins = T.size1();
    EMatrix M(n_bins,n_bins);
    for(int j=0;j<n_bins;j++)
	for(int k=0; k<n_bins; k++)
	    M(j,k) = T(j,k);
    return M;
}

constexpr double scale_factor = 115792089237316195423570985008687907853269984665640564039457584007913129639936e0;
constexpr double scale_min = 1.0/scale_factor;
constexpr double log_scale_min = -177.445678223345999210811423093293201427328034396225345054e0;


enum class site_t {unknown=0,poly,mono,missing,empty};

inline site_t classify_site(int x1, int x2)
{
    if (x1 == -1 and x2 == -1)
	return site_t::empty;
    else if (x1 < 0 or x2< 0)
	return site_t::missing;
    else if (x1 == x2)
	return site_t::mono;
    else
	return site_t::poly;
}

vector<double> get_column(const Matrix& M, int i)
{
    vector<double> v(M.size2());
    for(int j=0;j<v.size();j++)
        v[j] = M(i,j);
    return v;
}

void rescale(vector<double>& L, int& scale)
{
    const int n_bins = L.size();

    // Check if we need to scale the likelihoods
    bool need_scale = true;
    for(int k=0; k < n_bins; k++)
    {
	need_scale = need_scale and (L[k] < scale_min);
	assert(0 <= L[k] and L[k] <= 1.0);
    }

    // Scale likelihoods if necessary
    if (need_scale)
    {
	scale++;
	for(int k=0; k < n_bins; k++)
	    L[k] *= scale_factor;
    }
}

void rescale(Matrix& M, int i, int& scale)
{
    const int n_bins = M.size2();

    // Check if we need to scale the likelihoods
    bool need_scale = true;
    for(int k=0; k < n_bins; k++)
    {
	need_scale = need_scale and (M(i,k) < scale_min);
	assert(0 <= M(i,k) and M(i,k) <= 1.0);
    }

    // Scale likelihoods if necessary
    if (need_scale)
    {
	scale++;
	for(int k=0; k < n_bins; k++)
	    M(i,k) *= scale_factor;
    }
}

double sum_last(const Matrix& M)
{
    const int n_bins = M.size2();
    const int c = int(M.size1())-1;
    double sum = 0;
    for(int k=0; k < n_bins; k++)
        sum += M(c,k);
    return sum;
}

bool too_small(const EMatrix& M)
{
    for(int j=0;j<M.rows();j++)
    {
	double max_k = 0;
	for(int k=0;k<M.cols();k++)
	    max_k = std::max(max_k,M(j,k));
	if (max_k < scale_min)
	    return true;
    }
    return false;
}

EMatrix square(const EMatrix& M)
{
    return M*M;
}

int silly_log_2(int i)
{
    assert(i > 0);
    int count = 0;
    while (i>>1)
    {
	i>>=1;
	count++;
    };
    return count;
}

int silly_power_2(int i)
{
    return (1<<i);
}

vector<EMatrix> matrix_binary_power(const EMatrix& M, int L)
{
    vector<EMatrix> P {M};

    do {
	P.push_back(square(P.back()));
	if (too_small(P.back()))
	{
	    P.pop_back();
	    break;
	}
    } while(std::pow(2,P.size()) < L);

    return P;
}

vector<pair<int,site_t>> classify_sites(const alignment& A)
{
    vector<pair<int,site_t>> sites;
    for(int l=1; l < A.length();)
    {
	site_t s = classify_site(A(l,0), A(l,1));
	if (s == site_t::empty)
	{
	    l++;
	    continue;
	}
	int count = 0;

	do
	{
	    l++;
	    count++;
	}
	while(l < A.length() and classify_site(A(l,0),A(l,1)) == s);

	sites.push_back({count,s});
    }
    return sites;
}

void smc_group(vector<double>& L, vector<double>& L2, int& scale, const vector<EMatrix>& matrices, int length)
{
    const int n_bins = L.size();

    for(int i=0;i<length;)
    {
	int left = length - i;
	int x = silly_log_2(left);
	x = std::min<int>(x, matrices.size()-1);
	int taking = silly_power_2(x);

	auto& M = matrices[x];
	for(int k=0; k < n_bins; k++)
	{
	    double temp = 0;
	    for(int j=0;j<n_bins; j++)
		temp += L[j] * M(j,k);

	    assert(temp > -1.0e-9);
	    L2[k] = std::max(temp, 0.0);
	}
	i += taking;

	// Scale likelihoods if necessary
	rescale(L2, scale);

	// Swap current & next likelihoods
	std::swap(L, L2);
    }
}

vector<pair<double,int>> compress_states(const vector<int>& states, const vector<double>&t)
{
    int last_state = -1;
    vector<pair<double,int>> state_regions;
    for(int i=0;i<states.size();i++)
    {
        if (states[i] != last_state)
        {
            last_state = states[i];
            assert(0 <= last_state and last_state < t.size());
            state_regions.push_back({t[last_state],1});
        }
        else
            state_regions.back().second++;
    }
    return state_regions;
}


// Output formats for argweaver are specified here:  http://mdrasmus.github.io/argweaver/doc/
//
// 1. Ancestral recombination graph format: (*.arg)
//
// Also, they can be generated by e.g. `arg-sim -k 8 -L 100000 -N 10000 -r 1.6e-8 -m 1.8e-8 -o test1/test1`
//
// Example: (tab-delimited)
// ------------------- arg format ----------------
// start=0	end=10000
// name	event	age	pos	parents	children
// 1	coal	395	0	12	n0,n2
// 2	coal	5363	0	9	17,19
// 3	coal	26970	0	8	5,11
// 4	coal	60155	0		7,8
// 5	recomb	18044	246	3,10	13
// 6	coal	18044	0	7	9,10
// ...
// 26	coal	40287	0	8	14,25
// n0	gene	0	0	1
// n1	gene	0	0	11
// n2	gene	0	0	1
// n3	gene	0	0	17
// n4	gene	0	0	19
// ---------------------------------------------
//
// 2. SMC format
//
// Example: (tab-delimited)
// -------------------------- SMC format ------------------------------
// NAMES	n0	n1	n6	n3	n4	n7	n5	n2
// REGION	chr	1	100000
// TREE	1	740	((6:1002.805899[&&NHX:age=0.000000],4:1002.805899[&&NHX:age=0.000000])12:17041.819563[&&NHX:age=1002.805899],(2:18044.625462[&&NHX:age=0.000000],((5:5363.846221[&&NHX:age=0.000000],(7:122.586947[&&NHX:age=0.000000],1:122.586947[&&NHX:age=0.000000])14:5241.259274[&&NHX:age=122.586947])10:6697.962294[&&NHX:age=5363.846221],(3:1545.314509[&&NHX:age=0.000000],0:1545.314509[&&NHX:age=0.000000])11:10516.494006[&&NHX:age=1545.314509])9:5982.816948[&&NHX:age=12061.808515])13:0.000000[&&NHX:age=18044.625462])8[&&NHX:age=18044.625462];
// SPR	740	13	18044.625462	8	26970.598323
// TREE	741	840	((6:1002.805899[&&NHX:age=0.000000],4:1002.805899[&&NHX:age=0.000000])12:25967.792423[&&NHX:age=1002.805899],(2:18044.625462[&&NHX:age=0.000000],((5:5363.846221[&&NHX:age=0.000000],(7:122.586947[&&NHX:age=0.000000],1:122.586947[&&NHX:age=0.000000])14:5241.259274[&&NHX:age=122.586947])10:6697.962294[&&NHX:age=5363.846221],(3:1545.314509[&&NHX:age=0.000000],0:1545.314509[&&NHX:age=0.000000])11:10516.494006[&&NHX:age=1545.314509])9:5982.816948[&&NHX:age=12061.808515])13:8925.972860[&&NHX:age=18044.625462])8[&&NHX:age=26970.598323];
// SPR	840	14	5363.846221	12	18044.625462
// TREE	841	1460	(((6:1002.805899[&&NHX:age=0.000000],4:1002.805899[&&NHX:age=0.000000])12:17041.819563[&&NHX:age=1002.805899],(7:122.586947[&&NHX:age=0.000000],1:122.586947[&&NHX:age=0.000000])14:17922.038515[&&NHX:age=122.586947])10:8925.972860[&&NHX:age=18044.625462],(2:18044.625462[&&NHX:age=0.000000],(5:12061.808515[&&NHX:age=0.000000],(3:1545.314509[&&NHX:age=0.000000],0:1545.314509[&&NHX:age=0.000000])11:10516.494006[&&NHX:age=1545.314509])9:5982.816948[&&NHX:age=12061.808515])13:8925.972860[&&NHX:age=18044.625462])8[&&NHX:age=26970.598323];
// SPR	1460	5	8051.702368	11	8051.702368
// TREE	1461	2880	(((6:1002.805899[&&NHX:age=0.000000],4:1002.805899[&&NHX:age=0.000000])12:17041.819563[&&NHX:age=1002.805899],(7:122.586947[&&NHX:age=0.000000],1:122.586947[&&NHX:age=0.000000])14:17922.038515[&&NHX:age=122.586947])10:8925.972860[&&NHX:age=18044.625462],(2:18044.625462[&&NHX:age=0.000000],(5:8051.702368[&&NHX:age=0.000000],(3:1545.314509[&&NHX:age=0.000000],0:1545.314509[&&NHX:age=0.000000])11:6506.387859[&&NHX:age=1545.314509])9:9992.923095[&&NHX:age=8051.702368])13:8925.972860[&&NHX:age=18044.625462])8[&&NHX:age=26970.598323];
// ---------------------------------------------------------------------
// 


typedef double smc_tree;

vector<pair<smc_tree,int>> smc_trace(double rho_over_theta, vector<double> coalescent_rates, vector<double> level_boundaries, double error_rate, const alignment& A)
{
    assert(level_boundaries.size() >= 1);
    assert(level_boundaries[0] == 0.0);

    assert(rho_over_theta >= 0);

    assert(coalescent_rates.size() > 0);

    assert(A.n_sequences() == 2);

    // How many bins
    const int n_bins = 100;

    vector<double> alpha(n_bins); /// Pr (T < t[j])
    vector<double> beta(n_bins);  /// Pr (T < b[k])
    for(int i=0;i<n_bins;i++)
    {
	beta[i] = double(i)/n_bins;
	alpha[i] = (2.0*i+1)/(2*n_bins);
    }

    auto bin_boundaries = get_quantiles(beta, coalescent_rates, level_boundaries);
    bin_boundaries.push_back(bin_boundaries.back() + 1000000 );
    beta.push_back(1.0);

    const auto bin_times = get_quantiles(alpha, coalescent_rates, level_boundaries);

    const auto emission_probabilities = get_emission_probabilities(bin_times, error_rate);

    // This assumes equally-spaced bin quantiles.
    const auto pi = get_equilibrium(beta);

    const auto transition = get_transition_probabilities(bin_boundaries, bin_times, beta, alpha, coalescent_rates, level_boundaries, rho_over_theta);

    auto no_snp = get_no_snp_matrix(transition, emission_probabilities);

    auto missing = get_missing_matrix(transition);

    auto snp = get_snp_matrix(transition, emission_probabilities);

    Matrix L(A.length()+1, n_bins);
    for(int i=0;i<n_bins;i++)
        L(0,i) = pi[i];

    // # Iteratively compute likelihoods for remaining columns
    int scale = 0;
    for(int i=0;i<A.length();i++)
    {
        auto type = classify_site(A(i,0),A(i,1));

        const EMatrix* M = nullptr;
        if (type == site_t::missing)
            M = &missing;
	else if (type == site_t::mono)
            M = &no_snp;
	else if (type == site_t::poly)
            M = &snp;
	else
	    std::abort();

        for(int k=0;k<n_bins;k++)
        {
            double temp = 0;
            for(int j=0;j<n_bins;j++)
                temp += L(i,j) * (*M)(j,k);

            assert(temp > -1.0e-9);
            L(i+1,k) = std::max(temp, 0.0);
        }

        rescale(L, i+1, scale);
    }

    // # Compute the total likelihood
    log_double_t Pr (sum_last(L));
    Pr.log() += log_scale_min * scale;

    vector<int> states;
    vector<double> pr = get_column(L, L.size1()-1);
    states.push_back(choose(pr));

    for(int i=A.length()-2;i>=0;i--)
    {
        int s2 = states.back();
        vector<double> pr = get_column(L,i);
        for(int s1=0;s1<pr.size();s1++)
            pr[s1] *= transition(s1,s2);
        states.push_back(choose(pr));
    }

    std::reverse(states.begin(), states.end());

    return compress_states(states, bin_times);
}

log_double_t smc(double rho_over_theta, vector<double> coalescent_rates, vector<double> level_boundaries, double error_rate, const alignment& A)
{
    assert(level_boundaries.size() >= 1);
    assert(level_boundaries[0] == 0.0);

    assert(rho_over_theta >= 0);

    assert(coalescent_rates.size() > 0);

    assert(A.n_sequences() == 2);

    // How many bins
    const int n_bins = 100;

    vector<double> alpha(n_bins); /// Pr (T < t[j])
    vector<double> beta(n_bins);  /// Pr (T < b[k])
    for(int i=0;i<n_bins;i++)
    {
	beta[i] = double(i)/n_bins;
	alpha[i] = (2.0*i+1)/(2*n_bins);
    }

    auto bin_boundaries = get_quantiles(beta, coalescent_rates, level_boundaries);
    bin_boundaries.push_back(bin_boundaries.back() + 1000000 );
    beta.push_back(1.0);

    const auto bin_times = get_quantiles(alpha, coalescent_rates, level_boundaries);

    const auto emission_probabilities = get_emission_probabilities(bin_times, error_rate);

    // This assumes equally-spaced bin quantiles.
    const auto pi = get_equilibrium(beta);

    vector<double> L(n_bins);
    vector<double> L2(n_bins);
    int scale = 0;

    // FIXME: I think we should be able to start at site -1 with L[i] = pi[i], if pi[i] is the equilibrium of T(j,k)
    //        Its not exactly the equilibrium, so this is approximate, I guess.
    for(int i=0;i< n_bins; i++)
    {
	L[i] = pi[i];
//	if (A(0,0) >= 0 and A(0,1) >= 0)
//	    L[i] *= emission_probabilities[i](A(0,0), A(0,1));
    }

    // # Iteratively compute likelihoods for remaining columns
    const auto transition = get_transition_probabilities(bin_boundaries, bin_times, beta, alpha, coalescent_rates, level_boundaries, rho_over_theta);

    vector<EMatrix> no_snp = matrix_binary_power(get_no_snp_matrix(transition, emission_probabilities), A.length());

    vector<EMatrix> missing = matrix_binary_power(get_missing_matrix(transition), A.length());

    vector<EMatrix> snp = matrix_binary_power(get_snp_matrix(transition, emission_probabilities), A.length());

    for(auto& [count,type]: classify_sites(A))
    {
	if (type == site_t::missing)
	    smc_group(L, L2, scale, missing, count);
	else if (type == site_t::mono)
	    smc_group(L, L2, scale, no_snp, count);
	else if (type == site_t::poly)
	    smc_group(L, L2, scale, snp, count);
	else
	    std::abort();
    }

    // # Compute the total likelihood
    log_double_t Pr (sum(L));
    Pr.log() += log_scale_min * scale;
    return Pr;
}

extern "C" closure builtin_function_smc_density(OperationArgs& Args)
{
    double rho_over_theta = Args.evaluate(0).as_double();

    auto thetas = (vector<double>)Args.evaluate(1).as_<EVector>();

    auto level_boundaries = (vector<double>)Args.evaluate(2).as_<EVector>();

    // Perhaps we should make this different, depending on whether a sequence matches the reference.
    double error_rate = Args.evaluate(3).as_double();

    vector<double> coalescent_rates;
    for(auto theta: thetas)
	coalescent_rates.push_back(2.0/theta);

    auto a = Args.evaluate(4);
    auto& A = a.as_<Box<alignment>>().value();

    return { smc(rho_over_theta, coalescent_rates, level_boundaries, error_rate, A) };
}

extern "C" closure builtin_function_smc_trace(OperationArgs& Args)
{
    double rho_over_theta = Args.evaluate(0).as_double();

    auto thetas = (vector<double>)Args.evaluate(1).as_<EVector>();

    auto level_boundaries = (vector<double>)Args.evaluate(2).as_<EVector>();

    // Perhaps we should make this different, depending on whether a sequence matches the reference.
    double error_rate = Args.evaluate(3).as_double();

    vector<double> coalescent_rates;
    for(auto theta: thetas)
	coalescent_rates.push_back(2.0/theta);

    auto a = Args.evaluate(4);
    auto& A = a.as_<Box<alignment>>().value();

    auto compressed_states = smc_trace(rho_over_theta, coalescent_rates, level_boundaries, error_rate, A);

    EVector ecs;
    for(auto& [h,l]: compressed_states)
        ecs.push_back(EPair(h,l));

    return { ecs };
}

extern "C" closure builtin_function_trace_to_trees(OperationArgs& Args)
{
    auto trace = Args.evaluate(0).as_<EVector>();

    std::ostringstream s;

    for(auto& epair: trace)
    {
        auto height =  epair.as_<EPair>().first.as_double();
        auto length =  epair.as_<EPair>().second.as_int();
        s<<"["<<length<<"](1:"<<height<<",2:"<<height<<");";
    }

    return { String(s.str()) };
}

double li_stephens_2003_theta(int n)
{
    double inv_theta = 0;
    for(int i=1;i<=n;i++)
        inv_theta += 1.0/n;
    double theta = 1.0/inv_theta;
    return theta;
}

double emission_probability(int copied_letter, int emitted_letter, double emission_diff_state, double emission_same_state, bool all_previous_missing)
{
    // Emission is 1.0 if missing data at i2
    if (emitted_letter < 0)
    {
        // Emitting an N or -
        return 1.0;
    }
    else
    {
        // Emitting an A, T, G, or C
        if (copied_letter >= 0)
        {
            // Copying from an A, T, G, or C
            return (copied_letter == emitted_letter) ? emission_same_state : emission_diff_state;
        }
        else
        {
            // Copying from an N or -.

            // We aren't including the probability for sampling the letters of the first haplotype.
            // If all previous haplotypes are N/-, we should set this to 1.0.
            if (all_previous_missing)
                return 1.0;
            else
            {
                // How to copy a letter from an N or -?
                // Ideas. Ideally, we'd like to integrate over all possibilities for all the missing states,
                //        accounting for linkage.

                // *1. Assume we can set the missing state to agree with us -- not theoretically valid.

                //  2. Average over the previous state, assuming that it is drawn at random from previous non-missing
                //     states in this column.

                //  3. Is there an exact solution for the distribution of this letter given previous letters, if we ignore
                //     linkage?

                double p_same_state = 1.0;
                double p_diff_state = 1.0 - p_same_state;
                return p_same_state*emission_same_state + p_diff_state*emission_diff_state;
            }
        }
    }
}

/*
 *  OK, so for variant j, we compute the recombination probability based on the distance between them times rho.
 *  This is equal to the integal of rho dx from d[k-1] to d[k].  So lets say that if we have 10 sites, then the
 *  chromosome is of length 10, and the first site is at 0.5, and the last site is at 9.5.
 *
 *  So, we need to integrate rho from d[k-1]+0.5 to d[k] + 0.5
 */

// Question: do we want to actually distinguish between N and -?
log_double_t li_stephens_2003_conditional_sampling_distribution(const alignment& A, const vector<int>& d, int k, const EVector& rho, double theta)
{
    // 1. Determine mutation probabilities when copying from a given parent.
    assert(k>=1);
    double emission_diff_state = (theta <1.0) ? 0.5*theta/(k + theta) : 0.5/(k/theta + 1);
    double emission_same_state = 1.0 - emission_diff_state;

    // 2. Set the probability of copying from each of the k parents to 1/k at location 0.
    double L = A.length();
    Matrix m(L+1,k);
    vector<int> scale(L+1,0);
    for(int i=0;i<k;i++)
        m(0,i) = 1.0/k;

    int rho_index=0;
    auto rho_integral = [&](double l1, double l2)
    {
	assert(l1 <= l2);
	assert(rho[rho_index].as_<EPair>().first.as_double() <= l1);
	// Find the change-point just before l1.
	while(rho_index+1 < rho.size() and rho[rho_index+1].as_<EPair>().first.as_double() < l1)
	    rho_index++;
	double integral = 0;
	// As long as there is a change-point before l2, add in the integral of rho(l) from l1 to the next change-point.
	while(rho_index+1 < rho.size() and rho[rho_index+1].as_<EPair>().first.as_double() < l2)
	{
	    integral += rho[rho_index].as_<EPair>().second.as_double() * (rho[rho_index+1].as_<EPair>().first.as_double() - l1);
	    l1 = rho[rho_index].as_<EPair>().first.as_double();
	    rho_index++;
	}
	// Now add in the integral from the l1 to l2.
	integral += rho[rho_index].as_<EPair>().second.as_double() * (l2 - l1);
	return integral;
    };

    // 3. Perform the forward algorithm
    int prev_column = 0;
    for(int column1=0; column1<L; column1++)
    {
        double maximum = 0;

        int column2 = column1 + 1;
        double rho_mass = rho_integral(prev_column+0.5, d[column1]+0.5);
        double transition_diff_parent = (1.0-exp(-rho_mass/k))/k;
        double transition_same_parent = 1.0 - (k-1)*transition_diff_parent;
        int emitted_letter = A(column1,k);

        bool all_previous_missing = true;
        for(int i2=0;i2<k;i2++)
        {
            // Emission is 1.0 if missing data at i2
            int copied_letter  = A(column1,i2);
            double emission_i2 = emission_probability( copied_letter, emitted_letter, emission_diff_state, emission_same_state, all_previous_missing );
            if (copied_letter >= 0) all_previous_missing = false;

            double total = 0;
            for(int i1=0;i1<k;i1++)
            {
                double transition_i1_i2 = (i1==i2) ? transition_same_parent : transition_diff_parent;
                total += m(column1,i1) * transition_i1_i2 * emission_i2;
            }
            if (total > maximum) maximum = total;
	    assert( total > 0 );
            m(column2,i2) = total;
        }
        prev_column = d[column1];

        scale[column2] = scale[column1];

        //------- if exponent is too low, rescale ------//
        if (maximum > 0 and maximum < fp_scale::lo_cutoff)
        {
            int logs = -(int)log2(maximum);
            double scale_factor = pow2(logs);
            for(int i2=0;i2<k;i2++)
                m(column2,i2) *= scale_factor;
            scale[column2] -= logs;
        }
    }

    // 3. Compute the total probability
    double total = 0;
    for(int i=0;i<k;i++)
        total += m(L-1,i);

    log_double_t Pr = pow(log_double_t(2.0),scale[L-1]) * total;
    
    return { Pr };
}

log_double_t li_stephens_2003_composite_likelihood(const alignment& A, const vector<int>& d, const EVector& rho)
{
    assert(A.length() == d.size());

    int n = A.n_sequences();

    double theta_ish = li_stephens_2003_theta(n);

    log_double_t Pr = 1.0;
    for(int i=1;i<n;i++)
        Pr *= li_stephens_2003_conditional_sampling_distribution(A, d, i, rho, theta_ish);

    return Pr;
}


extern "C" closure builtin_function_li_stephens_2003_composite_likelihood_raw(OperationArgs& Args)
{
    auto locs = (vector<int>)Args.evaluate(0).as_<EVector>();

    auto arg1 = Args.evaluate(1);
    auto& rho_func = arg1.as_<EVector>();

    auto arg2 = Args.evaluate(2);
    auto& A = arg2.as_<Box<alignment>>().value();

    log_double_t Pr = li_stephens_2003_composite_likelihood(A, locs, rho_func);
    
    return { Pr };
}

int get_allele(const expression_ref& haplotype, int site)
{
    return haplotype.as_<EVector>()[site].as_int();
}

int get_allele(const expression_ref& haplotypes, int individual, int site)
{
    return get_allele(haplotypes.as_<EVector>()[individual], site);
}

// OK, for DEploid, we could take
// (a1) 0/1 panel + VCF
// (a2) 0/1 plaf  + VCF

// (b) ATGC panel + VCF
// (b) ATGC plaf  + VCF

// (c) ATGC complete sequences.

// Can we write out a kind of "patch file" for each iteration?
//   Technically, this could be a VCF, but that would take a lot of space.
//   Maybe just the first 3 fields - site,ref,alt?

// CSD = conditional sampling distribution

log_double_t deploid_01_plaf_only_CSD(const EVector& alt_allele_frequency, const EVector& haplotype)
{
    assert(alt_allele_frequency.size() == haplotype.size());

    log_prod total;
    for(int site=0; site < haplotype.size(); site++)
    {
        int h = haplotype[site].as_int();
        double f = alt_allele_frequency[site].as_double();
        double p = h?f:1.0-f;
        total *= p;
    }
    log_double_t Pr = total;
    return {Pr};
}

vector<double> get_switching_probs(double switching_rate, const EVector& sites)
{
    const int L = sites.size();
    vector<double> pr_switch(L);

    int prev_column = 0;
    for(int i=0;i<sites.size();i++)
    {
        double dist = sites[i].as_int() - prev_column;

        pr_switch[i] = (1.0-exp(-switching_rate*dist));

        prev_column = sites[i].as_int();
    }

    return pr_switch;
}

vector<pair<double,double>> get_transition_probs_deploid(double switching_rate, int k, const EVector& sites)
{
    const int L = sites.size();
    auto pr_switch = get_switching_probs(switching_rate, sites);

    vector<pair<double,double>> transition_pr(L);

    for(int i=0;i<L;i++)
    {
        double transition_diff_parent = pr_switch[i] / k;

        double transition_same_parent = (1.0 - pr_switch[i]) + transition_diff_parent;

        transition_pr[i] = {transition_diff_parent, transition_same_parent};
    }

    return transition_pr;
}

log_double_t panel_01_CSD_single(const EVector& source_hap, double emission_diff_state, const EVector& haplotype)
{
    // 1. Determine mutation probabilities when copying from a given parent.
    double emission_same_state = 1.0 - emission_diff_state;

    // 2. Check haplotype lengths and number of sites
    int L = source_hap.size();
    assert(haplotype.size() == L);

    log_prod Pr;

    // 3. Perform the forward algorithm
    for(int site=0; site<L; site++)
    {
        // Emission is 1.0 if missing data at state2
        int emitted_letter = get_allele(haplotype, site);
        int copied_letter  = get_allele(source_hap, site);
        Pr *= emission_probability( copied_letter, emitted_letter, emission_diff_state, emission_same_state, false );
    }

    return Pr;
}

log_double_t panel_01_CSD_no_recomb(const EVector& panel, double emission_diff_state, const EVector& haplotype)
{
    log_double_t Pr = 0;
    for(auto& source_hap: panel)
        Pr += panel_01_CSD_single(source_hap.as_<EVector>(), emission_diff_state, haplotype);
    Pr /= panel.size();
    return Pr;
}

log_double_t panel_01_CSD(const EVector& panel, const EVector& sites, double switching_rate, double emission_diff_state, const EVector& haplotype)
{
    if (switching_rate == 0)
        return panel_01_CSD_no_recomb(panel, emission_diff_state, haplotype);

    int k = panel.size();
    assert(k >= 1);

    // 1. Determine mutation probabilities when copying from a given parent.
    double emission_same_state = 1.0 - emission_diff_state;

    // 2. Check haplotype lengths and number of sites
    double L = panel[0].as_<EVector>().size();
    for(int i=0;i<panel.size();i++)
        assert(panel[i].as_<EVector>().size() == L);
    assert(sites.size() == L);
    assert(haplotype.size() == L);

    // 3. Set the probability of copying from each of the k parents to 1/k at location 0.
    Matrix F(L+1,k);
    vector<int> scale(L+1,0);
    for(int i=0;i<k;i++)
        F(0,i) = 1.0/k;


    auto transition_probs = get_transition_probs_deploid(switching_rate, k, sites);
    auto T = [&](int site, int path_state1, int path_state2)
    {
        auto& [transition_diff_state, transition_same_state] = transition_probs[site];
        return (path_state1 == path_state2) ? transition_same_state : transition_diff_state;
    };

    // 4. Perform the forward algorithm
    for(int site=0; site<L; site++)
    {
        int column1 = site;
        int column2 = site + 1;

        bool all_previous_missing = true;
        for(int state2=0;state2<k;state2++)
        {
            double total = 0;
            for(int state1=0;state1<k;state1++)
                total += F(column1,state1) * T(column1, state1, state2);
            F(column2,state2) = total;
        }

        double maximum = 0;
        for(int state2=0;state2<k;state2++)
        {
            // Emission is 1.0 if missing data at state2
            int emitted_letter = get_allele(haplotype, column1); // This will be 0 or 1, or negative for a missing value.
            int copied_letter  = get_allele(panel, state2, column1);
            if (copied_letter >= 0) all_previous_missing = false;
            F(column2,state2) *= emission_probability( copied_letter, emitted_letter, emission_diff_state, emission_same_state, all_previous_missing );
            maximum = std::max(maximum, F(column2,state2) );
        }

        scale[column2] = scale[column1];

        //------- if exponent is too low, rescale ------//
        if (maximum > 0 and maximum < fp_scale::lo_cutoff)
        {
            int logs = -(int)log2(maximum);
            double scale_factor = pow2(logs);
            for(int state2=0;state2<k;state2++)
                F(column2,state2) *= scale_factor;
            scale[column2] -= logs;
        }
    }

    // 3. Compute the total probability
    double total = 0;
    for(int i=0;i<k;i++)
        total += F(L-1,i);

    log_double_t Pr = pow(log_double_t(2.0),scale[L-1]) * total;

    return { Pr };
}

extern "C" closure builtin_function_haplotype01_from_plaf_probability(OperationArgs& Args)
{
    // 1. Population-Level Allele Frequencies (PLAF) - an EVector of double.
    auto arg0 = Args.evaluate(0);
    auto& plaf = arg0.as_<EVector>();

    // 2. Haplotypes - an EVector of EVector of Int
    auto arg1 = Args.evaluate(1);
    auto& haplotype = arg1.as_<EVector>();

    auto Pr = deploid_01_plaf_only_CSD(plaf, haplotype);

    return {Pr};
}

extern "C" closure builtin_function_haplotype01_from_panel_probability(OperationArgs& Args)
{
    // 0. Panel haplotypes
    auto arg0 = Args.evaluate(0);
    auto& panel = arg0.as_<EVector>();

    // 1. Panel sites
    auto arg1 = Args.evaluate(1);
    auto& sites = arg1.as_<EVector>();

    // 2. Switching rate
    double switching_rate = Args.evaluate(2).as_double();

    // 3. State-flipping probability
    double diff_state = Args.evaluate(3).as_double();

    // 4. Haplotypes - an EVector of EVector of Int
    auto arg4 = Args.evaluate(4);
    auto& haplotype = arg4.as_<EVector>();

    auto Pr = panel_01_CSD(panel, sites, switching_rate, diff_state, haplotype);

    return {Pr};
}



// In theory we could construct this in Haskell by something like
//    haplotype <- independant [ bernoulli f | f <- frequencies ]
// In that case redrawing individual element would come automatically.
extern "C" closure builtin_function_sample_haplotype01_from_plaf(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_(0);
    auto& alt_allele_frequency = arg0.as_<EVector>();

    int num_sites = alt_allele_frequency.size();

    object_ptr<EVector> H (new EVector(num_sites));
    auto& haplotype = *H;
    for(int site=0; site < num_sites; site++)
    {
        double f = alt_allele_frequency[site].as_double();
        if (bernoulli(f))
            haplotype[site] = 1;
        else
            haplotype[site] = 0;
    }
    return H;
}

extern "C" closure builtin_function_sample_haplotype01_from_panel(OperationArgs& Args)
{
    // 0. Panel - EVector of Evector of Int
    auto arg0 = Args.evaluate_(0);
    auto& panel = arg0.as_<EVector>();

    // 1. Sites - EVector of Int
    auto arg1 = Args.evaluate_(1);
    auto& sites = arg1.as_<EVector>();

    // 2. Switching rate
    double switching_rate = Args.evaluate_(2).as_double();

    // 3. State-flipping probability
    double diff_state = Args.evaluate_(3).as_double();

    int k = panel.size();
    assert(k > 0);
    int L = panel[0].as_<EVector>().size();
    for(int i=0;i<k;i++)
        assert(panel[i].as_<EVector>().size() == L);
    assert(sites.size() == L);

    EVector haplotype(L);
    int source_haplotype = uniform_int(0,k-1);

    auto pr_switch = get_switching_probs(switching_rate, sites);
    for(int i=0; i<L; i++)
    {
        // Maybe switch to new source haplotype for this site.
        if (bernoulli(pr_switch[i]))
            source_haplotype = uniform_int(0,k-1);

        // Get an emitted letter
        int source_letter = get_allele(panel, source_haplotype, i);
        int emitted_letter = source_letter;

        // Handle missing data in panel
        if (source_letter < 0)
            emitted_letter = bernoulli(0.5);
        else if (bernoulli(diff_state))
            emitted_letter = 1 - emitted_letter;

        // Record the value for the haplotype
        haplotype[i] = emitted_letter;
    }
    return {haplotype};
}



double wsaf_at_site(int site, const EVector& weights, const EVector& haplotypes)
{
    int num_strains = weights.size();

    double q = 0;
    for(int j=0;j<num_strains;j++)
    {
        double w = weights[j].as_double();
        double h = get_allele(haplotypes, j, site);
        assert(h == 0 or h == 1);
        q += w*h;
    }

    return std::min(q,1.0);
}

EPair sample_site_reads01(int N, double wsaf, double error_rate, double c, double outlier_frac)
{
    assert(0 <= N);
    assert(0 <= wsaf and wsaf <= 1.0);
    assert(0 <= error_rate and error_rate <= 1.0);
    assert(0 <= c);

    double pi = wsaf + error_rate*(1.0 - 2.0*wsaf);

    int alt = (bernoulli(outlier_frac)) ? beta_binomial(N, 1.0, 1.0) : beta_binomial(N, c*pi, c*(1-pi));
    int ref = N - alt;

    return EPair(ref,alt);
}

log_double_t site_likelihood_for_reads01(int counts, int ref, int alt, double wsaf, double error_rate, double c, double outlier_frac)
{
    assert(0 <= ref);
    assert(0 <= alt);
    assert(std::isnan(wsaf) or (0 <= wsaf and wsaf <= 1.0));
    assert(0 <= error_rate and error_rate <= 1.0);
    assert(0 <= c);

    if (counts != ref + alt)
        return 0.0;

    double pi = wsaf + error_rate*(1.0 - 2.0*wsaf);

    return (1.0-outlier_frac) * beta_binomial_pdf(ref+alt, c*pi, c*(1.0-pi), alt)
           +outlier_frac      * beta_binomial_pdf(ref+alt, 1.0, 1.0, alt);
}

log_double_t site_likelihood_for_reads01(int counts, const expression_ref& reads, double wsaf, double error_rate, double c, double outlier_frac)
{
    int ref = reads.as_<EPair>().first.as_int();
    int alt = reads.as_<EPair>().second.as_int();

    return site_likelihood_for_reads01(counts, ref, alt, wsaf, error_rate, c, outlier_frac);
}

// Pr(D|h, w, n, \psi) where psi includes
// * e = error rate
// * c = concentration parameter for beta in beta-binomial
extern "C" closure builtin_function_probability_of_reads01(OperationArgs& Args)
{
    // 1. Read counts at each locus
    auto arg0 = Args.evaluate(0);
    auto& counts = arg0.as_<EVector>();

    // 2. Mixture weights - an EVector of double.
    auto arg1 = Args.evaluate(1);
    auto& weights = arg1.as_<EVector>();

    // 3. Haplotypes - an EVector of EVector of Int
    auto arg2 = Args.evaluate(2);
    auto& haplotypes = arg2.as_<EVector>();

    // 4. Error rate
    double error_rate = Args.evaluate(3).as_double();
    assert(0 <= error_rate and error_rate <= 1.0);

    // 5. Beta concentration parameter
    double c = Args.evaluate(4).as_double();
    assert(c > 0);

    // 6. Outlier fraction
    double outlier_frac = Args.evaluate(5).as_double();
    assert(outlier_frac >= 0 and outlier_frac <= 1);

    // 7. Reads = EVector of EPair of Int
    auto arg6 = Args.evaluate(6);
    auto& reads = arg6.as_<EVector>();

    int num_sites = counts.size();
    if (reads.size() != num_sites)
        return { log_double_t(0.0) };

#ifndef NDEBUG
    int num_strains = weights.size();
    assert(haplotypes.size() == weights.size());

    for(int i=0;i<num_strains;i++)
        assert(haplotypes[i].as_<EVector>().size() == num_sites);
#endif

    // 2. Accumulate observation probabilities!
    log_double_t Pr = 1.0;
    for(int site=0; site<num_sites; site++)
    {
        double wsaf = wsaf_at_site(site, weights, haplotypes);

        auto pr = site_likelihood_for_reads01(counts[site].as_int(), reads[site], wsaf, error_rate, c, outlier_frac);

        if (pr == 0.0)
        {
            Pr = 0.0;
            break;
        }

        Pr *= pr;
    }

    return { Pr };
}

// Pr(D|h, w, n, \psi) where psi includes
// * e = error rate
// * c = concentration parameter for beta in beta-binomial
extern "C" closure builtin_function_sample_reads01(OperationArgs& Args)
{
    // 1. Read counts at each locus
    auto arg0 = Args.evaluate_(0);
    auto& counts = arg0.as_<EVector>();

    // 2. Mixture weights - an EVector of double.
    auto arg1 = Args.evaluate_(1);
    auto& weights = arg1.as_<EVector>();

    // 3. Haplotypes - an EVector of EVector of Int
    auto arg2 = Args.evaluate_(2);
    auto& haplotypes = arg2.as_<EVector>();

    // 4. Error rate
    double error_rate = Args.evaluate_(3).as_double();
    assert(0 <= error_rate and error_rate <= 1.0);

    // 5. Beta concentration parameter
    double c = Args.evaluate_(4).as_double();
    assert(c > 0);

    // 6. Outlier fraction
    double outlier_frac = Args.evaluate_(5).as_double();
    assert(outlier_frac >= 0 and outlier_frac <= 1);

    int num_sites = counts.size();
#ifndef NDEBUG
    int num_strains = weights.size();
    assert(haplotypes.size() == weights.size());

    for(int i=0;i<num_strains;i++)
        assert(haplotypes[i].as_<EVector>().size() == num_sites);
#endif

    // 2. Accumulate observation probabilities!
    EVector reads(num_sites);

    for(int site=0; site<num_sites; site++)
    {
        double wsaf = wsaf_at_site(site, weights, haplotypes);

        reads[site] = sample_site_reads01(counts[site].as_int(), wsaf, error_rate, c, outlier_frac);
    }

    return reads;
}

int get_allele_from_state(int state, int i)
{
    return (state&(1<<i))?1:0;
}

double get_prior(int A, double f, int n)
{
    double prior = 1;
    for(int i=0;i<n;i++)
    {
        if (A&(1<<i))
            prior *= f;
        else
            prior *= (1.0-f);
    }
    return prior;
}

matrix<log_double_t> emission_pr(const vector<int>& K, const EVector& reads, const EVector& haplotypes, const EVector& weights, double error_rate, double concentration, double outlier_frac)
{
    int L = haplotypes[0].as_<EVector>().size();
    int N = K.size();
    int n_states = (1<<N); // 2^N

    auto E = matrix<log_double_t>(L, n_states);
    for(int site=0; site<L; site++)
    {
        double current_wsaf = wsaf_at_site(site, weights, haplotypes);
        for(int state = 0; state < n_states; state++)
        {
            double wsaf = current_wsaf;
            for(int i=0; i<N; i++)
            {
                int k = K[i];
                int old_allele = get_allele(haplotypes, k, site);
                int new_allele = get_allele_from_state(state,i);
                wsaf += weights[k].as_double() * (new_allele - old_allele);
            }

            // Avoid out-of-bounds terms caused by rounding error.
            wsaf = std::max(0.0,std::min(1.0,wsaf));

            int ref = reads[site].as_<EPair>().first.as_int();
            int alt = reads[site].as_<EPair>().second.as_int();
            E(site, state) = site_likelihood_for_reads01(ref+alt, reads[site], wsaf, error_rate, concentration, outlier_frac);
        }
    }
    return E;
}


// FIXME -- should we really be taking a context index here?
// FIXME -- how are we going to call this from the proposal function?

extern "C" closure builtin_function_emission_pr_for_reads01(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. Get haplotype indices
    context_ptr hap_indices(C0, Args.reg_for_slot(1));
    vector<int> K = (vector<int>) hap_indices.list_to_vector();

    // 2. reads = EVector (EPair Int Int)
    auto arg2 = evaluate_slot(C0, 2);
    auto& reads = arg2.as_<EVector>();

    // 3. haplotypes = EVector (EVector Int)
    context_ptr haplotypes_ptr(C0, Args.reg_for_slot(3));
    EVector haplotypes = haplotypes_ptr.list_to_vector();

    // 4. weights = EVector Double
    auto weights = evaluate_slot(C0, 4).as_<EVector>();

    // 5. error_rate = double
    double error_rate = evaluate_slot(C0, 5).as_double();

    // 6. concentration = double
    double concentration = evaluate_slot(C0, 6).as_double();

    // 7. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 7).as_double();

    object_ptr<Box<matrix<log_double_t>>> result = new Box<matrix<log_double_t>>;
    *result = emission_pr(K, reads, haplotypes, weights, error_rate, concentration, outlier_frac);
    return result;
}

log_double_t shift_gaussian(context_ref& C, int r, double scale)
{
    double x = C.evaluate_reg(r).as_double();

    x += gaussian(0, scale);

    C.set_reg_value(r, {x});

    return exp_to<log_double_t>(0.0);
}

Proposal shift_gaussian(int r, double scale)
{
    return [=](context_ref& C) {return shift_gaussian(C, r, scale);};
}

log_double_t shift_laplace(context_ref& C, int r, double scale)
{
    double x = C.evaluate_reg(r).as_double();

    x += laplace(0, scale);

    C.set_reg_value(r, {x});

    return exp_to<log_double_t>(0.0);
}

Proposal shift_laplace(int r, double scale)
{
    return [=](context_ref& C) {return shift_laplace(C, r, scale);};
}

log_double_t propose_two_titres_constant_sum(context_ref& C, int r1, int r2)
{
    double x1 = C.evaluate_reg(r1).as_double();
    double x2 = C.evaluate_reg(r2).as_double();

    log_double_t w1 = exp_to<log_double_t>(x1);
    log_double_t w2 = exp_to<log_double_t>(x2);

    log_double_t total = w1 + w2;

    log_double_t w1_new = uniform()*total;
    log_double_t w2_new = total - w1_new;

    double x1_new = log(w1_new);
    double x2_new = log(w2_new);

    C.set_reg_value(r1, {x1_new});
    C.set_reg_value(r2, {x2_new});

    return (w1 * w2) / (w1_new * w2_new);
}



bool all_different(vector<int> v)
{
    std::sort(v.begin(), v.end());
    for(int i=1;i<v.size();i++)
        if (v[i-1] == v[i]) return false;

    return true;
}

int get_state_from_haplotypes(const EVector& haplotypes, const vector<int>& K, int site)
{
    int N = K.size();
    int state = 0;

    for(int i=0; i < N; i++)
    {
        int old_allele = get_allele(haplotypes, K[i], site);
        state += (old_allele << i);
    }

    return state;
}

// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(reads | h, w, rror_rates, c)

// Therefore, we need(x[i], x[j], h[i], h[j], i, j, plaf, reads, h, w, error_rate, c)

extern "C" closure builtin_function_propose_haplotypes_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C(M,context_index);

    // 1. Get haplotype indices
    context_ptr hap_indices(C, Args.reg_for_slot(1));
    vector<int> K = (vector<int>) hap_indices.list_to_vector();

    int N = K.size();

    int n_states = (1<<N);

    // 2. Get h[i]
    vector<int> haplotype_regs(N);

    context_ptr haplotypes_ptr(C, Args.reg_for_slot(2));
    for(int i=0; i<N; i++)
    {
        if (auto haplotype_mod = haplotypes_ptr.list_element(K[i]).modifiable())
            haplotype_regs[i] = haplotype_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: haplotype"<<i+1<<" reg "<<haplotype_regs[0]<<" is not a modifiable!";
    }

    EVector haplotypes = haplotypes_ptr.list_to_vector();

    // 3. Get frequencies
    auto arg4 = evaluate_slot(C, 3);
    auto& frequencies = arg4.as_<EVector>();

    // 4. Mixture weights = EVector of double.
    auto weights1 = evaluate_slot(C, 4).as_<EVector>();

    // 5. reads = EVector of EPair of Int
    auto arg6 = evaluate_slot(C, 5);
    auto& reads = arg6.as_<EVector>();

    // 6. error_rate = double
    double error_rate = evaluate_slot(C, 6).as_double();

    // 7. concentration = double
    double concentration = evaluate_slot(C, 7).as_double();

    // 8. outlier_frac = double
    double outlier_frac = evaluate_slot(C, 8).as_double();

    //----------- Make sure that the N haplotypes are DIFFERENT -------------
    if (not all_different(K))
        return {log_double_t(1)};

    int L = haplotypes[0].as_<EVector>().size();

    //---------- Compute emission probabilities for the two weight vectors -----------//

    auto E = emission_pr(K, reads, haplotypes, weights1, error_rate, concentration, outlier_frac);

    //---------- Sample new haplotypes -----------//
    vector<object_ptr<EVector>> new_haplotypes( N );
    for(auto& h: new_haplotypes)
        h = object_ptr<EVector>(new EVector(L));

    log_double_t pr_sample_0 = 1;
    log_double_t pr_sample_1 = 1;

    for(int site = 0; site < L; site++)
    {
        double plaf = frequencies[site].as_double();
        vector<log_double_t> F(n_states);
        for(int i=0;i<n_states;i++)
            F[i] = E(site,i) * get_prior(i, plaf, N);

        int old_A = get_state_from_haplotypes(haplotypes, K, site);
        pr_sample_0 *= choose_P(old_A, F);

        int new_A = choose(F);
        pr_sample_1 *= choose_P(new_A, F);

        for(int i=0;i<N;i++)
            (*new_haplotypes[i])[site] = get_allele_from_state(new_A,i);
    }

    for(int i=0;i<N;i++)
        C.set_reg_value(haplotype_regs[i], new_haplotypes[i]);

    log_double_t ratio = pr_sample_0 / pr_sample_1;

    return {ratio};
}


// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(reads | h, w, rror_rates, c)

// Therefore, we need(x[i], x[j], h[i], h[j], i, j, plaf, reads, h, w, error_rate, c)

extern "C" closure builtin_function_propose_weights_and_haplotypes_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. Get haplotype indices
    context_ptr hap_indices(C0, Args.reg_for_slot(1));
    vector<int> K = (vector<int>) hap_indices.list_to_vector();

    int N = K.size();

    int n_states = (1<<N);

    // 2. Get x[i]
    vector<int> titre_regs(N);

    context_ptr titres_ptr(C0, Args.reg_for_slot(2));

    for(int i=0; i<N; i++)
    {
        if (auto titre_mod = titres_ptr.list_element(K[i]).modifiable())
            titre_regs[i] = titre_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: titre reg"<<i+1<<" is not a modifiable!";
    }

    // 3. Get h[i]
    vector<int> haplotype_regs(N);

    context_ptr haplotypes_ptr(C0, Args.reg_for_slot(3));
    for(int i=0; i<N; i++)
    {
        if (auto haplotype_mod = haplotypes_ptr.list_element(K[i]).modifiable())
            haplotype_regs[i] = haplotype_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: haplotype"<<i+1<<" reg "<<haplotype_regs[0]<<" is not a modifiable!";
    }

    EVector haplotypes = haplotypes_ptr.list_to_vector();

    // 4. Get frequencies
    auto arg4 = evaluate_slot(C0, 4);
    auto& frequencies = arg4.as_<EVector>();

    // 5. Mixture weights = EVector of double.
    constexpr int weight_slot = 5;
    auto weights1 = evaluate_slot(C0, weight_slot).as_<EVector>();

    // 6. reads = EVector of EPair of Int
    auto arg6 = evaluate_slot(C0, 6);
    auto& reads = arg6.as_<EVector>();

    // 7. error_rate = double
    double error_rate = evaluate_slot(C0, 7).as_double();

    // 8. concentration = double
    double concentration = evaluate_slot(C0, 8).as_double();

    // 9. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 9).as_double();

    //----------- Make sure that the N haplotypes are DIFFERENT -------------
    if (not all_different(K))
        return {log_double_t(1)};

    int L = haplotypes[0].as_<EVector>().size();

    //------------- Copy the context indices ------------------//

    context C1 = C0; // old weights, new haplotype

    context C2 = C0; // new weights, new haplotype

    //-------------- Propose new weights in C2 ----------------//

    log_double_t w_ratio = 1;

    w_ratio *= shift_laplace(C2, titre_regs[0], 3.0);

    if (N >= 2)
        w_ratio *= shift_laplace(C2, titre_regs[1], 0.125);

    auto weights2 = evaluate_slot(C2, weight_slot).as_<EVector>();

    //---------- Compute emission probabilities for the two weight vectors -----------//

    auto E1 = emission_pr(K, reads, haplotypes, weights1, error_rate, concentration, outlier_frac);

    auto E2 = emission_pr(K, reads, haplotypes, weights2, error_rate, concentration, outlier_frac);

    //---------- Sample new haplotypes for C1 -----------//
    vector<object_ptr<EVector>> new_haplotypes1( N );
    for(auto& h: new_haplotypes1)
        h = object_ptr<EVector>(new EVector(L));

    log_double_t pr_sample_0 = 1;
    log_double_t pr_sample_1 = 1;

    for(int site = 0; site < L; site++)
    {
        double plaf = frequencies[site].as_double();
        vector<log_double_t> F(n_states);
        for(int i=0;i<n_states;i++)
            F[i] = E1(site,i) * get_prior(i, plaf, N);

        int old_A = get_state_from_haplotypes(haplotypes, K, site);
        pr_sample_0 *= choose_P(old_A, F);

        int new_A = choose(F);
        pr_sample_1 *= choose_P(new_A, F);

        for(int i=0;i<N;i++)
            (*new_haplotypes1[i])[site] = get_allele_from_state(new_A,i);
    }

    //---------- Sample new haplotypes for C2 -----------//
    vector<object_ptr<EVector>> new_haplotypes2( N );
    for(auto& h: new_haplotypes2)
        h = object_ptr<EVector>(new EVector(L));

    log_double_t pr_sample_2 = 1;

    for(int site = 0; site < L; site++)
    {
        double plaf = frequencies[site].as_double();
        vector<log_double_t> F(n_states);
        for(int i=0;i<n_states;i++)
            F[i] = E2(site,i) * get_prior(i, plaf, N);

        int new_A = choose(F);
        pr_sample_2 *= choose_P(new_A, F);

        for(int i=0;i<N;i++)
            (*new_haplotypes2[i])[site] = get_allele_from_state(new_A,i);
    }

    for(int i=0;i<N;i++)
        C1.set_reg_value(haplotype_regs[i], new_haplotypes1[i]);
    auto Pr1_over_Pr0 = C1.heated_probability_ratio(C0);

    // ASSUME Pr(h0)/sample_hap0 = Pr(h1)/sample_hap1
    //        Pr(h1)/Pr(h0) = sample_hap1 / sample_hap0
    assert( std::abs( log(Pr1_over_Pr0) - log(pr_sample_1/pr_sample_0) ) < 1.0e-9 );

    for(int i=0;i<N;i++)
        C2.set_reg_value(haplotype_regs[i], new_haplotypes2[i]);
    auto Pr2_over_Pr0 = C2.heated_probability_ratio(C0);

    if (log_verbose >= 4)
    {
        std::cerr<<"propose_weights_and_three_haplotypes: Pr1/Pr0 = "<<Pr1_over_Pr0<<"      Pr2/Pr0 = "<<Pr2_over_Pr0<<"\n";
    }

    auto Pr = vector<log_double_t>{Pr1_over_Pr0/pr_sample_1, w_ratio * Pr2_over_Pr0/pr_sample_2};

    int choice_index = choose(Pr);

    log_double_t ratio = 1;
    if (choice_index == 0)
    {
        C0 = C1;
        ratio = 1.0/Pr1_over_Pr0;
    }
    else if (choice_index == 1)
    {
        C0 = C2;
        ratio = 1.0/Pr2_over_Pr0;
    }

    if (log_verbose >= 4)
    {
        std::cerr<<"propose_weights_and_haplotypes: choice = "<<choice_index<<"    Pr = {"<<Pr[0]<<","<<Pr[1]<<"}   ratio = "<<ratio<<"\n";
    }

    return {ratio};
}

log_double_t resample_haps_from_panel_no_recomb(context_ref& C,
                                                const vector<int>& K,
                                                const vector<int>& haplotype_regs,
                                                const EVector& haplotypes,
                                                const EVector& panel,
                                                double miscopy_prob,
                                                const EVector& weights,
                                                const EVector& reads,
                                                double error_rate,
                                                double concentration,
                                                double outlier_frac)
{
    assert(all_different(K));

    int n_resample_haps = K.size();
    int n_panel_haps = panel.size();
    double emission_diff_state = miscopy_prob;
    double emission_same_state = 1.0 - emission_diff_state;
    int L = haplotypes[0].as_<EVector>().size();

    //---------- 1. Compute transition probabilities -----------//
    int n_path_states = ipow(n_panel_haps, n_resample_haps);

    // get a single path state, and advance the iterator
    auto get_one_path_state = [&](int& path_state)
    {
        int result = path_state % n_panel_haps;
        assert(result >= 0 and result < n_panel_haps);

        path_state /= n_panel_haps;

        return result;
    };

    //---------- 2. Compute emission probabilities -----------//

    int n_haplotype_states = (1<<n_resample_haps);
    auto emission_prs = emission_pr(K, reads, haplotypes, weights, error_rate, concentration, outlier_frac);

    // The probability of emitting a specific haplotype state, given the paths for all resampled haplotypes.
    auto E2 = [&](int site, int haplotype_state, int path_state)
    {
        double prior_haplotype_state = 1;

        for(int i=0;i<n_resample_haps;i++)
        {
            int this_path_state = get_one_path_state(path_state);
            int parent_state = get_allele(panel, this_path_state, site);
            int child_state = get_allele_from_state(haplotype_state, i);
            prior_haplotype_state *= (parent_state == child_state) ? emission_same_state : emission_diff_state;
        };

        return prior_haplotype_state * emission_prs(site,haplotype_state);
    };

    // The probability of emitting ANY/ALL specific haplotype states, given the paths for all resampled haplotypes.
    auto E = [&](int site, int path_state)
    {
        log_double_t pr = 0;
        for(int haplotype_state=0; haplotype_state < n_haplotype_states; haplotype_state++)
            pr += E2(site, haplotype_state, path_state);
        return pr;
    };


    //----------------------- 3. Forward matrix-----------------------//
    matrix<log_double_t> F(L+1, n_path_states, 0.0);

    // Initially each combination of paths is equally likely.
    for(int s=0;s<n_path_states;s++)
        F(0,s) = 1.0/n_path_states;

    for(int site = 0; site < L; site++)
    {
        int column1 = site;
        int column2 = site+1;

        // Get the probabability of transitioning to (column1,state2)
        for(int path_state=0; path_state<n_path_states; path_state++)
            F(column2, path_state) = F(column1, path_state) * E(site, path_state);
    }

    //--------- 4. Back-sample path_states from forward matrix -------//
    vector<log_double_t> pr_path(n_path_states);
    for(int path_state=0; path_state<n_path_states; path_state++)
        pr_path[path_state] = F(L, path_state);

    int chosen_path_state = choose(pr_path);

    //---------- 5. Sample new haplotypes given path states-----------//
    vector<object_ptr<EVector>> new_haplotypes( n_resample_haps );
    for(auto& h: new_haplotypes)
        h = object_ptr<EVector>(new EVector(L));

    for(int site=0; site < L; site++)
    {
        vector<log_double_t> pr_haplotypes(n_haplotype_states);
        for(int haplotype_state = 0; haplotype_state < n_haplotype_states; haplotype_state++)
            pr_haplotypes[haplotype_state] = E2(site, haplotype_state, chosen_path_state);

        int new_haplotype_state = choose(pr_haplotypes);

        for(int i=0;i<n_resample_haps;i++)
            (*new_haplotypes[i])[site] = get_allele_from_state(new_haplotype_state,i);
    }

    for(int i=0;i<n_resample_haps;i++)
        C.set_reg_value(haplotype_regs[i], new_haplotypes[i]);

    //---------- 6. Compute total probability-----------//
    log_double_t total = 0;
    for(int s=0;s<n_path_states;s++)
        total += F(L,s);

    return total;
}

log_double_t resample_haps_from_panel(context_ref& C,
                                      const vector<int>& K,
                                      const vector<int>& haplotype_regs,
                                      const EVector& haplotypes,
                                      const EVector& panel,
                                      const EVector& sites,
                                      double switching_rate,
                                      double miscopy_prob,
                                      const EVector& weights,
                                      const EVector& reads,
                                      double error_rate,
                                      double concentration,
                                      double outlier_frac)
{
    if (switching_rate == 0)
        return resample_haps_from_panel_no_recomb(C, K, haplotype_regs, haplotypes, panel, miscopy_prob, weights, reads, error_rate, concentration, outlier_frac);

    assert(all_different(K));

    int n_resample_haps = K.size();
    int n_panel_haps = panel.size();
    double emission_diff_state = miscopy_prob;
    double emission_same_state = 1.0 - emission_diff_state;
    int L = haplotypes[0].as_<EVector>().size();

    //---------- 1. Compute transition probabilities -----------//
    int n_path_states = ipow(n_panel_haps, n_resample_haps);

    // get a single path state, and advance the iterator
    auto get_one_path_state = [&](int& path_state)
    {
        int result = path_state % n_panel_haps;
        assert(result >= 0 and result < n_panel_haps);

        path_state /= n_panel_haps;

        return result;
    };

    auto transition_prs = get_transition_probs_deploid(switching_rate, n_panel_haps, sites);

    auto T = [&](int site, int path_state1, int path_state2)
    {
        double pr = 1;
        auto& [transition_diff_state, transition_same_state] = transition_prs[site];
        for(int i=0;i<n_resample_haps;i++)
        {
            int this_path_state1 = get_one_path_state(path_state1);
            int this_path_state2 = get_one_path_state(path_state2);
            pr *= (this_path_state1 == this_path_state2) ? transition_same_state : transition_diff_state;
        }
        return pr;
    };

    //---------- 2. Compute emission probabilities -----------//

    int n_haplotype_states = (1<<n_resample_haps);
    auto emission_prs = emission_pr(K, reads, haplotypes, weights, error_rate, concentration, outlier_frac);

    // The probability of emitting a specific haplotype state, given the paths for all resampled haplotypes.
    auto E2 = [&](int site, int haplotype_state, int path_state)
    {
        double prior_haplotype_state = 1;

        for(int i=0;i<n_resample_haps;i++)
        {
            int this_path_state = get_one_path_state(path_state);
            int parent_state = get_allele(panel, this_path_state, site);
            int child_state = get_allele_from_state(haplotype_state, i);
            prior_haplotype_state *= (parent_state == child_state) ? emission_same_state : emission_diff_state;
        };

        return prior_haplotype_state * emission_prs(site,haplotype_state);
    };

    // The probability of emitting ANY/ALL specific haplotype states, given the paths for all resampled haplotypes.
    auto E = [&](int site, int path_state)
    {
        log_double_t pr = 0;
        for(int haplotype_state=0; haplotype_state < n_haplotype_states; haplotype_state++)
            pr += E2(site, haplotype_state, path_state);
        return pr;
    };


    //----------------------- 3. Forward matrix-----------------------//
    matrix<log_double_t> F(L+1, n_path_states, 0.0);

    // Initially each combination of paths is equally likely.
    for(int s=0;s<n_path_states;s++)
        F(0,s) = 1.0/n_path_states;

    for(int site = 0; site < L; site++)
    {
        int column1 = site;
        int column2 = site+1;

        // Get the probabability of transitioning to (column1,state2)
        for(int state1=0;state1<n_path_states;state1++)
            for(int state2=0;state2<n_path_states;state2++)
            {
                F(column2,state2) += F(column1,state1) * T(site, state1, state2);
            }

        // Multiply by the emission probability for (column2, state2)
        for(int state2=0;state2<n_path_states;state2++)
            F(column2,state2) *= E(site, state2);
    }
    
    //--------- 4. Back-sample path_states from forward matrix -------//
    vector<log_double_t> pr_path(n_path_states);
    for(int path_state=0; path_state<n_path_states; path_state++)
        pr_path[path_state] = F(L, path_state);
    int current_path_state = choose(pr_path);

    vector<int> path_states;
    path_states.push_back(current_path_state);

    for(int site=L-2;site >= 0; site--)
    {
        for(int path_state=0; path_state<n_path_states; path_state++)
            pr_path[path_state] = F(site+1,path_state)*T(site+1,path_state, current_path_state);

         current_path_state = choose(pr_path);
         path_states.push_back(current_path_state);
    }
    std::reverse(path_states.begin(), path_states.end());

    //---------- 5. Sample new haplotypes given path states-----------//
    vector<object_ptr<EVector>> new_haplotypes( n_resample_haps );
    for(auto& h: new_haplotypes)
        h = object_ptr<EVector>(new EVector(L));

    for(int site=0; site < L; site++)
    {
        vector<log_double_t> pr_haplotypes(n_haplotype_states);
        for(int haplotype_state = 0; haplotype_state < n_haplotype_states; haplotype_state++)
            pr_haplotypes[haplotype_state] = E2(site, haplotype_state, path_states[site]);

        int new_haplotype_state = choose(pr_haplotypes);

        for(int i=0;i<n_resample_haps;i++)
            (*new_haplotypes[i])[site] = get_allele_from_state(new_haplotype_state,i);
    }

    for(int i=0;i<n_resample_haps;i++)
        C.set_reg_value(haplotype_regs[i], new_haplotypes[i]);

    //---------- 6. Compute total probability-----------//
    log_double_t total = 0;
    for(int s=0;s<n_path_states;s++)
        total += F(L,s);

    return total;
}

// We need the markov blanket for h[i]:
//   Pr(h[i] | panel) * Pr(reads | h, w, rror_rates, c)

// Therefore, we need(indices, haplotypes, panel, switch_rate, miscopy_prob, reads, w, error_rate, c)

extern "C" closure builtin_function_resample_haplotypes_from_panel(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C(M,context_index);

    // 1. Get haplotype indices ([])
    context_ptr hap_indices(C, Args.reg_for_slot(1));
    vector<int> K = (vector<int>) hap_indices.list_to_vector();

    // 2. Get haplotypes ([])
    vector<int> haplotype_regs(K.size());

    context_ptr haplotypes_ptr(C, Args.reg_for_slot(2));
    for(int i=0; i<K.size(); i++)
    {
        if (auto haplotype_mod = haplotypes_ptr.list_element(K[i]).modifiable())
            haplotype_regs[i] = haplotype_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: haplotype"<<i+1<<" reg "<<haplotype_regs[0]<<" is not a modifiable!";
    }

    EVector haplotypes = haplotypes_ptr.list_to_vector();

    // 3. Get panel ([])
    context_ptr panel_ptr(C, Args.reg_for_slot(3));
    EVector panel = panel_ptr.list_to_vector();

    // 4. Get sites (EVector)
    EVector sites = evaluate_slot(C,4).as_<EVector>();

    // 5. Get switching rate
    double switching_rate = evaluate_slot(C,5).as_double();

    // 6. Get emission_diff_state
    double miscopy_prob = evaluate_slot(C,6).as_double();

    // 7. Mixture weights = EVector of double.
    auto weights = evaluate_slot(C, 7).as_<EVector>();

    // 8. reads = EVector of EPair of Int
    auto arg6 = evaluate_slot(C, 8);
    auto& reads = arg6.as_<EVector>();

    // 9. error_rate = double
    double error_rate = evaluate_slot(C, 9).as_double();

    // 10. concentration = double
    double concentration = evaluate_slot(C, 10).as_double();

    // 11. outlier_frac = double
    double outlier_frac = evaluate_slot(C, 11).as_double();

    // ---- 1. Make sure that the haplotypes to resample are DIFFERENT --------//
    if (not all_different(K))
        return {log_double_t(1)};

    // ---- 2. Resample the haplotypes
    resample_haps_from_panel(C, K, haplotype_regs, haplotypes, panel, sites, switching_rate, miscopy_prob, weights, reads, error_rate, concentration, outlier_frac);

    return {log_double_t(1)};
}


extern "C" closure builtin_function_resample_weights_and_haplotypes_from_panel(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. Get haplotype indices
    context_ptr hap_indices(C0, Args.reg_for_slot(1));
    vector<int> K = (vector<int>) hap_indices.list_to_vector();

    int N = K.size();

    // 2. Get x[i]
    vector<int> titre_regs(N);

    context_ptr titres_ptr(C0, Args.reg_for_slot(2));

    for(int i=0; i<N; i++)
    {
        if (auto titre_mod = titres_ptr.list_element(K[i]).modifiable())
            titre_regs[i] = titre_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: titre reg"<<i+1<<" is not a modifiable!";
    }

    // 3. Get h[i]
    vector<int> haplotype_regs(N);

    context_ptr haplotypes_ptr(C0, Args.reg_for_slot(3));
    for(int i=0; i<N; i++)
    {
        if (auto haplotype_mod = haplotypes_ptr.list_element(K[i]).modifiable())
            haplotype_regs[i] = haplotype_mod->get_reg();
        else
            throw myexception()<<"propose_weights_and_haplotypes_from_plaf: haplotype"<<i+1<<" reg "<<haplotype_regs[0]<<" is not a modifiable!";
    }

    EVector haplotypes = haplotypes_ptr.list_to_vector();

    // 4. Get panel ([])
    context_ptr panel_ptr(C0, Args.reg_for_slot(4));
    EVector panel = panel_ptr.list_to_vector();

    // 5. Get sites (EVector)
    context_ptr sites_ptr(C0, Args.reg_for_slot(5));
    EVector sites = sites_ptr.list_to_vector();

    // 6. Get switching rate
    double switching_rate = evaluate_slot(C0,6).as_double();

    // 7. Get emission_diff_state
    double miscopy_prob = evaluate_slot(C0,7).as_double();

    // 8. Mixture weights = EVector of double.
    constexpr int weight_slot = 8;
    auto weights1 = evaluate_slot(C0, weight_slot).as_<EVector>();

    // 9. reads = EVector of EPair of Int
    auto arg6 = evaluate_slot(C0, 9);
    auto& reads = arg6.as_<EVector>();

    // 10. error_rate = double
    double error_rate = evaluate_slot(C0, 10).as_double();

    // 11. concentration = double
    double concentration = evaluate_slot(C0, 11).as_double();

    // 12. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 12).as_double();

    //----------- 1. Make sure that the N haplotypes are DIFFERENT -------------
    if (not all_different(K))
        return {log_double_t(1)};

    //------------- 2. Copy the context indices ------------------//

    context C1 = C0; // old weights, new haplotype

    context C2 = C0; // new weights, new haplotype

    //-------------- 3. Propose new weights in C2 ----------------//

    log_double_t w_ratio = 1;

    if (uniform() < 0.5)
    {
        w_ratio *= shift_laplace(C2, titre_regs[0], 3.0);

        if (N >= 2)
            w_ratio *= shift_laplace(C2, titre_regs[1], 0.125);
    }
    else
    {
        if (N >= 2)
            w_ratio = propose_two_titres_constant_sum(C2, titre_regs[0], titre_regs[1]);
    }

    auto weights2 = evaluate_slot(C2, weight_slot).as_<EVector>();

    //---------- 4. Sum out haplotypes and resample -----------//

    auto prob1 = resample_haps_from_panel(C1, K, haplotype_regs, haplotypes, panel, sites, switching_rate, miscopy_prob, weights1, reads, error_rate, concentration, outlier_frac);

    auto prob2 = resample_haps_from_panel(C2, K, haplotype_regs, haplotypes, panel, sites, switching_rate, miscopy_prob, weights2, reads, error_rate, concentration, outlier_frac);

    // NOTE: Unfortunately, we (probably?) can't compute the proposal probability for the haplotypes.
    //       We CAN compute the proposal probability for the haplotypes+paths.
    //       But its not clear how to to compute the probability for proposing a haplotype.

    //--------- 5. Choose which weights we will use ----------//
    auto Pr = vector<log_double_t>{prob1, w_ratio * prob2};

    int choice_index = choose(Pr);

    log_double_t ratio = 1;
    if (choice_index == 0)
        C0 = C1;
    else if (choice_index == 1)
        C0 = C2;

    if (log_verbose >= 4)
    {
        std::cerr<<"propose_weights_and_haplotypes_from_panel: choice = "<<choice_index<<"    Pr = {"<<Pr[0]<<","<<Pr[1]<<"}   ratio = "<<ratio<<"\n";
    }

    return {ratio};
}
