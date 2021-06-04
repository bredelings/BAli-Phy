#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

#include <vector>

#include "mcmc/proposals.H"
#include "math/pow2.H"
#include "math/logprod.H"
#include "util/matrix.H"
#include "util/log-level.H"
#include "util/range.H"
#include "util/math/log-double.H"
#include "alignment/alignment.H"
#include "alignment/alignment-util.H"
#include "probability/choose.H"
#include "probability/probability.H"
#include "computation/machine/args.H"
#include "computation/expression/constructor.H"
#include "computation/machine/graph_register.H"
#include "computation/context.H"

#include <Eigen/Dense>
#include <unsupported/Eigen/MatrixFunctions>

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

// Question: do we want to actually distinguish between N and -?
log_double_t li_stephens_2003_conditional_sampling_distribution(const alignment& A, const vector<int>& d, int k, double rho, double theta)
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

    // 3. Perform the forward algorithm
    int prev_column = 0;
    for(int column1=0; column1<L; column1++)
    {
        double maximum = 0;

        int column2 = column1 + 1;
        double dist = d[column1] - prev_column;
        double transition_diff_parent = (1.0-exp(-rho*dist/k))/k;
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

extern "C" closure builtin_function_li_stephens_2003_composite_likelihood(OperationArgs& Args)
{
    double rho = Args.evaluate(0).as_double();

    auto arg1 = Args.evaluate(1);
    auto& A = arg1.as_<Box<alignment>>().value();

    int n = A.n_sequences();

    auto variant_columns = find_columns(A, [&](int c){return is_variant_column(A,c) and not is_column_with_gap(A,c);});
    alignment A2 = select_columns(A, variant_columns);

    double theta_ish = li_stephens_2003_theta(n);

    log_double_t Pr = 1.0;

    for(int i=1;i<n;i++)
        Pr *= li_stephens_2003_conditional_sampling_distribution(A2, variant_columns, i, rho, theta_ish);

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

log_double_t panel_01_CSD(const EVector& panel, const EVector& sites, double switching_rate, double emission_diff_state, const EVector& haplotype)
{
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
    Matrix m(L+1,k);
    vector<int> scale(L+1,0);
    for(int i=0;i<k;i++)
        m(0,i) = 1.0/k;

    // 4. Perform the forward algorithm
    int prev_column = 0;
    for(int column1=0; column1<L; column1++)
    {
        double maximum = 0;

        // Compute the probability of going from panel haplotype i -> j.
        // Note that the switching probability includes the probability of "switching" from i->i.
        int column2 = column1 + 1;
        double dist = sites[column1].as_int() - prev_column;
        double pr_switch = (1.0-exp(-switching_rate*dist));
        double transition_diff_parent = pr_switch / k;
        double transition_same_parent = (1.0 - pr_switch) + (pr_switch / k);

        int emitted_letter = get_allele(haplotype, column1); // This will be 0 or 1, or negative for a missing value.

        bool all_previous_missing = true;
        for(int i2=0;i2<k;i2++)
        {
            // Emission is 1.0 if missing data at i2
            int copied_letter  = get_allele(panel, i2, column1);
            double emission_i2 = emission_probability( copied_letter, emitted_letter, emission_diff_state, emission_same_state, all_previous_missing );
            if (copied_letter >= 0) all_previous_missing = false;

            double total = 0;
            for(int i1=0;i1<k;i1++)
            {
                double transition_i1_i2 = (i1==i2) ? transition_same_parent : transition_diff_parent;
                total += m(column1,i1) * transition_i1_i2 * emission_i2;
            }
            if (total > maximum) maximum = total;
            m(column2,i2) = total;
        }
        prev_column = sites[column1].as_int();

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
    // 0. Population-Level Allele Frequencies (PLAF) - an EVector of double.
    auto arg0 = Args.evaluate(0);
    auto& panel = arg0.as_<EVector>();

    // 1. Population-Level Allele Frequencies (PLAF) - an EVector of double.
    auto arg1 = Args.evaluate(1);
    auto& sites = arg1.as_<EVector>();

    // 2. Switching rate
    double switching_rate = Args.evaluate(2).as_double();

    // 3. State-flipping probability
    double diff_state = Args.evaluate(3).as_double();

    // 4. Haplotypes - an EVector of EVector of Int
    auto arg4 = Args.evaluate(4);
    auto& haplotype = arg1.as_<EVector>();

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

    Args.make_changeable();

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
    // 0. Population-Level Allele Frequencies (PLAF) - an EVector of double.
    auto arg0 = Args.evaluate(0);
    auto& panel = arg0.as_<EVector>();

    // 1. Population-Level Allele Frequencies (PLAF) - an EVector of double.
    auto arg1 = Args.evaluate(1);
    auto& sites = arg1.as_<EVector>();

    // 2. Switching rate
    double switching_rate = Args.evaluate(2).as_double();

    // 3. State-flipping probability
    double diff_state = Args.evaluate(3).as_double();

    int k = panel.size();
    assert(k > 0);
    int L = panel[0].as_<EVector>().size();
    for(int i=0;i<k;i++)
        assert(panel[i].as_<EVector>().size() == L);
    assert(sites.size() == L);

    EVector haplotype(L);
    int source_haplotype = uniform_int(0,k-1);

    int prev_site = 0;
    for(int i=0; i<L; i++)
    {
        // Get an emitted letter
        int source_letter = get_allele(panel, source_haplotype, i);
        int emitted_letter = source_letter;
        if (bernoulli(diff_state) and emitted_letter >= 0)
            emitted_letter = 1 - emitted_letter;
        if (emitted_letter <= 0)
            emitted_letter = bernoulli(0.5);

        // Maybe switch to new source haplotype for next site.
        double dist = sites[i].as_int() - prev_site;
        double pr_switch = (1.0-exp(-switching_rate*dist));
        if (bernoulli(pr_switch))
            source_haplotype = uniform_int(0,k-1);

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

log_double_t site_likelihood_for_reads01(int ref, int alt, double wsaf, double error_rate, double c, double outlier_frac)
{
    assert(0 <= ref);
    assert(0 <= alt);
    assert(std::isnan(wsaf) or (0 <= wsaf and wsaf <= 1.0));
    assert(0 <= error_rate and error_rate <= 1.0);
    assert(0 <= c);

    double pi = wsaf + error_rate*(1.0 - 2.0*wsaf);

    return (1.0-outlier_frac) * beta_binomial_pdf(ref+alt, c*pi, c*(1.0-pi), alt)
           +outlier_frac      * beta_binomial_pdf(ref+alt, 1.0, 1.0, alt);
}

log_double_t site_likelihood_for_reads01(const expression_ref& data, double wsaf, double error_rate, double c, double outlier_frac)
{
    int ref = data.as_<EPair>().first.as_int();
    int alt = data.as_<EPair>().second.as_int();

    return site_likelihood_for_reads01(ref, alt, wsaf, error_rate, c, outlier_frac);
}

// Pr(D|h, w, n, \psi) where psi includes
// * e = error rate
// * c = concentration parameter for beta in beta-binomial
extern "C" closure builtin_function_probability_of_reads01(OperationArgs& Args)
{
    // 1a. Mixture weights - an EVector of double.
    auto arg0 = Args.evaluate(0);
    auto& weights = arg0.as_<EVector>();

    // 1b. Haplotypes - an EVector of EVector of Int
    auto arg1 = Args.evaluate(1);
    auto& haplotypes = arg1.as_<EVector>();

    // 1c. Reads = EVector of EPair of Int
    auto arg2 = Args.evaluate(2);
    auto& reads = arg2.as_<EVector>();

    // 1d. Error rate
    double error_rate = Args.evaluate(3).as_double();
    assert(0 <= error_rate and error_rate <= 1.0);

    // 1e. Beta concentration parameter
    double c = Args.evaluate(4).as_double();
    assert(c > 0);

    // 1f. Outlier fraction
    double outlier_frac = Args.evaluate(5).as_double();
    assert(outlier_frac >= 0 and outlier_frac <= 1);

    int num_strains = weights.size();
    assert(haplotypes.size() == weights.size());

    int num_sites = reads.size();
#ifndef NDEBUG
    for(int i=0;i<num_strains;i++)
        assert(haplotypes[i].as_<EVector>().size() == num_sites);
#endif

    // 2. Accumulate observation probabilities!
    log_double_t Pr = 1.0;
    for(int site=0; site<num_sites; site++)
    {
        double wsaf = wsaf_at_site(site, weights, haplotypes);

        Pr *= site_likelihood_for_reads01(reads[site], wsaf, error_rate, c, outlier_frac);
    }

    return { Pr };
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

matrix<log_double_t> emission_pr(const vector<int>& K, const EVector& data, const EVector& haplotypes, const EVector& weights, double error_rate, double concentration, double outlier_frac)
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

            E(site, state) = site_likelihood_for_reads01(data[site], wsaf, error_rate, concentration, outlier_frac);
        }
    }
    return E;
}

// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(data | h, w, rror_rates, c)

// Therefore, we need(h[i], i, plaf, data, h, w, error_rate, c)

extern "C" closure builtin_function_propose_haplotype_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C(M,context_index);

    // 1. IO state = int
    int io_state = Args.evaluate(1).as_int();

    auto evaluate_slot = [&](int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    // 2. Get haplotype reg
    int haplotype_reg = Args.reg_for_slot(2);
    if (auto haplotype_mod_reg = C.find_modifiable_reg(haplotype_reg))
        haplotype_reg = *haplotype_mod_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype reg "<<haplotype_reg<<" is not a modifiable!";

    // 3. Get haplotype index
    int haplotype_index = evaluate_slot(3).as_int();

    // 4. Get frequencies
    auto arg4 = evaluate_slot(4);
    auto& frequencies = arg4.as_<EVector>();

    // 5. Mixture weights = EVector of double.
    auto arg5 = evaluate_slot(5);
    auto& weights = arg5.as_<EVector>();

    // 6. data = EVector of EPair of Int
    auto arg6 = evaluate_slot(6);
    auto& data = arg6.as_<EVector>();

    // 7. haplotypes = EVector of EVector of Int
    auto arg7 = evaluate_slot(7);
    auto& haplotypes = arg7.as_<EVector>();

    // 8. error_rate = double
    double error_rate = evaluate_slot(8).as_double();

    // 9. concentration = double
    double concentration = evaluate_slot(9).as_double();

    // 10. outlier fraction = double
    double outlier_frac = evaluate_slot(10).as_double();

    int L = haplotypes[0].as_<EVector>().size();

    auto E = emission_pr({haplotype_index}, data, haplotypes, weights, error_rate, concentration, outlier_frac);

    EVector new_haplotype(L);

    log_double_t ratio = 1;
    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F0 = E(site,0)*(1.0 - f);
        auto F1 = E(site,1)*f;
        int old_allele = get_allele(haplotypes, haplotype_index, site);
        int new_allele = choose2(F0, F1);
        ratio *= (choose2_P(old_allele, F0, F1) / choose2_P(new_allele, F0, F1));
        new_haplotype[site] = new_allele;
    }

    C.set_reg_value(haplotype_reg, new_haplotype);

    return EPair(io_state+1, ratio);
}

// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(data | h, w, rror_rates, c)

// Therefore, we need(h[i], h[j], i, j, plaf, data, h, w, error_rate, c)

extern "C" closure builtin_function_propose_two_haplotypes_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C(M,context_index);

    // 1. IO state = int
    int io_state = Args.evaluate(1).as_int();

    auto evaluate_slot = [&](int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    // 2. Get h[i]
    int haplotype_reg1 = Args.reg_for_slot(2);
    if (auto haplotype_mod_reg1 = C.find_modifiable_reg(haplotype_reg1))
        haplotype_reg1 = *haplotype_mod_reg1;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype reg "<<haplotype_reg1<<" is not a modifiable!";

    // 3. Get h[j]
    int haplotype_reg2 = Args.reg_for_slot(3);
    if (auto haplotype_mod_reg2 = C.find_modifiable_reg(haplotype_reg2))
        haplotype_reg2 = *haplotype_mod_reg2;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype reg "<<haplotype_reg2<<" is not a modifiable!";

    // 4. Get haplotype index
    int haplotype_index1 = evaluate_slot(4).as_int();

    // 5. Get haplotype index
    int haplotype_index2 = evaluate_slot(5).as_int();

    // 6. Get frequencies
    auto arg4 = evaluate_slot(6);
    auto& frequencies = arg4.as_<EVector>();

    // 7. Mixture weights = EVector of double.
    auto arg5 = evaluate_slot(7);
    auto& weights = arg5.as_<EVector>();

    // 8. data = EVector of EPair of Int
    auto arg6 = evaluate_slot(8);
    auto& data = arg6.as_<EVector>();

    // 9. haplotypes = EVector of EVector of Int
    auto arg7 = evaluate_slot(9);
    auto& haplotypes = arg7.as_<EVector>();

    // 10. error_rate = double
    double error_rate = evaluate_slot(10).as_double();

    // 11. concentration = double
    double concentration = evaluate_slot(11).as_double();

    // 12. concentration = double
    double outlier_frac = evaluate_slot(12).as_double();

    log_double_t ratio = 1;

    // Make sure that the two haplotypes are DIFFERENT.
    if (haplotype_index1 == haplotype_index2) return EPair(io_state+1, ratio);

    int L = haplotypes[0].as_<EVector>().size();

    auto E = emission_pr({haplotype_index1, haplotype_index2}, data, haplotypes, weights, error_rate, concentration, outlier_frac);

    EVector new_haplotype1(L);

    EVector new_haplotype2(L);

    log_double_t pr_sample_old = 1;
    log_double_t pr_sample_new = 1;

    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F00 = E(site,0)*(1.0 - f)*(1.0-f);
        auto F01 = E(site,1)*(1.0 - f)*f;
        auto F10 = E(site,2)*(1.0 - f)*f;
        auto F11 = E(site,3)*f*f;
        auto F = vector<log_double_t>{F00,F01,F10,F11};

        int old_allele1 = get_allele(haplotypes, haplotype_index1, site);
        int old_allele2 = get_allele(haplotypes, haplotype_index2, site);
        int old_A = (old_allele2<<1)+(old_allele1);
        int new_A = choose(F);

        int new_allele1 = (new_A&1)?1:0;
        int new_allele2 = (new_A&2)?1:0;

        pr_sample_old *= choose_P(old_A, F);
        pr_sample_new *= choose_P(new_A, F);

        new_haplotype1[site] = new_allele1;
        new_haplotype2[site] = new_allele2;
    }

    C.set_reg_value(haplotype_reg1, new_haplotype1);
    C.set_reg_value(haplotype_reg2, new_haplotype2);

    ratio = pr_sample_old / pr_sample_new;

    return EPair(io_state+1, ratio);
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

// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(data | h, w, error_rates, c)

// We need x[i], h[i], i, plaf, data, h, w, error_rate, c

extern "C" closure builtin_function_propose_weights_and_haplotype_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. IO state = int
    int io_state = Args.evaluate(1).as_int();

    // 2. Get x[i]
    int titre_reg = Args.reg_for_slot(2);
    if (auto titre_mod_reg = C0.find_modifiable_reg(titre_reg))
        titre_reg = *titre_mod_reg;
    else
        throw myexception()<<"propose_weights_and_haplotype_from_plaf: titre reg "<<titre_reg<<" is not a modifiable!";

    // 3. Get h[i]
    int haplotype_reg = Args.reg_for_slot(3);
    if (auto haplotype_mod_reg = C0.find_modifiable_reg(haplotype_reg))
        haplotype_reg = *haplotype_mod_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype reg "<<haplotype_reg<<" is not a modifiable!";

    // 4. Get haplotype index
    int haplotype_index = evaluate_slot(C0, 4).as_int();

    // 5. Get frequencies
    auto arg4 = evaluate_slot(C0, 5);
    auto& frequencies = arg4.as_<EVector>();

    // 6. Mixture weights = EVector of double.
    auto weights1 = evaluate_slot(C0, 6).as_<EVector>();

    // 7. data = EVector of EPair of Int
    auto arg6 = evaluate_slot(C0, 7);
    auto& data = arg6.as_<EVector>();

    // 8. haplotypes = EVector of EVector of Int
    auto arg7 = evaluate_slot(C0, 8);
    auto& haplotypes = arg7.as_<EVector>();

    // 9. error_rate = double
    double error_rate = evaluate_slot(C0, 9).as_double();

    // 10. concentration = double
    double concentration = evaluate_slot(C0, 10).as_double();

    // 11. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 11).as_double();

    int L = haplotypes[0].as_<EVector>().size();

    //------------- Copy the context indices ------------------//

    context C1 = C0; // old weights, new haplotype

    context C2 = C0; // new weights, new haplotype

    //---------------- Propose a new weight -------------------//

    log_double_t w_ratio = shift_laplace(C2, titre_reg, 3.0);

    auto weights2 = evaluate_slot(C2, 6).as_<EVector>();

    //---------- Compute emission probabilities for the two weight vectors -----------//

    auto E1 = emission_pr({haplotype_index}, data, haplotypes, weights1, error_rate, concentration, outlier_frac);

    auto E2 = emission_pr({haplotype_index}, data, haplotypes, weights2, error_rate, concentration, outlier_frac);

    EVector new_haplotype1(L);

    log_double_t pr_sample_hap0 = 1;
    log_double_t pr_sample_hap1 = 1;
    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F0 = E1(site,0)*(1.0 - f);
        auto F1 = E1(site,1)*f;

        int old_allele = get_allele(haplotypes, haplotype_index, site);
        pr_sample_hap0 *= choose2_P(old_allele, F0, F1);

        int new_allele = choose2(F0, F1);
        pr_sample_hap1 *= choose2_P(new_allele, F0, F1);
        new_haplotype1[site] = new_allele;
    }

    EVector new_haplotype2(L);
    log_double_t pr_sample_hap2 = 1;
    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F0 = E2(site,0)*(1.0 - f);
        auto F1 = E2(site,1)*f;

        int new_allele = choose2(F0, F1);
        pr_sample_hap2 *= choose2_P(new_allele, F0, F1);
        new_haplotype2[site] = new_allele;
    }

    C1.set_reg_value(haplotype_reg, new_haplotype1);
    auto Pr1_over_Pr0 = C1.heated_probability_ratio(C0);

    // ASSUME Pr(h0)/sample_hap0 = Pr(h1)/sample_hap1
    //        Pr(h1)/Pr(h0) = sample_hap1 / sample_hap0
    assert( std::abs( log(Pr1_over_Pr0) - log(pr_sample_hap1/pr_sample_hap0) ) < 1.0e-9 );

    C2.set_reg_value(haplotype_reg, new_haplotype2);
    auto Pr2_over_Pr0 = C2.heated_probability_ratio(C0);

    auto Pr = vector<log_double_t>{Pr1_over_Pr0/pr_sample_hap1, w_ratio * Pr2_over_Pr0/pr_sample_hap2};

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

    return EPair(io_state+1, ratio);
}

// We need the markov blanket for h[i]:
//   Pr(h[i] | plaf) * Pr(data | h, w, rror_rates, c)

// Therefore, we need(x[i], x[j], h[i], h[j], i, j, plaf, data, h, w, error_rate, c)

extern "C" closure builtin_function_propose_weights_and_two_haplotypes_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. IO state = int
    int io_state = Args.evaluate(1).as_int();

    // 2. Get x[i]
    int titre1_reg = Args.reg_for_slot(2);
    if (auto titre1_mod_reg = C0.find_modifiable_reg(titre1_reg))
        titre1_reg = *titre1_mod_reg;
    else
        throw myexception()<<"propose_weights_and_haplotype_from_plaf: titre1 reg "<<titre1_reg<<" is not a modifiable!";

    // 3. Get x[j]
    int titre2_reg = Args.reg_for_slot(3);
    if (auto titre2_mod_reg = C0.find_modifiable_reg(titre2_reg))
        titre2_reg = *titre2_mod_reg;
    else
        throw myexception()<<"propose_weights_and_haplotype_from_plaf: titre2 reg "<<titre2_reg<<" is not a modifiable!";

    // 4. Get h[i]
    int haplotype1_reg = Args.reg_for_slot(4);
    if (auto haplotype_mod1_reg = C0.find_modifiable_reg(haplotype1_reg))
        haplotype1_reg = *haplotype_mod1_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype1 reg "<<haplotype1_reg<<" is not a modifiable!";

    // 5. Get h[j]
    int haplotype2_reg = Args.reg_for_slot(5);
    if (auto haplotype_mod2_reg = C0.find_modifiable_reg(haplotype2_reg))
        haplotype2_reg = *haplotype_mod2_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype2 reg "<<haplotype2_reg<<" is not a modifiable!";

    // 6. Get haplotype index
    int haplotype_index1 = evaluate_slot(C0, 6).as_int();

    // 7. Get haplotype index
    int haplotype_index2 = evaluate_slot(C0, 7).as_int();

    // 8. Get frequencies
    auto arg4 = evaluate_slot(C0, 8);
    auto& frequencies = arg4.as_<EVector>();

    // 9. Mixture weights = EVector of double.
    auto weights1 = evaluate_slot(C0, 9).as_<EVector>();

    // 10. data = EVector of EPair of Int
    auto arg6 = evaluate_slot(C0, 10);
    auto& data = arg6.as_<EVector>();

    // 11. haplotypes = EVector of EVector of Int
    auto arg7 = evaluate_slot(C0,11);
    auto& haplotypes = arg7.as_<EVector>();

    // 12. error_rate = double
    double error_rate = evaluate_slot(C0, 12).as_double();

    // 13. concentration = double
    double concentration = evaluate_slot(C0, 13).as_double();

    // 14. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 14).as_double();

    // Make sure that the two haplotypes are DIFFERENT.
    if (haplotype_index1 == haplotype_index2) return EPair(io_state+1, log_double_t(1));

    int L = haplotypes[0].as_<EVector>().size();

    //------------- Copy the context indices ------------------//

    context C1 = C0; // old weights, new haplotype

    context C2 = C0; // new weights, new haplotype

    //---------------- Propose a new weight -------------------//

    log_double_t w_ratio1 = shift_laplace(C2, titre1_reg, 3.0);

    log_double_t w_ratio2 = shift_laplace(C2, titre2_reg, 0.125);

    log_double_t w_ratio = w_ratio1 * w_ratio2;

    auto weights2 = evaluate_slot(C2, 9).as_<EVector>();

    //---------- Compute emission probabilities for the two weight vectors -----------//

    auto E1 = emission_pr({haplotype_index1, haplotype_index2}, data, haplotypes, weights1, error_rate, concentration, outlier_frac);

    auto E2 = emission_pr({haplotype_index1, haplotype_index2}, data, haplotypes, weights2, error_rate, concentration, outlier_frac);

    //---------- Sample new haplotypes for C1 -----------//
    EVector new_haplotype1_1(L);
    EVector new_haplotype1_2(L);

    log_double_t pr_sample_0 = 1;
    log_double_t pr_sample_1 = 1;

    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F00 = E1(site,0)*(1.0 - f)*(1.0-f);
        auto F01 = E1(site,1)*(1.0 - f)*f;
        auto F10 = E1(site,2)*(1.0 - f)*f;
        auto F11 = E1(site,3)*f*f;
        auto F = vector<log_double_t>{F00,F01,F10,F11};

        int old_allele1 = get_allele(haplotypes, haplotype_index1, site);
        int old_allele2 = get_allele(haplotypes, haplotype_index2, site);
        int old_A = (old_allele2<<1)+(old_allele1);
        int new_A = choose(F);

        int new_allele1 = (new_A&1)?1:0;
        int new_allele2 = (new_A&2)?1:0;

        pr_sample_0 *= choose_P(old_A, F);
        pr_sample_1 *= choose_P(new_A, F);

        new_haplotype1_1[site] = new_allele1;
        new_haplotype1_2[site] = new_allele2;
    }

    //---------- Sample new haplotypes for C2 -----------//
    EVector new_haplotype2_1(L);
    EVector new_haplotype2_2(L);

    log_double_t pr_sample_2 = 1;

    for(int site = 0; site < L; site++)
    {
        double f = frequencies[site].as_double();
        auto F00 = E2(site,0)*(1.0 - f)*(1.0-f);
        auto F01 = E2(site,1)*(1.0 - f)*f;
        auto F10 = E2(site,2)*(1.0 - f)*f;
        auto F11 = E2(site,3)*f*f;
        auto F = vector<log_double_t>{F00,F01,F10,F11};

        int new_A = choose(F);

        int new_allele1 = (new_A&1)?1:0;
        int new_allele2 = (new_A&2)?1:0;

        pr_sample_2 *= choose_P(new_A, F);

        new_haplotype2_1[site] = new_allele1;
        new_haplotype2_2[site] = new_allele2;
    }

    C1.set_reg_value(haplotype1_reg, new_haplotype1_1);
    C1.set_reg_value(haplotype2_reg, new_haplotype1_2);
    auto Pr1_over_Pr0 = C1.heated_probability_ratio(C0);

    // ASSUME Pr(h0)/sample_hap0 = Pr(h1)/sample_hap1
    //        Pr(h1)/Pr(h0) = sample_hap1 / sample_hap0
    assert( std::abs( log(Pr1_over_Pr0) - log(pr_sample_1/pr_sample_0) ) < 1.0e-9 );

    C2.set_reg_value(haplotype1_reg, new_haplotype2_1);
    C2.set_reg_value(haplotype2_reg, new_haplotype2_2);
    auto Pr2_over_Pr0 = C2.heated_probability_ratio(C0);

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

    return EPair(io_state+1, ratio);
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
//   Pr(h[i] | plaf) * Pr(data | h, w, rror_rates, c)

// Therefore, we need(x[i], x[j], h[i], h[j], i, j, plaf, data, h, w, error_rate, c)

extern "C" closure builtin_function_propose_weights_and_three_haplotypes_from_plaf(OperationArgs& Args)
{
    assert(not Args.evaluate_changeables());

    auto evaluate_slot = [&](context_ref& C, int slot) {return C.evaluate_reg(Args.reg_for_slot(slot));};

    reg_heap& M = Args.memory();

    // 0. context index = int
    int context_index = Args.evaluate(0).as_int();
    context_ref C0(M,context_index);

    // 1. IO state = int
    int io_state = Args.evaluate(1).as_int();

    // 8. Get haplotype indices
    vector<int> K = (vector<int>)evaluate_slot(C0,8).as_<EVector>();

    int N = K.size();

    int n_states = (1<<N);

    // 2. Get x[i]
    vector<int> titre_regs(N);

    titre_regs[0] = Args.reg_for_slot(2);
    titre_regs[1] = Args.reg_for_slot(3);
    titre_regs[2] = Args.reg_for_slot(4);

    for(int i=0; i<N; i++)
    {
        int& r = titre_regs[i];
        if (auto titre_mod_reg = C0.find_modifiable_reg(r))
            r = *titre_mod_reg;
        else
            throw myexception()<<"propose_weights_and_haplotype_from_plaf: titre reg"<<i+1<<" "<<r<<" is not a modifiable!";
    }

    // 5. Get h[i]
    vector<int> haplotype_regs(N);

    haplotype_regs[0] = Args.reg_for_slot(5);
    if (auto haplotype_mod1_reg = C0.find_modifiable_reg(haplotype_regs[0]))
        haplotype_regs[0] = *haplotype_mod1_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype1 reg "<<haplotype_regs[0]<<" is not a modifiable!";

    // 6. Get h[j]
    haplotype_regs[1] = Args.reg_for_slot(6);
    if (auto haplotype_mod2_reg = C0.find_modifiable_reg(haplotype_regs[1]))
        haplotype_regs[1] = *haplotype_mod2_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype2 reg "<<haplotype_regs[1]<<" is not a modifiable!";

    // 7. Get h[k]
    haplotype_regs[2] = Args.reg_for_slot(7);
    if (auto haplotype_mod3_reg = C0.find_modifiable_reg(haplotype_regs[2]))
        haplotype_regs[2] = *haplotype_mod3_reg;
    else
        throw myexception()<<"propose_haplotype_from_plaf: haplotype3 reg "<<haplotype_regs[2]<<" is not a modifiable!";

    // 9. Get frequencies
    auto arg4 = evaluate_slot(C0, 9);
    auto& frequencies = arg4.as_<EVector>();

    // 10. Mixture weights = EVector of double.
    constexpr int weight_slot = 10;
    auto weights1 = evaluate_slot(C0, weight_slot).as_<EVector>();

    // 11. data = EVector of EPair of Int
    auto arg6 = evaluate_slot(C0, 11);
    auto& data = arg6.as_<EVector>();

    // 12. haplotypes = EVector of EVector of Int
    auto arg7 = evaluate_slot(C0,12);
    auto& haplotypes = arg7.as_<EVector>();

    // 13. error_rate = double
    double error_rate = evaluate_slot(C0, 13).as_double();

    // 14. concentration = double
    double concentration = evaluate_slot(C0, 14).as_double();

    // 15. outlier_frac = double
    double outlier_frac = evaluate_slot(C0, 15).as_double();

    //----------- Make sure that the N haplotypes are DIFFERENT -------------
    if (not all_different(K))
        return EPair(io_state+1, log_double_t(1));

    int L = haplotypes[0].as_<EVector>().size();

    //------------- Copy the context indices ------------------//

    context C1 = C0; // old weights, new haplotype

    context C2 = C0; // new weights, new haplotype

    //---------------- Propose a new weight -------------------//

    log_double_t w_ratio1 = shift_laplace(C2, titre_regs[0], 3.0);

    log_double_t w_ratio2 = shift_laplace(C2, titre_regs[1], 0.125);

    log_double_t w_ratio = w_ratio1 * w_ratio2;

    auto weights2 = evaluate_slot(C2, weight_slot).as_<EVector>();

    //---------- Compute emission probabilities for the two weight vectors -----------//

    auto E1 = emission_pr(K, data, haplotypes, weights1, error_rate, concentration, outlier_frac);

    auto E2 = emission_pr(K, data, haplotypes, weights2, error_rate, concentration, outlier_frac);

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
        std::cerr<<"propose_weights_and_three_haplotypes: choice = "<<choice_index<<"\n";
    }

    return EPair(io_state+1, ratio);
}


