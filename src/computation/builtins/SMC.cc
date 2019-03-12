#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

#include <vector>

#include "util/matrix.H"
#include "util/range.H"
#include "util/math/log-double.H"
#include "alignment/alignment.H"
#include "computation/machine/args.H"

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

// We need emission probabilities for 2*t, since t is the depth of the tree,
// but the distance between the tips is 2*t.

// So... this should be 0.25*JC_transition_p( ), but maybe leaving out the
// 0.25 doesn't hurt, since its a constant factor.

vector<Matrix> get_emission_probabilities(const vector<double>& t)
{
    vector<Matrix> E(t.size());
    for(int i=0;i<E.size();i++)
	E[i] = JC_transition_p(2.0 * t[i]);
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

EMatrix get_no_snp_matrix(const Matrix& T, const vector<Matrix>& emission)
{
    // This is only valid for Jukes-Cantor
    int n_bins = T.size1();
    EMatrix M(n_bins,n_bins);
    for(int j=0;j<n_bins;j++)
	for(int k=0; k<n_bins; k++)
	    M(j,k) = T(j,k) * emission[k](0,0);
    return M;
}

EMatrix get_snp_matrix(const Matrix& T, const vector<Matrix>& emission)
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

log_double_t smc(double rho_over_theta, vector<double> coalescent_rates, vector<double> level_boundaries, const alignment& A)
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

    const auto emission_probabilities = get_emission_probabilities(bin_times);

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

    for(auto& group: classify_sites(A))
    {
	if (group.second == site_t::missing)
	    smc_group(L, L2, scale, missing, group.first);
	else if (group.second == site_t::mono)
	    smc_group(L, L2, scale, no_snp, group.first);
	else if (group.second == site_t::poly)
	    smc_group(L, L2, scale, snp, group.first);
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

    auto coalescent_rates_ = Args.evaluate(1).as_<EVector>();

    auto level_boundaries_ = Args.evaluate(2).as_<EVector>();

    vector<double> coalescent_rates;
    for(auto& r: coalescent_rates_)
	coalescent_rates.push_back(r.as_double());

    vector<double> level_boundaries;
    level_boundaries.push_back(0);
    for(auto& l: level_boundaries_)
	level_boundaries.push_back(l.as_double());

    auto a = Args.evaluate(3);
    auto& A = a.as_<Box<alignment>>().value();

    return { smc(rho_over_theta, coalescent_rates, level_boundaries, A) };
}
