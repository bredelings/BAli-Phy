#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include "matrix.H"
#include "math/log-double.H"
using std::vector;


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

vector<double> get_bin_times(int n, double eta)
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

Matrix get_transition_probabilities(int n, double rho, double theta)
{
    Matrix P(n,n);
    return P;
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

log_double_t smc(const double rho, double theta, const matrix<int>& data)
{
    assert(rho >= 0);
    assert(theta > 0);
    assert(data.size1() == 2);

    // How many bins
    const int n_bins = 100;

    // Lower end of each bin. boundaries[0] = 0. The upper end of the last bin is \infty
    auto bin_boundaries = get_bin_boundaries(n_bins, 2.0/theta);

    auto bin_times = get_bin_times(n_bins, 2.0/theta);

    auto pi = get_equilibrium(bin_boundaries, 2.0/theta);

    auto transition = get_transition_probabilities(n_bins, rho, theta);

    auto emission_probabilities = get_emission_probabilities(bin_times);

    vector<log_double_t> L(n_bins, 1.0);
    vector<log_double_t> L2(n_bins);

    for(int l=0; l < data.size2(); l++)
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

    log_double_t Pr = 1;
    for(int i=0; i < n_bins; i++)
	Pr += L[i];
    return Pr;
}
