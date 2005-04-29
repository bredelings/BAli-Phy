#include "probability.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>

#include "logsum.H"

using std::valarray;

log_double_t double_factorial(int n) 
{
  log_double_t x = 1;
  for(int i=3;i<=n;i+=2)
    x *= i;
  return x;
}

log_double_t num_branches(int n) {
  return 2*n-3;
}

log_double_t num_topologies(int n) {
  return double_factorial(2*n-5);
}

log_double_t num_topologies_in_partition(int n1,int n2) 
{
  log_double_t total = num_topologies(n1) * num_topologies(n2);
  total *= num_branches(n1) * num_branches(n2);
  return total;
}

double log_gamma(double x) {
  return gsl_sf_lngamma(x);
}

log_double_t dirichlet_pdf(const valarray<double>& p,const valarray<double>& n) {
  assert(p.size() == n.size());

  log_double_t Pr = 1;
  for(int i=0;i<p.size();i++) 
  {
    Pr *= pow<log_double_t>(p[i],n[i]);

    // This is because we our proposal is symmetric on the scale of log(p[i]).
    Pr *= exp<log_double_t>(p[i]);
  }

  // This term is constant in p
  Pr.log() += log_gamma(n.sum()+n.size());
  for(int i=0;i<p.size();i++)
    Pr.log() -= log_gamma(n[i]+1);

  return Pr;
}

log_double_t dirichlet_pdf(const valarray<double>& p,const valarray<double>& q,
			 double N) {
  return dirichlet_pdf(p,q*N);
}

/// log density for y if y=ln (x+delta), and x ~ Exp(mu)

/// f(x) = exp(-x/mu)/mu   g(y) = exp(-(exp(y)-delta)/mu)/mu * exp(y)
log_double_t exp_exponential_pdf(double y, double mu, double delta) {
  double x = exp(y)-delta;
  assert(x >= 0);

  return exp<log_double_t>(-x/mu + y)/mu;
}

log_double_t exponential_pdf(double x, double mu) {
  return exp<log_double_t>(-x/mu)/mu;
}

log_double_t shift_laplace_pdf(double x, double mu, double sigma) {
  double a = sigma/sqrt(2);
  return gsl_ran_laplace_pdf(x-mu,a);
}

log_double_t beta_pdf(double p,double f, double N) {
    const double a  = 1.0 + N * f;
    const double b  = 1.0 + N * (1.0 - f);

    if (p <= 0.0 or p >= 1.0)
      return 0;
    else
      return gsl_ran_beta_pdf(p,a,b);
}
