#include "probability.H"
#include <gsl/gsl_randist.h>

using std::valarray;

double dirichlet_log_pdf(const valarray<double>& p,const valarray<double>& n) {
  assert(p.size() == n.size());

  double Pr = 0;
  for(int i=0;i<p.size();i++) {
    Pr += n[i]*log(p[i]);

    // This is because we our proposal is symmetric on the scale of log(p[i]).
    Pr += p[i];
  }

  // This term is constant in p
  //  Pr += log_gamma(n.sum()+n.size());
  //  for(int i=0;i<p.size();i++)
  //    Pr -= log_gamma(n[i]+1);

  return Pr;
}

double dirichlet_log_pdf(const valarray<double>& p,const valarray<double>& q,
			 double N) {
  return dirichlet_log_pdf(p,q*N);
}

/// log density for y if y=ln (x+delta), and x ~ Exp(mu)

/// f(x) = exp(-x/mu)/mu   g(y) = exp(-(exp(y)-delta)/mu)/mu * exp(y)
double exp_exponential_log_pdf(double y, double mu, double delta) {
  double x = exp(y)-delta;
  assert(x >= 0);
  return -log(mu) -x/mu + y;
}

double exp_exponential_pdf(double y, double mu, double delta) {
  return exp(exp_exponential_log_pdf(y,mu));
}

double shift_laplace_pdf(double x, double mu, double sigma) {
  double a = sigma/sqrt(2);
  return gsl_ran_laplace_pdf(x-mu,a);
}

