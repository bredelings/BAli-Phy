#include "rates.H"

#include <valarray>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include "probability.H"
#include "rng.H"
#include "util.H"

namespace substitution {

  using std::vector;
  using std::valarray;
  using std::string;
  /*--------------- RateDistribution -----------------*/

  double RateDistribution::pdf(double x,double dx) const {
    double x1 = x-0.5*dx;
    double x2 = x+0.5*dx;
    if (x1<0) x1=0;
    return (cdf(x2)-cdf(x1))/(x2-x1);
  }

  // f(x) = cdf(x)-p, monotonically increasing from -p (@0) to 1-p (@ \infty)
  double RateDistribution::quantile(double p, double tol) const {
    assert(0.0 <= p and p <= 1.0);

    double x = 1.0;

    // We know the zero is in (min,max), if max>=min
    double min=0.0;
    double max=-1;

    const int max_iterations = 2000;
    int iterations=0;

    double dx = 0.001;
    while(std::abs(dx) > tol) {
      double f = cdf(x)-p;

      // take advantage of the monotonicity
      if (f < 0)
	min = x;
      else if (f > 0)
	max = x;
      else return x;

      // propose do Newton-Raphson step
      double dfdx = pdf(x,std::abs(dx/100));
      dx = -f/dfdx;

      // deal with trying to jump out of (min,max)
      double x2 = x;
      if (x2+dx < min)
	x2 = (x2+min)/2.0;
      else if (x+dx > max and max >= 0)
	x2 = (x2+max)/2.0;
      else
	x2 = x2 + dx;
      iterations++;

      double f2 = cdf(x2) - p;
      // move to x2 if its an improvement
      if (std::abs(f2) <= std::abs(f)) 
	x = x2;
      // If x2 is NOT an improvement then...
      else {
	assert(min <= x2);
	assert(max < 0 or x2 <= max);

	// ...update (min,max), ...
	if (f2 < 0)
	  min = x2;
	else if (f2 > 0)
	  max = x2;
	else
	  return x2;

	assert(max >= 0);

	if (max > 0) {
	  // ...propose another location, ...
	  x2 = 0.5*(min + max);

	  f2 = cdf(x2) - p;
	  if (f2 < 0)
	    min = x2;
	  else if (f2 > 0)
	    max = x2;
	  else
	    return x2;
	  
	  // and try to move there.
	  if (std::abs(f2) < std::abs(f))
	    x = x2;
	}

      }

      assert(min <= x);
      if (max >= 0) {
	assert(min <= max);
	assert(x <= max);
	assert(cdf(min) <= p and p <= cdf(max));
      }
      assert(iterations<max_iterations);
    }
    return x;
  }

  RateDistribution::~RateDistribution() {}

  /*--------------- UniformRateDistribution -----------------*/

  string Uniform::name() const {
    return "Uniform";
  }

  string Uniform::parameter_name(int) const {
    std::abort();
  }

  Uniform::~Uniform() {}

  /*--------------- GammaRateDistribution -----------------*/
  
  double Gamma::prior() const {
    double g_sigma = parameters_[0];
    double log_g_sigma = log(g_sigma);
    return log(shift_laplace_pdf(log_g_sigma,-4,0.5));
  }

  void Gamma::fiddle() {
    if (fixed[0]) return;

    vector<double> v = parameters_;
    double g_sigma = v[0];
    double log_g_sigma = log(g_sigma);

    const double sigma = 0.25;
    log_g_sigma += gaussian(0,sigma);

    g_sigma = exp(log_g_sigma);
    double alpha = 1.0/(g_sigma*g_sigma);
    if (alpha < 100000)
      v[0] = g_sigma;

    parameters(v);
  }

  string Gamma::name() const {
    return "gamma";
  }

  string Gamma::parameter_name(int i) const {
    return "gamma:sigma\\mu";
  }

  double Gamma::cdf(double x) const {
    double a = 1.0/(parameters_[0]*parameters_[0]);
    double b = 1.0/a;

    return gsl_cdf_gamma_P(x,a,b);
  }

  double Gamma::pdf(double x,double) const {
    double a = 1.0/(parameters_[0]*parameters_[0]);
    double b = 1.0/a;

    return gsl_ran_gamma_pdf(x,a,b);
  }

  double Gamma::quantile(double p,double) const {
    double a = 1.0/(parameters_[0]*parameters_[0]);
    double b = 1.0/a;
    
    return gsl_cdf_gamma_Pinv(p,a,b);
  }



  /*-------------- LogNormal Distribution ----------------*/
  // E X = exp(lmu + 0.5*lsigma^2)
  // Var X = (exp(lsigma^2)-1) - exp(2*lmu + lsigma^2)

  // EX==1 => lmu = -0.5 * lsigma^2
  //          Var X = (exp(lsigma^2)-1  => log(Var X + 1) = lsigma^2

  double LogNormal::prior() const {
    const double mean_stddev = 0.01;
    return log(mean_stddev) - parameters_[0]/mean_stddev;
  }

  void LogNormal::fiddle() {
    vector<double> v = parameters_;
    double& p = v[0];
 
    const double sigma = 0.04;
    double p2 = p + gaussian(0,sigma);
    if (p2 < 0) p2 = -p2;

    parameters(v);
  }

  double LogNormal::cdf(double x) const {
    double Var = parameters_[0]*parameters_[0];
    double lVar = log(Var+1.0);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;

    return gsl_cdf_lognormal_P(x,lmu,lsigma);
  }

  double LogNormal::pdf(double x,double) const {
    double Var = parameters_[0]*parameters_[0];
    double lVar = log(Var+1.0);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;

    return gsl_ran_lognormal_pdf(x,lmu,lsigma);
  }

  double LogNormal::quantile(double P,double) const {
    double Var = parameters_[0]*parameters_[0];
    double lVar = log(Var+1.0);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;


    return gsl_cdf_lognormal_Pinv(P,lmu,lsigma);
  }

  string LogNormal::name() const {
    return "log-normal";
  }

  string LogNormal::parameter_name(int i) const {
    return "log-normal:sigma/mu";
  }

  /*-------------- MultipleDistribution ----------------*/

  void MultipleDistribution::super_fiddle() {
    read();

    const double sigma = 0.05;

    // Read, fiddle, and set f
    valarray<double> f(n_dists());
    for(int i=0;i<f.size();i++)
      f[i] = fraction(i);

    dirichlet_fiddle(f,sigma);

    for(int i=0;i<f.size();i++)
      fraction(i) = f[i];
  }

  double MultipleDistribution::super_prior() const {
    // Read, fiddle, and set f
    valarray<double> f(n_dists());
    for(int i=0;i<f.size();i++)
      f[i] = fraction(i);

    valarray<double> q(1.0/f.size(),f.size());

    return dirichlet_log_pdf(f,q,10);
  }

  double MultipleDistribution::cdf(double x) const {
    double P=0;
    for(int i=0;i<n_dists();i++) 
      P += fraction(i) * SubModels(i).cdf(x);

    return P;
  }

  double MultipleDistribution::pdf(double x,double dx) const {
    double density=0;
    for(int i=0;i<n_dists();i++) 
      density += fraction(i) * SubModels(i).pdf(x,dx);

    return density;
  }

  MultipleDistribution::MultipleDistribution(const std::vector<OwnedPointer<RateDistribution> >& models) 
    :SuperDerivedModelOver<RateDistribution,RateDistribution>(models,models.size())
  {
    // Set the rates and fractions
    for(int i=0;i<n_dists();i++)
      parameters_[i] = 1.0/n_dists();

    recalc();
  }

  string MultipleDistribution::name() const {
    string n="multi:(";
    for(int i=0;i<n_submodels();i++) {
      n += SubModels(i).name();
      if (i+1 < n_submodels())
	n += " + ";
    }
    n += ")";
    return n;
  }

  string MultipleDistribution::super_parameter_name(int i) const {
    string n = "multi:p";
    n += convertToString(i);
    return n;
  }


}
