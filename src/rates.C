#include "rates.H"

#include <valarray>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include "probability.H"
#include "rng.H"

namespace substitution {

  using std::vector;
  using std::valarray;
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


  /*--------------- GammaRateDistribution -----------------*/
  
  double Gamma::prior() const {
    double g_sigma = parameters_[0];
    double log_g_sigma = log(g_sigma);
    return log(shift_laplace_pdf(log_g_sigma,-4,0.5));
  }

  void Gamma::fiddle(const std::valarray<bool>& fixed) {
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

  void LogNormal::fiddle(const std::valarray<bool>& fixed) {
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

  /*-------------- MultipleDistribution ----------------*/

  double MultipleDistribution::cdf(double x) const {
    double P=0;
    for(int i=0;i<distributions.size();i++) 
      P += fraction(i) * distributions[i]->cdf(x*rate(i));

    return P;
  }

  double MultipleDistribution::pdf(double x,double dx) const {
    double density=0;
    for(int i=0;i<distributions.size();i++) 
      density += fraction(i) * distributions[i]->pdf(x*rate(i),dx)/rate(i);

    return density;
  }

  double MultipleDistribution::prior() const {
    double Pr =0;
    for(int i=0;i<distributions.size();i++)
      Pr += distributions[i]->prior();

    // I don't think we need to do anything for the Dirichlet priors... (?)
    return Pr;
  }


  void dirichlet_fiddle(valarray<double>& v, double sigma) {

    // proposal new point in 
    for(int i=0;i<v.size();i++) {
      v[i] += gaussian(0,sigma/v.size());
      if (v[i] < 0)
	v[i] = -v[i];
      else if (v[i] > 1.0)
	v[i] = 1.0 - v[i];
    }

    double sum=0;
    for(int i=0;i<v.size();i++)
      sum += v[i];
    
    for(int i=0;i<v.size();i++)
      v[i] = v[i]/sum;
  }

  void MultipleDistribution::read() {
    // Get the sub-distribution parameters
    for(int i=0, j=2*distributions.size();i<distributions.size();i++) {
      for(int k=0;k<distributions[i]->parameters().size();k++)
	parameters_[j++] = distributions[i]->parameters()[k];
    }
  }


  void MultipleDistribution::write() {
    // Get the sub-distribution parameters
    for(int i=0,j=2*distributions.size();i<distributions.size();i++) {
      vector<double> temp(distributions[i]->parameters().size());
      for(int k=0;k<temp.size();k++)
	temp[k] = parameters_[j++];
      distributions[i]->parameters(temp);
    }
  }

  std::valarray<bool> subvector(const std::valarray<bool>& v1,int s, int l) {
    std::valarray<bool> v2(l);
    for(int i=0;i<l;i++)
      v2[i] = v1[s+i];
    return v2;
  }

  void MultipleDistribution::fiddle(const std::valarray<bool>& fixed) {

    for(int i=0,start=0;
	i<distributions.size();
	start+=distributions[i]->parameters().size(),
	  i++)
      distributions[i]->fiddle(subvector(fixed,
					 start,
					 distributions[i]->parameters().size()
					 )
			       );

    read();

    const double sigma = 0.05;

    // Read, fiddle, and set f
    valarray<double> f(ndists());
    for(int i=0;i<f.size();i++)
      f[i] = fraction(i);

    dirichlet_fiddle(f,sigma);

    for(int i=0;i<f.size();i++)
      fraction(i) = f[i];
    
    // Read, fiddle, and set f
    valarray<double> r(ndists());
    for(int i=0;i<r.size();i++)
      r[i] = rate(i);

    r /= r.sum();

    dirichlet_fiddle(r,sigma);

    double meanrate = 0;
    for(int i=0;i<ndists();i++)
      meanrate += fraction(i) * r[i];

    r /= meanrate;

    for(int i=0;i<r.size();i++)
      rate(i) = r[i];
  }

  void MultipleDistribution::parameters(const vector<double>& p) {
    RateDistribution::parameters(p);
    write();
  }

  MultipleDistribution& MultipleDistribution::operator=(const MultipleDistribution& M) {
    RateDistribution::operator=(M);

    distributions = M.distributions;
    for(int i=0;i<distributions.size();i++) {
      if (distributions[i])
	delete distributions[i];
      distributions[i] = distributions[i]->clone();
    }

    return (*this);
  }
  
  MultipleDistribution::MultipleDistribution(const MultipleDistribution& M)
    :RateDistribution(M)
  {
    distributions = M.distributions;
    for(int i=0;i<distributions.size();i++) 
      distributions[i] = distributions[i]->clone();
  }


  int nparams(const vector<const RateDistribution*>& d) {
    int n=0;
    for(int i=0;i<d.size();i++) {
      n += d[i]->parameters().size();
    }
    return n;
  }
  
  MultipleDistribution::MultipleDistribution(const vector<const RateDistribution*>& d)
    :RateDistribution(2*d.size() + nparams(d)),
     distributions(d.size())
  {
    // Get local copies of the distributions
    for(int i=0;i<distributions.size();i++)
      distributions[i] = d[i]->clone();
    
    // Set the rates and fractions
    for(int i=0;i<ndists();i++) {
      parameters_[i] = 1.0/distributions.size();
      parameters_[i + ndists()] = 1.0/distributions.size();
    }

    double meanrate = 0;
    for(int i=0;i<ndists();i++)
      meanrate += fraction(i) * rate(i);

    for(int i=0;i<ndists();i++)
      rate(i) /= meanrate;

    // Get the sub-distribution parameters
    read();

  }

  MultipleDistribution::~MultipleDistribution() 
  {
    for(int i=0;i<distributions.size();i++)
      delete distributions[i];
  }

}
