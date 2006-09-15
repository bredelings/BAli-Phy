#include "rates.H"

#include <cmath>
#include <valarray>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>
#include "probability.H"
#include "util.H"

namespace substitution {

  using std::cerr;
  using std::endl;
  using std::vector;
  using std::valarray;
  using std::string;

  //--------------- RateDistribution -----------------//

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
    while(std::abs(dx) > tol and iterations < max_iterations) {
      double f = cdf(x)-p;

      // take advantage of the monotonicity
      if (f < 0)
	min = x;
      else if (f > 0)
	max = x;
      else return x;

      // propose do Newton-Raphson step
      double dfdx = pdf(x);
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

  Uniform::~Uniform() {}

  //--------------- GammaRateDistribution -----------------//
  
  efloat_t Gamma::prior() const {
    double g_sigma = parameters_[0];
    double log_g_sigma = log(g_sigma);
    return laplace_pdf(log_g_sigma,-4,0.5);
  }

  string Gamma::name() const {
    return "gamma";
  }

  double Gamma::cdf(double x) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 3.0);

    double M = 1;
    double V = (s*M)*(s*M);

    double a = 1.0/(s*s);
    double b = M/a;

    if (a < 1000)
      return gsl_cdf_gamma_P(x,a,b);
    else {
      double sigma2 =  log1p(V/(M*M));
      double mu = log(M) - sigma2/2.0;
      double sigma = sqrt(sigma2);

      // don't go crazy
      sigma = minmax(sigma, 1.0e-5, 1.0e5);

      return gsl_cdf_lognormal_P(x,mu,sigma);
    }
  }

  double Gamma::pdf(double x) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 3.0);

    double M = 1;
    double V = (s*M)*(s*M);

    double a = 1.0/(s*s);
    double b = M/a;

    if (a < 1000)
      return gsl_ran_gamma_pdf(x,a,b);
    else {
      double sigma2 =  log1p(V/(M*M));
      double mu = log(M) - sigma2/2.0;
      double sigma = sqrt(sigma2);

      // don't go crazy
      sigma = minmax(sigma, 1.0e-5, 1.0e5);

      return gsl_ran_lognormal_pdf(x,mu,sigma);
    }
  }

  double Gamma::quantile(double p,double) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 3.0);

    double M = 1;
    double V = (s*M)*(s*M);

    double a = 1.0/(s*s);
    double b = M/a;

    if (a < 1000)
      return gsl_cdf_gamma_Pinv(p,a,b);
    else {
      double sigma2 =  log1p(V/(M*M));
      double mu = log(M) - sigma2/2.0;
      double sigma = sqrt(sigma2);

      // don't go crazy
      sigma = minmax(sigma, 1.0e-5, 1.0e5);

      return gsl_cdf_lognormal_P(p,mu,sigma);
    }
  }

  Gamma::Gamma() {
    add_parameter("gamma::sigma/mu", 0.1);
  }

  //--------------- GammaRateDistribution -----------------//

  efloat_t Beta::prior() const 
  {
    double mu = parameters_[0];
    double s  = parameters_[1];

    double a = (1 - mu - mu*s*s)/(s*s);
    double b = a * (1-mu)/mu;

    if (a<1.0 or b<1.0)
      return 0.0;

    efloat_t P = 1;
    P *= beta_pdf(mu,10,1);
    P *= laplace_pdf(s,-4,0.5);

    return P;
  }

  string Beta::name() const {
    return "beta";
  }

  double Beta::cdf(double x) const {
    double mu = parameters_[0];
    double  s = parameters_[1];

    double a = (1 - mu - mu*s*s)/(s*s);
    double b = a * (1-mu)/mu;

    if (a<0 or b<0)
      a=b=1;

    double r = 100.0/std::max(a,b);
    if (r < 1) {
      a *= r;
      b *= r;
    }

    return gsl_cdf_beta_P(x,a,b);
  }

  double Beta::pdf(double x) const {
    double mu = parameters_[0];
    double  s = parameters_[1];

    double a = (1 - mu - mu*s*s)/(s*s);
    double b = a * (1-mu)/mu;

    if (a<0 or b<0)
      a=b=1;

    double r = 100.0/std::max(a,b);
    if (r < 1) {
      a *= r;
      b *= r;
    }

    return gsl_ran_beta_pdf(x,a,b);
  }

  double Beta::quantile(double p,double) const
  {
    double mu = parameters_[0];
    double  s = parameters_[1];

    double a = (1 - mu - mu*s*s)/(s*s);
    double b = a * (1-mu)/mu;

    if (a<0 or b<0)
      a=b=1;

    double r = 100.0/std::max(a,b);
    if (r < 1) {
      a *= r;
      b *= r;
    }

    return gsl_cdf_beta_Pinv(p,a,b);
  }

  Beta::Beta()
  {
    add_parameter("beta::mu", 0.5);
    add_parameter("beta::sigma/mu", 0.1);
  }

  //-------------- LogNormal Distribution ----------------//
  // E X = exp(lmu + 0.5*lsigma^2)
  // Var X = (exp(lsigma^2)-1) - exp(2*lmu + lsigma^2)

  // EX==1 => lmu = -0.5 * lsigma^2
  //          Var X = (exp(lsigma^2)-1  => log(Var X + 1) = lsigma^2

  efloat_t LogNormal::prior() const {
    double g_sigma = parameters_[0];
    double log_g_sigma = log(g_sigma);
    return laplace_pdf(log_g_sigma,-4,0.5);
  }

  double LogNormal::cdf(double x) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 1.0e5);

    double Var = s*s;
    double lVar = log1p(Var);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;

    // don't go crazy
    lsigma = minmax(lsigma, 1.0e-5, 1.0e5);

    return gsl_cdf_lognormal_P(x,lmu,lsigma);
  }

  double LogNormal::pdf(double x) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 1.0e5);

    double Var = s*s;
    double lVar = log1p(Var);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;

    // don't go crazy
    lsigma = minmax(lsigma, 1.0e-5, 1.0e5);

    return gsl_ran_lognormal_pdf(x,lmu,lsigma);
  }

  double LogNormal::quantile(double P,double) const 
  {
    double s = minmax(parameters_[0], 1.0e-5, 1.0e5);

    double Var = s*s;
    double lVar = log1p(Var);

    double lsigma = sqrt(lVar);
    double lmu = -0.5 * lVar;

    // don't go crazy
    lsigma = minmax(lsigma,1.0e-5,1.0e5);

    return gsl_cdf_lognormal_Pinv(P,lmu,lsigma);
  }

  string LogNormal::name() const {
    return "log-normal";
  }

  LogNormal::LogNormal() 
  {
    add_parameter("log-normal::sigma/mu", 0.1);
  }
    

  //-------------- MultipleDistribution ----------------//

  efloat_t MultipleDistribution::super_prior() const 
  {
    valarray<double> f(n_dists());
    for(int i=0;i<f.size();i++)
      f[i] = fraction(i);

    // uniform - 1 observeration per bin
    return dirichlet_pdf(f,1);
  }

  double MultipleDistribution::cdf(double x) const {
    double P=0;
    for(int i=0;i<n_dists();i++) 
      P += fraction(i) * SubModels(i).cdf(x);

    return P;
  }

  double MultipleDistribution::pdf(double x) const {
    double density=0;
    for(int i=0;i<n_dists();i++) 
      density += fraction(i) * SubModels(i).pdf(x);

    return density;
  }

  MultipleDistribution::MultipleDistribution(const std::vector<OwnedPointer<RateDistribution> >& models) 
  {
    // Set the rates and fractions
    for(int i=0;i<n_dists();i++) {
      string pname = string("multi:p")+convertToString(i+1);
      add_parameter(pname, 1.0/n_dists() );
    }

    for(int i=0;i<models.size();i++)
      add_submodel(convertToString(i+1),*models[i]);
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

  /// Choose boundaries between bins based on quantiles
  vector<double> uniform_boundaries(const vector<double>& r, const RateDistribution& D)
  {
    vector<double> b(r.size()-1);
    
    for(int i=0;i<b.size();i++)
      b[i] = D.quantile( (D.cdf(r[i]) + D.cdf(r[i+1]))/2.0 ) ;
    
    return b;
  }
  
  /// Choose boundaries between bins based on the log rates
  vector<double> log_boundaries(const vector<double>& r)
  {
    vector<double> b(r.size()-1);
    for(int i=0;i<b.size();i++)
      b[i] = sqrt(r[i]*r[i+1]);
    
    return b;
  }
  
  /// Compute the probability of each bin from the bin boundaries
  vector<double> get_fractions(const vector<double>& b,const RateDistribution& D)
  {
    vector<double> f(b.size()+1);
    f[0] = D.cdf(b.front());
    for(int i=1;i<b.size();i++) 
      f[i] = D.cdf(b[i])-D.cdf(b[i-1]);
    f.back() = 1-D.cdf(b.back());
    return f;
  }
  
  double Discretization::operator()(double x) const
  {
    for(int i=0;i<b.size();i++)
      if (x < b[i]) return r[i];
    
    return r.back();
  }
  
  double Discretization::moment(double p) const 
  {
    double m=0;
    for(int i=0;i<size();i++)
      m += f[i]*pow(r[i],p);
    return m;
  }
  
  
  void Discretization::scale(double S)
  {
    for(int i=0;i<r.size();i++)
      r[i] *= S;
  }

  double Discretization::scale() const
  {
    double S=0;
    for(int i=0;i<size();i++)
      S += f[i]*r[i];
    return S;
  }
  
  
  Discretization::Discretization(int N,const RateDistribution& D,double a)
    :p(N),r(N),A(a)
  {
    for(int i=0;i<N;i++) {
      double p1 = (2.0*i+1)/(2.0*N);
      p[i] = gsl_cdf_beta_P(p1,A,A);
    }
    
    for(int i=0;i<N;i++) 
      r[i] = D.quantile(p[i]);

    b = log_boundaries(r);

    f = get_fractions(b,D);

    // check for errors
    bool error=false;

    for(int i=0;i<f.size();i++)
      if ((not std::isfinite(f[i])) or (not std::isfinite(r[i])))
	error=true;

    if (error) {
      show_parameters(cerr,D);
      for(int i=0;i<r.size();i++)
	cerr<<"r["<<i<<"] = "<<r[i]<<endl;
      cerr<<endl;
      for(int i=0;i<f.size();i++)
	cerr<<"f["<<i<<"] = "<<f[i]<<endl;
      std::abort();
    }
  }
  
  
  
  UniformDiscretization::UniformDiscretization(int N):Discretization(N) { }
    
  UniformDiscretization::UniformDiscretization(int N, const RateDistribution& D)
      :Discretization(N,D,1)
  {
    b = uniform_boundaries(r,D);
    f = get_fractions(b,D);
  }
  
  
  double Discretization::error(double (*g)(double x),const RateDistribution& D) const
  {
    int N2 = (size()+2)*20;
    if (N2 < 200) N2 = 200;
    Discretization d2(N2,D);
    
    double E=0;
    for(int i=0;i<d2.size();i++) {
      double x = d2.r[i];
      double e = std::abs(g(x) - g((*this)(x)));
      E += d2.f[i]*e;
    }
    return E;
  }
  
  double Discretization::error2(double (*g)(double x),const RateDistribution& D) const
  {
    int N2 = (size()+2)*20;
    if (N2 < 200) N2 = 200;
    Discretization d2(N2,D);
    
    double E=0;
    for(int i=0;i<d2.size();i++) {
      double x = d2.r[i];
      double e = std::abs( g(x) - g((*this)(x)));
      e = e*e;
      E += d2.f[i]*e;
    }
    return E;
  }

}
