#ifndef DISTRIBUTION_H
#define DISTRIBUTION_H

#include "model.H"
#include "log-double.H"

namespace probability {

  class Distribution: virtual public ::Model
  {
  public:
    /// Make a copy of this object
    virtual Distribution* clone() const =0;

    /// The probability density function at x, estimated on scale dx
    virtual log_double_t pdf(double x) const =0;

    log_double_t operator()(double x) const {return pdf(x);}

    /// The cumulative density function at x
    virtual double cdf(double x) const =0;

    /// Compute the p-th quantile, with tolerance tol
    virtual double quantile(double p,double tol=1.0e-6) const;

    /// Compute the m-th moment
    virtual double moment(int m) const =0;

    /// Compute the mean
    virtual double mean() const;

    /// Compute the variance
    virtual double variance() const;

    virtual ~Distribution();
  };
  
  template <class T>
  class DistributionWrapper: public probability::Distribution
  {
  protected:
    OwnedPointer<T> D;
  public:
    DistributionWrapper<T>* clone() const {return new DistributionWrapper<T>(*this);}

    efloat_t pdf(double x) const {return D->pdf(x);}

    double cdf(double x) const {return D->cdf(x);}

    double quantile(double p, double tol=1.0e-6) const {return D->quantile(p,tol);}

    double moment(int m) const {return D->moment(m);}

    double mean() const {return D->mean();}
 
    double variance() const {return D->variance();}

    std::string name() const {return D->name();}

    DistributionWrapper()
      :D(new T)
    {}

    ~DistributionWrapper() {}
  };


  /// Uniform Distribution
  class Uniform: public Distribution
  {
  public:
    Uniform* clone() const {return new Uniform(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double start() const {return parameter(0);}
    double end() const {return parameter(1);}
 
    Uniform();
    Uniform(double,double);

    ~Uniform();
  };

  /// Exponential Distribution
  class Exponential: public Distribution
  {
  public:
    Exponential* clone() const {return new Exponential(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double mean() const;

    double variance() const;

    double mu() const {return parameter(0);}
 
    Exponential();

    Exponential(double mu);

    ~Exponential();
  };

  /// Gamma Distribution
  class Gamma: public Distribution
  {
  public:
    Gamma* clone() const {return new Gamma(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double mean() const;

    double variance() const;

    double alpha() const {return parameter(0);}
    double beta() const {return parameter(1);}
 
    Gamma();

    Gamma(double,double);

    ~Gamma();
  };

  /// Beta Distribution
  class Beta: public Distribution
  {
  public:
    Beta* clone() const {return new Beta(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double mean() const;

    double alpha() const {return parameter(0);}
    double beta() const {return parameter(1);}
 
    Beta();
    Beta(double,double);

    ~Beta();
  };

  /// LogNormal Distribution
  class LogNormal: public Distribution
  {
  public:
    LogNormal* clone() const {return new LogNormal(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double mean() const;

    double variance() const;

    double lmu() const {return parameter(0);}
    double lsigma() const {return parameter(1);}
 
    LogNormal();
    LogNormal(double,double);

    ~LogNormal();
  };

  /// Normal Distribution
  class Normal: public Distribution
  {
  public:
    Normal* clone() const {return new Normal(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double mean() const;

    double variance() const;

    double mu() const {return parameter(0);}
    double sigma() const {return parameter(1);}
 
    Normal();
    Normal(double,double);

    ~Normal();
  };

  /// Cauchy Distribution
  class Cauchy: public Distribution
  {
  public:
    Cauchy* clone() const {return new Cauchy(*this);}

    log_double_t pdf(double x) const;

    double cdf(double x) const;

    double quantile(double p,double tol = 1.0e-5) const;

    std::string name() const;

    double moment(int m) const;

    double m() const {return parameter(0);}
    double s() const {return parameter(1);}
 
    Cauchy();
    Cauchy(double m_=0.0,double s_=1.0);

    ~Cauchy();
  };


  double gamma_quantile_no_approx(double p, double a, double b);
  double gamma_quantile(double p, double a, double b);
}

#endif
