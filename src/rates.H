#ifndef RATES_H
#define RATES_H

#include <vector>
#include <cassert>
#include <cmath>
#include "model.H"

namespace substitution {

  /// A generic distribution for rate variation
  /// We require that if X ~ d, the E(X) = 1
  class RateDistribution: virtual public ::Model 
  {
  public:
    /// Make a copy of this object
    virtual RateDistribution* clone() const =0;

    /// The cumulative density function at x
    virtual double cdf(double x) const =0;

    /// The probability density function at x, estimated on scale dx
    virtual double pdf(double x) const =0;

    /// Compute the p-th quantile, with tolerance tol
    virtual double quantile(double p,double tol=1.0e-6) const;

    virtual ~RateDistribution();
  };
  

  /// A Gamma distribution for rate variation
  class Gamma: public RateDistribution 
  {
  public:
    Gamma* clone() const {return new Gamma(*this);}

    double cdf(double) const;
    double pdf(double) const;
    double quantile(double,double tol=1.0e-6) const;

    efloat_t prior() const;

    std::string name() const;

    Gamma();
  };


  /// A Beta distribution for rate variation
  class Beta: public RateDistribution 
  {
  public:
    Beta* clone() const {return new Beta(*this);}

    double cdf(double) const;
    double pdf(double) const;
    double quantile(double,double tol=1.0e-6) const;

    efloat_t prior() const;

    std::string name() const;

    Beta();
  };


  class LogNormal: public RateDistribution 
  {
  public:
    LogNormal* clone() const {return new LogNormal(*this);}

    double cdf(double) const;
    double pdf(double) const;
    double quantile(double,double tol=1.0e-6) const;

    efloat_t prior() const;

    std::string name() const;

    LogNormal();
  };


  /// A Uniform distribution for rate variation
  class Uniform: public RateDistribution 
  {
  public:
    Uniform* clone() const {return new Uniform(*this);}

    double cdf(double x) const {
      if (x < 0) return 0;
      if (x > 2) return 1;
      else return x/2;
    }
    double pdf(double x) const {if (x < 0 or x > 2) return 0; else return 0.5;};

    efloat_t prior() const { return 0.5;}

    double quantile(double p,double) const {return 2*p;}

    std::string name() const;

    ~Uniform();
  };

  /// A sum of distributions for rate variation
  class MultipleDistribution: public RateDistribution, public SuperModelOver<RateDistribution> 
  {
  public:

    efloat_t super_prior() const;

    MultipleDistribution* clone() const {return new MultipleDistribution(*this);}

    double cdf(double) const;
    double pdf(double) const;
    
    /// Number of sub-distributions
    int n_dists() const {return n_submodels();}

    /// Fraction of sub-distribution i
    double fraction(int i) const {
      assert(0 <= i and i < n_dists());
      return parameter(i);
    }

    /// Fraction of sub-distribution i
    double& fraction(int i) {
      assert(0 <= i and i < n_dists());
      return parameters_[i];
    }

    std::string name() const;

    MultipleDistribution(const std::vector<OwnedPointer<RateDistribution> >&);

    ~MultipleDistribution() {}
  };


  inline substitution::MultipleDistribution operator+(const substitution::RateDistribution& D1,const substitution::RateDistribution& D2) {
    std::vector<OwnedPointer<substitution::RateDistribution> > temp;
    temp.push_back(D1);
    temp.push_back(D2);
    return substitution::MultipleDistribution(temp);
  }
  
  /// A Discretization of a Distribution
  struct Discretization
  {
    vector<double> p;
    vector<double> r;
    
    vector<double> b;
    vector<double> f;
    
    double A;
    
    double operator()(double x) const;
    
    int size() const {return f.size();}
    
    double error(double (*f)(double x),const RateDistribution& D) const;
    double error2(double (*f)(double x),const RateDistribution& D) const;
    
    double moment(double p) const;
    
    double scale() const; 
    void scale(double r);
    
    Discretization(int N):r(N),f(N) { }
    
    Discretization(int N,const RateDistribution& D,double a=2);
  };

  /// A Discretization with equal bin probabilities
  struct UniformDiscretization: public Discretization
  {
    UniformDiscretization(int N);
    
    UniformDiscretization(int N, const RateDistribution& D);
  };

}
#endif
