#ifndef RATES_H
#define RATES_H

#include <vector>
#include <cassert>
#include <cmath>
#include "model.H"

namespace substitution {

  /// A generic distribution for rate variation
  /// We require that if X ~ d, the E(X) = 1
  class RateDistribution: virtual public Model {
  protected:
    /// Construct a RateDistribution with np parameters
    RateDistribution(int np): Model(np) {};

  public:
    /// Make a copy of this object
    virtual RateDistribution* clone() const =0;

    /// The cumulative density function at x
    virtual double cdf(double x) const =0;

    /// The probability density function at x, estimated on scale dx
    virtual double pdf(double x,double dx=0.001) const;

    /// Compute the p-th quantile, with tolerance tol
    virtual double quantile(double p,double tol=1.0e-6) const;

    /// Construct a RateDistribution with np parameters
    RateDistribution() {};

    virtual ~RateDistribution();
  };
  

  /// A Gamma distribution for rate variation
  class Gamma: public RateDistribution {
    void recalc() {}
  public:
    Gamma* clone() const {return new Gamma(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    double quantile(double,double tol=1.0e-6) const;

    double prior() const;

    void fiddle(); 

    std::string name() const;
    std::string parameter_name(int) const;

    Gamma():RateDistribution(1) 
    {
      parameters_[0] = 0.1;
    }
  };


  class LogNormal: public RateDistribution {
    void recalc() {}
  public:
    LogNormal* clone() const {return new LogNormal(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    double quantile(double,double tol=1.0e-6) const;

    double prior() const;

    void fiddle(); 

    std::string name() const;
    std::string parameter_name(int) const;

    LogNormal():RateDistribution(1)
    {
      parameters_[0] = 0.1;
    }
  };


  /// A Uniform distribution for rate variation
  class Uniform: public RateDistribution {
    void recalc() {}
  public:
    Uniform* clone() const {return new Uniform(*this);}

    double cdf(double x) const {
      if (x < 0) return 0;
      if (x > 2) return 1;
      else return x/2;
    }
    double pdf(double x,double) const {if (x < 0 or x > 2) return 0; else return 0.5;};

    double prior() const { return log(0.5);}

    void fiddle() {}

    double quantile(double p,double) const {return 2*p;}

    std::string name() const;
    std::string parameter_name(int) const;

    Uniform():RateDistribution(0) 
    {}
  };

  /// A sum of distributions for rate variation
  class MultipleDistribution: public SuperDerivedModelOver<RateDistribution,RateDistribution> 
  {
    void super_fiddle(); 
  public:

    double super_prior() const;

    MultipleDistribution* clone() const {return new MultipleDistribution(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    
    /// Number of sub-distributions
    int n_dists() const {return n_submodels();}

    /// Fraction of sub-distribution i
    double fraction(int i) const {
      assert(0 <= i and i < distributions.size());
      return parameters_[i];
    }

    /// Fraction of sub-distribution i
    double& fraction(int i) {
      assert(0 <= i and i < distributions.size());
      return parameters_[i];
    }

    std::string name() const;
    std::string super_parameter_name(int) const;

    MultipleDistribution(const std::vector<OwnedPointer<RateDistribution> >&);

    ~MultipleDistribution() {}
  };


  inline substitution::MultipleDistribution operator+(const substitution::RateDistribution& D1,const substitution::RateDistribution& D2) {
    std::vector<OwnedPointer<substitution::RateDistribution> > temp;
    temp.push_back(D1);
    temp.push_back(D2);
    return substitution::MultipleDistribution(temp);
  }
}
#endif
