#ifndef RATES_H
#define RATES_H

#include <vector>
#include <valarray>
#include <cassert>
#include <cmath>
#include "clone.H"

namespace substitution {

  /// A generic distribution for rate variation
  /// We require that if X ~ d, the E(X) = 1
  class RateDistribution:public  Cloneable {
  protected:
    std::vector<double> parameters_;

  public:
    /// Make a copy of this object
    virtual RateDistribution* clone() const =0;

    /// The cumulative density function at x
    virtual double cdf(double x) const =0;

    /// The probability density function at x, estimated on scale dx
    virtual double pdf(double x,double dx=0.001) const;

    /// The prior on the parameters_, in log space
    virtual double prior() const =0;

    /// Symmetrically propose another vector of parameters
    virtual void fiddle(const std::valarray<bool>&) = 0; 

    /// Get the distribution parameters_
    virtual const std::vector<double>& parameters() const {return parameters_;}

    /// Set the distribution parameters_
    virtual void parameters(const std::vector<double>& p) {
      assert(p.size() == parameters_.size());
      parameters_ = p;
    }

    /// Compute the p-th quantile, with tolerance tol
    virtual double quantile(double p,double tol=1.0e-6) const;

    /// Construct a RateDistribution with np parameters
    RateDistribution(int np): parameters_(np) {};


    virtual ~RateDistribution();
  };
  

  /// A Gamma distribution for rate variation
  class Gamma: public RateDistribution {
  public:
    Gamma* clone() const {return new Gamma(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    double quantile(double,double tol=1.0e-6) const;

    double prior() const;

    void fiddle(const std::valarray<bool>&); 

    Gamma()
      :RateDistribution(1) 
    {
      parameters_[0] = 0.1;
    }
  };


  class LogNormal: public RateDistribution {
  public:
    LogNormal* clone() const {return new LogNormal(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    double quantile(double,double tol=1.0e-6) const;

    double prior() const;

    void fiddle(const std::valarray<bool>&); 

    LogNormal()
      :RateDistribution(1)
    {
      parameters_[0] = 0.1;
    }
  };


  /// A Uniform distribution for rate variation
  class Uniform: public RateDistribution {
  public:
    Uniform* clone() const {return new Uniform(*this);}

    double cdf(double x) const {
      if (x < 0) return 0;
      if (x > 2) return 1;
      else return x/2;
    }
    double pdf(double x,double) const {if (x < 0 or x > 2) return 0; else return 0.5;};

    double prior() const { return log(0.5);}

    void fiddle(const std::valarray<bool>&) {}

    double quantile(double p,double) const {return 2*p;}

    Uniform()
      :RateDistribution(0) 
    {}
  };

  /// A sum of distributions for rate variation
  class MultipleDistribution: public RateDistribution {
    std::vector<RateDistribution*> distributions;

    /// Load parameters from sub-distributions
    void read();

    /// Write parameters to sub-distributions
    void write();
  public:

    MultipleDistribution* clone() const {return new MultipleDistribution(*this);}

    double cdf(double) const;
    double pdf(double,double) const;
    
    double prior() const;

    /// Number of sub-distributions
    int ndists() const {return distributions.size();}

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

    /// Rate of sub-distribution i, where 1 is the average rate
    double rate(int i) const {
      assert(0 <= i and i < distributions.size());
      return parameters_[distributions.size()+i];
    };

    /// Rate of sub-distribution i, where 1 is the average rate
    double& rate(int i) {
      assert(0 <= i and i < distributions.size());
      return parameters_[distributions.size()+i];
    };

    void fiddle(const std::valarray<bool>&); 

    const std::vector<double>& parameters() const {return parameters_;}

    void parameters(const std::vector<double>& p);

    MultipleDistribution& operator=(const MultipleDistribution&);

    MultipleDistribution(const MultipleDistribution&);

    MultipleDistribution(const std::vector<const RateDistribution*>&);

    ~MultipleDistribution();
  };


  inline substitution::MultipleDistribution operator+(const substitution::RateDistribution& D1,const substitution::RateDistribution& D2) {
    std::vector<const substitution::RateDistribution*> temp;
    temp.push_back(&D1);
    temp.push_back(&D2);
    return substitution::MultipleDistribution(temp);
  }
}
#endif
