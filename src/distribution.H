/*
   Copyright (C) 2007-2012 Benjamin Redelings

This file is part of BAli-Phy.

BAli-Phy is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

BAli-Phy is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with BAli-Phy; see the file COPYING.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef DISTRIBUTION_H
#define DISTRIBUTION_H

#include "model.H"
#include "log-double.H"

namespace probability {

  class Distribution: public ::Model
  {
  public:
    /// Make a copy of this object
    virtual Distribution* clone() const =0;

    /// The probability density function at x, estimated on scale dx
    virtual log_double_t pdf(double x) const =0;

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
    owned_ptr<T> D;
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
      :D(claim(new T))
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

    double start() const {return get_parameter_value_as<Double>(0);}
    double end() const {return get_parameter_value_as<Double>(1);}
 
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

    double mu() const {return get_parameter_value_as<Double>(0);}
 
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

    double alpha() const {return get_parameter_value_as<Double>(0);}
    double beta() const {return get_parameter_value_as<Double>(1);}
 
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

    double alpha() const {return get_parameter_value_as<Double>(0);}
    double beta() const {return get_parameter_value_as<Double>(1);}
 
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

    double lmu() const {return get_parameter_value_as<Double>(0);}
    double lsigma() const {return get_parameter_value_as<Double>(1);}
 
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

    double mu() const {return get_parameter_value_as<Double>(0);}
    double sigma() const {return get_parameter_value_as<Double>(1);}
 
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

    double m() const {return get_parameter_value_as<Double>(0);}
    double s() const {return get_parameter_value_as<Double>(1);}
 
    Cauchy();
    Cauchy(double m_=0.0,double s_=1.0);

    ~Cauchy();
  };
}

#endif
