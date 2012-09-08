/*
   Copyright (C) 2004-2007 Benjamin Redelings

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

  //--------------- UniformRateDistribution -----------------//

  void Uniform::recalc(const vector<int>&)
  {
    vector<Double> p(2);
    p[0] = 0;
    p[1] = 2;
    D->set_parameter_values(p);
  }

  Uniform::Uniform() {}

  //--------------- GammaRateDistribution -----------------//
  
  efloat_t Gamma::prior() const {
    double g_sigma = get_parameter_value_as<Double>(0);
    double log_g_sigma = log(g_sigma);
    return laplace_pdf(log_g_sigma,-3,1.0);
  }

  void Gamma::recalc(const vector<int>&)
  {
    double s = minmax(double(get_parameter_value_as<Double>(0)), 1.0e-5, 3.0);

    vector<Double> p(2);
    p[0] = 1.0/(s*s);
    p[1] = 1.0/p[0];

    D->set_parameter_values(p);
  }

  Gamma::Gamma() {
    add_parameter(Parameter("Gamma.sigmaOverMu", Double(0.1), lower_bound(0)));
  }

  //--------------- Beta RateDistribution -----------------//

  // mu    = E(X)
  // gamma = Var(X)/[ mu * (1-mu)] = 1/(1 + a + b)  \in (0,1]
  //
  // N = a + b = 1/gamma - 1

  const double alpha_beta_min = 0.5;

  void Beta::recalc(const vector<int>& /* indices */)
  {
    double mu = get_parameter_value_as<Double>(0);      // E(x)
    double gamma = get_parameter_value_as<Double>(1);   // Var(x)/E(x)

    double N = 1.0/gamma - 1.0;

    double a = N * mu;
    double b = N * (1.0 - mu);

    if (mu < 0 or mu > 1) std::abort();
    if (gamma < 0 or gamma > 1) std::abort();
    
    vector<Double> p(2);
    p[0] = std::max(a, alpha_beta_min);
    p[1] = std::max(b, alpha_beta_min);

    D->set_parameter_values(p);
  }

  efloat_t Beta::prior() const 
  {
    double mu = get_parameter_value_as<Double>(0);      // E(x)
    double gamma = get_parameter_value_as<Double>(1);   // Var(x)/E(x)

    double N = 1.0/gamma - 1.0;

    double a = N * mu;
    double b = N * (1.0 - mu);

    if (a <= alpha_beta_min or b <= alpha_beta_min) 
      return 0.0;

    if (D->alpha()<1.0 or D->beta()<1.0)
      return 0.0;

    efloat_t Pr = 1;
    Pr *= beta_pdf(mu, 10, 1);   
    Pr *= exponential_pdf(gamma, 0.1);

    return Pr;
  }

  Beta::Beta()
  {
    add_parameter(Parameter("Beta.mu", Double(0.5), between(0, 1)));
    add_parameter(Parameter("Beta.varOverMu", Double(0.1), between(0,1)));
  }

  //-------------- LogNormal Distribution ----------------//
  // E X = exp(lmu + 0.5*lsigma^2)
  // Var X = (exp(lsigma^2)-1) - exp(2*lmu + lsigma^2)

  // EX==1 => lmu = -0.5 * lsigma^2
  //          Var X = (exp(lsigma^2)-1  => log(Var X + 1) = lsigma^2

  efloat_t LogNormal::prior() const {
    double g_sigma = get_parameter_value_as<Double>(0);
    double log_g_sigma = log(g_sigma);
    return laplace_pdf(log_g_sigma,-3,1.0);
  }

  void LogNormal::recalc(const vector<int>&)
  {
    double s = minmax(double(get_parameter_value_as<Double>(0)), 1.0e-5, 1.0e5);

    double Var = s*s;
    double lVar = log1p(Var);

    vector<Double> p(2);
    //    double lm = 
    p[0] = -0.5 * lVar;
    double ls = p[1] = sqrt(lVar);

    // don't go crazy
    p[1] = std::max(ls,1.0e-5);

    D->set_parameter_values(p);
  }

  LogNormal::LogNormal() 
  {
    add_parameter(Parameter("LogNormal.sigmaOverMu", Double(0.1), lower_bound(0)));
  }
    

  /// Choose boundaries between bins based on quantiles
  vector<double> uniform_boundaries(const vector<double>& r, const Distribution& D)
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
  vector<double> get_fractions(const vector<double>& b,const probability::Distribution& D)
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
  
  Discretization::Discretization(int N)
    :r(N),f(N),A(1)
  { }
    
  
  Discretization::Discretization(int N,const Distribution& D,double a)
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
    
  UniformDiscretization::UniformDiscretization(int N, const Distribution& D)
      :Discretization(N,D,1)
  {
    b = uniform_boundaries(r,D);
    f = get_fractions(b,D);
  }
  
  
  double Discretization::error(double (*g)(double x),const Distribution& D) const
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
  
  double Discretization::error2(double (*g)(double x),const Distribution& D) const
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
