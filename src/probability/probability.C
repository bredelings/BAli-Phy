/*
   Copyright (C) 2004-2006,2009 Benjamin Redelings

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

#include "probability/probability.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_cdf.h>
#include <iostream>

#include "math/logsum.H"
#include "util.H"

using std::valarray;
using std::vector;

log_double_t double_factorial(int n) 
{
  log_double_t x = 1;
  for(int i=3;i<=n;i+=2)
    x *= i;
  return x;
}

log_double_t num_branches(int n) {
  return 2*n-3;
}

log_double_t num_topologies(int n) {
  return double_factorial(2*n-5);
}

log_double_t num_topologies_in_partition(int n1,int n2) 
{
  log_double_t total = num_topologies(n1) * num_topologies(n2);
  total *= num_branches(n1) * num_branches(n2);
  return total;
}

double log_gamma(double x) {
  assert(x>0.0);
  return gsl_sf_lngamma(x);
}

log_double_t dirichlet_pdf(const valarray<double>& p,const valarray<double>& n) 
{
  assert(p.size() == n.size());

  log_double_t Pr = 1;
  for(int i=0;i<p.size();i++) 
    Pr *= pow(log_double_t(p[i]),n[i]-1.0);

  // This term is constant in p
  Pr.log() += log_gamma(n.sum());
  for(int i=0;i<p.size();i++)
    Pr.log() -= log_gamma(n[i]);

  return Pr;
}

log_double_t dirichlet_pdf(const vector<double>& p,const vector<double>& n) 
{
  assert(p.size() == n.size());

  log_double_t Pr = 1;
  for(int i=0;i<p.size();i++) 
    Pr *= pow(log_double_t(p[i]),n[i]-1.0);

  // This term is constant in p
  Pr.log() += log_gamma(sum(n));
  for(int i=0;i<p.size();i++)
    Pr.log() -= log_gamma(n[i]);

  return Pr;
}

log_double_t dirichlet_pdf(const valarray<double>& p,double N,const valarray<double>& q)
{
  return dirichlet_pdf(p,N*p.size()*q);
}

log_double_t dirichlet_pdf(const valarray<double>& p,double N) {
  return dirichlet_pdf(p,valarray<double>(N,p.size()));
}

valarray<double> safe_count(valarray<double> n)
{
  for(int i=0;i<n.size();i++)
    if (n[i] < 1.0) n[i] = 1.0;
  return n;
}

log_double_t dirichlet_safe_pdf(const valarray<double>& p,const valarray<double>& n) 
{
  return dirichlet_pdf(p,safe_count(n));
}

log_double_t dirichlet_safe_pdf(const valarray<double>& p,double N, const valarray<double>& q) 
{
  return dirichlet_safe_pdf(p,N*p.size()*q);
}

log_double_t uniform_pdf(double x, double a, double b)
{
  assert(b > a);

  if (x < a or x > b) return 0;

  return 1.0/log_double_t(b-a);
}

/// log density for y if y=ln (x+delta), and x ~ Exp(mu)

/// f(x) = exp(-x/mu)/mu   g(y) = exp(-(exp(y)-delta)/mu)/mu * exp(y)
log_double_t exp_exponential_pdf(double y, double mu, double delta) {
  double x = exp(y)-delta;
  assert(x >= 0);

  return exp<log_double_t>(-x/mu + y)/mu;
}

log_double_t exponential_pdf(double x, double mu) {
  if (x < 0) 
    return 0;

  return exp<log_double_t>(-x/mu)/mu;
}

log_double_t laplace_pdf(double x, double mu, double sigma) {
  double a = sigma/sqrt(2);
  return gsl_ran_laplace_pdf(x-mu,a);
}

log_double_t cauchy_pdf(double x, double l, double s)
{
  double C = (x-l)/s;
  C = M_PI*s*(1.0+C*C);
  log_double_t C2 = C;
  return pow(C2,-1.0);
}

log_double_t beta_pdf(double p,double a, double b) 
{
    if (p <= 0.0 or p >= 1.0)
      return 0;
    else
      return gsl_ran_beta_pdf(p,a,b);
}

double beta_quantile(double p, double a, double b)
{
  if (a<0 or b<0)
    a=b=1;

  // Avoid values GSL can't handle: could we approximate with e.g. normal here?
  double r = 100.0/std::max(a,b);
  if (r < 1) {
    a *= r;
    b *= r;
  }

  // Avoid values GSL can't handle.
  a = std::max(a,0.1);
  b = std::max(b,0.1);

  double x = gsl_cdf_beta_Pinv(p,a,b);
  std::cerr<<"x = "<<x<<" p = "<<p<<" a = "<<a<<" b = "<<b<<std::endl;
  return x;
}

log_double_t gamma_pdf(double x,double a, double b) 
{
  if (x < 0) 
    return 0;

  log_double_t Pr = 1;

  Pr.log() = (a-1)*log(x) - x/b - log_gamma(a) - a*log(b);

  return Pr;
}

log_double_t normal_pdf(double x, double mu, double sigma)
{
  assert(sigma >= 0);

  log_double_t Pr = 1;
  double sigma2 = sigma * sigma;
  x -= mu;

  Pr.log() = -0.5*log(2.0*M_PI*sigma2) -(x*x)/(2.0*sigma2);

  return Pr;
}

double normal_quantile(double p, double mu, double sigma)
{
  assert(p >= 0);
  assert(p <= 1);
  assert(sigma >= 0);
  return mu+gsl_cdf_gaussian_Pinv(p,sigma);
}

log_double_t log_normal_pdf(double x, double mu, double sigma)
{
  assert(sigma >= 0);

  log_double_t Pr = 1;
  Pr.log() = normal_pdf(log(x),mu,sigma)-log(x);
  return Pr;
}

double log_normal_quantile(double p, double lmu, double lsigma)
{
  assert(p >= 0);
  assert(p <= 1);
  assert(lsigma >= 0);

  // don't go crazy
  lsigma = minmax(lsigma, 1.0e-5, 1.0e5);

  return gsl_cdf_lognormal_Pinv(p, lmu, lsigma);
}

static double pointChi2(double prob, double v)
{
  // Returns z so that Prob{x<z}=prob where x is Chi2 distributed with df
  // = v
  // RATNEST FORTRAN by
  // Best DJ & Roberts DE (1975) The percentage points of the
  // Chi2 distribution. Applied Statistics 24: 385-388. (AS91)

  double e = 0.5e-6, aa = 0.6931471805, p = prob, g;
  double xx, c, ch, a = 0, q = 0, p1 = 0, p2 = 0, t = 0, x = 0, b = 0, s1, s2, s3, s4, s5, s6;
    
  if (v <= 0)
    throw myexception()<<"Arguments out of range: alpha = "<<a;

  assert(p >= 0 and p <= 1);

  if (p < 0.000002) 
  {
    std::cerr<<"Warning: can't handle p = "<<p<<" in gamma quantile: using 0.000002";

    p = 0.000002;
  }

  if (p > 0.999998) 
  {
    std::cerr<<"Warning: can't handle p = "<<p<<" in gamma quantile: using 0.999998";

    p = 0.999998;
  }

  g = gsl_sf_lngamma(v / 2);
    
  xx = v / 2;
  c = xx - 1;
  if (v < -1.24 * log(p)) {
    ch = pow((p * xx * exp(g + xx * aa)), 1 / xx);
    if (ch - e < 0) {
      return ch;
    }
  } else {
    if (v > 0.32) {
      x = gsl_cdf_gaussian_Pinv(p,1);
      p1 = 0.222222 / v;
      ch = v * pow((x * sqrt(p1) + 1 - p1), 3.0);
      if (ch > 2.2 * v + 6) {
	ch = -2 * (log(1 - p) - c * log(.5 * ch) + g);
      }
    } else {
      ch = 0.4;
      a = log(1 - p);

      do {
	q = ch;
	p1 = 1 + ch * (4.67 + ch);
	p2 = ch * (6.73 + ch * (6.66 + ch));
	t = -0.5 + (4.67 + 2 * ch) / p1
	  - (6.73 + ch * (13.32 + 3 * ch)) / p2;
	ch -= (1 - exp(a + g + .5 * ch + c * aa) * p2 / p1)
	  / t;
      } while (std::abs(q / ch - 1) - .01 > 0);
    }
  }
  do {
    q = ch;
    p1 = 0.5 * ch;
      
    if ((t = gsl_sf_gamma_inc_P(xx, p1)) < 0)
      throw myexception()<<"Arguments out of range: t < 0";

    p2 = p - t;
    t = p2 * exp(xx * aa + g + p1 - c * log(ch));
    b = t / ch;
    a = 0.5 * t - b * c;

    s1 = (210 + a * (140 + a * (105 + a * (84 + a * (70 + 60 * a))))) / 420;
    s2 = (420 + a * (735 + a * (966 + a * (1141 + 1278 * a)))) / 2520;
    s3 = (210 + a * (462 + a * (707 + 932 * a))) / 2520;
    s4 = (252 + a * (672 + 1182 * a) + c * (294 + a * (889 + 1740 * a))) / 5040;
    s5 = (84 + 264 * a + c * (175 + 606 * a)) / 2520;
    s6 = (120 + c * (346 + 127 * c)) / 5040;
    ch += t
      * (1 + 0.5 * t * s1 - b
	 * c
	 * (s1 - b
	    * (s2 - b
	       * (s3 - b
		  * (s4 - b * (s5 - b * s6))))));
  } while (std::abs(q / ch - 1) > e);
    
  assert(not std::isnan(ch) and std::isfinite(ch));
  assert(ch >= 0);
  return (ch);
}

double gamma_quantile_no_approx(double p, double a, double b)
{
  assert(a >= 0);
  assert(b >= 0);

  assert(p >= 0);
  assert(p <= 1);

  return 0.5 * b * pointChi2(p, 2.0* a);
}

double gamma_quantile(double p, double a, double b)
{
  assert(a >= 0);
  assert(b >= 0);
  assert(p >= 0);
  assert(p <= 1);

  if (a < 10000)
    return gamma_quantile_no_approx(p,a,b);
  else {
    double M = a*b;
    double V = a*b*b;

    // Isn't this the same as log1p(1.0/a)?
    double sigma2 =  log1p(V/(M*M));
    double mu = log(M) - sigma2/2.0;
    double sigma = sqrt(sigma2);

    // don't go crazy
    sigma = minmax(sigma, 1.0e-5, 1.0e5);

    return gsl_cdf_lognormal_Pinv(p,mu,sigma);
  }
}
