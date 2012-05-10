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

#include "probability.H"
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf.h>
#include <iostream>

#include "logsum.H"
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
  log_double_t Pr = 1;
  double sigma2 = sigma * sigma;
  x -= mu;

  Pr.log() = -0.5*log(2.0*M_PI*sigma2) -(x*x)/(2.0*sigma2);

  return Pr;
}

log_double_t log_normal_pdf(double x, double mu, double sigma)
{
  log_double_t Pr = 1;
  Pr.log() = normal_pdf(log(x),mu,sigma)-log(x);
  return Pr;
}
