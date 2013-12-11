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

#include <ctime>
#include <cmath>
#include <fstream>
#include <iostream>
#include <boost/random/random_device.hpp>

#include "rng.H"

using std::valarray;

unsigned long get_random_seed()
{
  unsigned long s=0;
  const int bits_per_read = sizeof(unsigned)*8;
  
  boost::random::random_device random;
  
  if (random.entropy())
    for(int i=0;i*bits_per_read < sizeof(s)*8;i++) 
    {
      unsigned u = random();
      s <<= bits_per_read;
      s |=  u;
    }
  else
    s = time(NULL);
  
  return s;
}

std::mt19937_64 standard;

unsigned long myrand_init(unsigned long s) 
{
  standard.seed(s);
  return s;
}

unsigned long myrand_init() 
{
  return myrand_init(get_random_seed());
}

double uniform() 
{
  return std::uniform_real_distribution<>(0.0, 1.0)(standard);
}

int uniform(int min, int max)
{
  return std::uniform_int_distribution<unsigned long>(min, max)(standard);
}

/// returns a value in [0,max-1]
unsigned long myrandom(unsigned long max) {
  return uniform(0, max-1);
} 

long myrandom(long min,long max) {
  return uniform(min,max-1);
}

double log_unif() {
  return -std::exponential_distribution<>(1.0)(standard);
}

double gaussian(double mu,double sigma) 
{
  return std::normal_distribution<>(mu, sigma)(standard);
}

double laplace(double mu,double sigma) 
{
  double x = exponential(sigma);
  auto y = standard();
  if (y&1)
    x = -x;
  x += mu;
  return x;
}

double cauchy(double l,double s) {
  return std::cauchy_distribution<>(l,s)(standard);
}

double exponential(double mu) {
  return std::exponential_distribution<>(1.0/mu)(standard);
}

double beta(double a, double b) {
  double x = gamma(a,1);
  double y = gamma(b,1);
  return x/(x+y);
}

double gamma(double a, double b) {
  return std::gamma_distribution<>(a,b)(standard);
}

unsigned poisson(double mu) {
  return std::poisson_distribution<>(mu)(standard);
}

unsigned geometric(double p) {
  return std::geometric_distribution<>(p)(standard);
}

unsigned binomial(int n, double p) {
  return std::binomial_distribution<>(n,p)(standard);
}

unsigned bernoulli(double p) {
  double u = uniform();

  if (u<p)
    return 1;
  else
    return 0;
}

valarray<double> dirichlet(const valarray<double>& n) 
{
  valarray<double> x(n.size());
  for(int i=0;i<n.size();i++)
    x[i] = gamma(n[i],1.0);
  x /= x.sum();
  return x;
}

