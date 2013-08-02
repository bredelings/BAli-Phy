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

#include <cassert>
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
  assert(not standard);
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

double myrandomf() {
  return uniform();
}

/// returns a value in [0,max-1]
unsigned long myrandom(unsigned long max) {
  return std::uniform_int_distribution<unsigned long>(0, max-1)(standard);
} 

long myrandom(long min,long max) {
  unsigned long diff = max - min;
  return myrandom(diff)+min;
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

double gamma(double a, double b) {
  return std::gamma_distribution<>(a,b)(standard);
}

unsigned poisson(double mu) {
  return std::poisson_distribution<>(mu)(standard);
}

unsigned geometric(double p) {
  return std::geometric_distribution<>(p)(standard);
}

valarray<double> dirichlet(const valarray<double>& n) 
{
  valarray<double> x(n.size());
  for(int i=0;i<n.size();i++)
    x[i] = gamma(n[i],1.0);
  x /= x.sum();
  return x;
}

