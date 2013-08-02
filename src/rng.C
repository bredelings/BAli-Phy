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

/************* Interfaces to rng::standard *********************/
namespace rng {
  RNG* standard;

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
}

unsigned long myrand_init() {
  assert(not rng::standard);
  rng::init();
  unsigned long s = rng::standard->seed();
  
  assert(rng::standard);
  return s;
}

unsigned long myrand_init(unsigned long s) {
  assert(not rng::standard);
  rng::init();
  s = rng::standard->seed(s);
  
  assert(rng::standard);
  return s;
}

unsigned long uniform_unsigned_long() {
  return rng::standard->get();
}

double uniform() {
  return rng::standard->uniform();
}

double myrandomf() {
  return uniform();
}

double log_unif() {
  return rng::standard->log_unif();
}

double gaussian(double mu,double sigma) {
  return rng::standard->gaussian(mu,sigma);
}

double laplace(double mu,double sigma) {
  return rng::standard->laplace(mu,sigma);
}

double cauchy(double l,double s) {
  return rng::standard->cauchy(l,s);
}

double exponential(double mu) {
  return rng::standard->exponential(mu);
}

double gamma(double a, double b) {
  return rng::standard->gamma(a,b);
}

unsigned poisson(double mu) {
  return rng::standard->poisson(mu);
}

unsigned geometric(double mu) {
  return rng::standard->geometric(mu);
}

valarray<double> dirichlet(const valarray<double>& n) {
  return rng::standard->dirichlet(n);
}

/*************** Functions for rng,dng and RNG **************/
void dng::init() { }

using namespace rng;

void rng::init() {
  // set up default generator and default seed from environment
  gsl_rng_env_setup();
  standard = new RNG;
}



unsigned long RNG::seed() {
  return seed(get_random_seed());
}

unsigned long RNG::seed(unsigned long int s) {
  assert(generator != NULL);
  gsl_rng_set(generator,s);
  return s;
}

RNG::RNG() {
  generator = gsl_rng_alloc(gsl_rng_default);

}

RNG::~RNG() {
  gsl_rng_free(generator);
}



unsigned Binomial::operator()(double p,unsigned long n1) {
  //  cerr<<"mean = "<<p*n<<endl;
  unsigned n = (unsigned)n1;
  assert((unsigned long)(n) == n1);
  
  assert(p>=0.0);
  assert(p<=1.0+1e-10);

  return gsl_ran_binomial(generator,p,n);
}

double Exponential::operator()() {
  return gsl_ran_exponential(generator,mu);
}


unsigned Poisson::operator()(double lambda) {
  //  cerr<<"lambda = "<<lambda<<endl;
  assert(lambda>=0.0);
  
  return gsl_ran_poisson(generator,lambda);
}


// this division and stuff is killing us
tuple Multinomial::operator()(const valarray<double>& p,unsigned long n1) {
  unsigned n = (unsigned)n1;
  assert((unsigned long)(n) == n1);

  assert(std::abs(double(p.sum())-1.0) < 1e-10);

  tuple m(0,p.size());
  double remaining = 1.0;
  for(int i=1;n and i<p.size();i++) {
    m[i] = Bin(p[i]/remaining,n);
    n -= m[i];
    remaining -= p[i];
  }
  m[0] = n;
  return m;
}

valarray<double> RNG::dirichlet(const valarray<double>& n)
{
  valarray<double> D(n.size());

  for(int i=0;i<D.size();i++)
    D[i] = gamma(n[i],1);

  D /= D.sum();

  return D;
}

