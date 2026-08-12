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
#include <cstdint>
#include "util/assert.hh"
#include <cmath>
#include <fstream>
#include <iostream>
#include <limits>
#include <boost/random/random_device.hpp>

#include "util/rng.H"

using std::valarray;
using std::int64_t;
using std::uint64_t;

namespace
{

// Alternative samplers are only needed near p=1; this cutoff keeps their
// expected work and variance small.
}

uint64_t get_random_seed()
{
    boost::random::random_device random;
    const int bits_per_read = sizeof(boost::random::random_device::result_type)*8;
  
    uint64_t s=0;
    if (random.entropy())
    {
	for(int i=0;i*bits_per_read < sizeof(s)*8;i++) 
	{
	    unsigned u = random();
	    s <<= bits_per_read;
	    s |=  u;
	}
    }
    else
	s = time(NULL);
  
    return s;
}

std::mt19937_64 standard_rng;

std::uint64_t myrand_init(std::uint64_t s) 
{
    standard_rng.seed(s);
    return s;
}

std::uint64_t myrand_init() 
{
    return myrand_init(get_random_seed());
}

double uniform() 
{
    return std::uniform_real_distribution<>(0.0, 1.0)(standard_rng);
}

int64_t uniform_int(int64_t min, int64_t max)
{
    assert(min <= max);
    return std::uniform_int_distribution<std::int64_t>(min, max)(standard_rng);
}

/// returns a value in [0,max-1]
uint64_t myrandom(uint64_t max) {
    return uniform_int(0, max-1);
} 

int64_t myrandom(int64_t min, int64_t max) {
    assert(min < max);
    return uniform_int(min,max-1);
}

double log_unif() {
    // This inverse-CDF sampler truncates the far tail at the precision of a
    // double uniform; a variable-number-of-bits algorithm could represent it.
    return -std::exponential_distribution<>(1.0)(standard_rng);
}

double gaussian(double mu,double sigma) 
{
    assert(sigma > 0);
    return std::normal_distribution<>(mu, sigma)(standard_rng);
}

double laplace(double mu,double sigma) 
{
    assert(sigma > 0);
    double x = exponential(sigma);
    auto y = standard_rng();
    if (y&1)
	x = -x;
    x += mu;
    return x;
}

double cauchy(double l,double s) {
    return std::cauchy_distribution<>(l,s)(standard_rng);
}

double exponential(double mu)
{
    assert(mu > 0);
    return std::exponential_distribution<>(1.0/mu)(standard_rng);
}

double beta(double a, double b)
{
    assert(a > 0);
    assert(b > 0);
    double x = gamma(a,1);
    double y = gamma(b,1);
    return x/(x+y);
}

double gamma(double a, double b)
{
    assert(a > 0);
    assert(b > 0);
    return std::gamma_distribution<>(a,b)(standard_rng);
}

unsigned poisson(double mu)
{
    assert(mu >= 0);
    if (mu == 0)
	return 0;
    else
    {
	assert(mu > 0.0);
	return std::poisson_distribution<>(mu)(standard_rng);
    }
}

unsigned geometric(double p) {
    assert(0 <= p and p <= 1);
    return std::geometric_distribution<>(p)(standard_rng);
}

// Sample from both log tails so a small failure probability is not lost when
// the success probability rounds to one.
int geometric_from_logs(double log_p, double log_q)
{
    // NaN is represented by a finite placeholder whose associated density remains NaN.
    if (std::isnan(log_p) or std::isnan(log_q)) return 0;
    if (log_q == -INFINITY) return 0;
    if (log_p == -INFINITY) return std::numeric_limits<int>::max();

    // For K failures before success, P(K >= k)=q^k. Sampling from log(U)/log(q) preserves a
    // tiny failure tail without reconstructing q, and saturates before converting to Int.
    double n = std::floor(log_unif() / log_q);
    if (not std::isfinite(n) or n >= std::numeric_limits<int>::max())
        return std::numeric_limits<int>::max();
    return int(n);
}

int negative_binomial(int r, double p)
{
    assert(r >= 0);
    assert(0 < p and p <= 1);

    if (r == 0 or p == 1) return 0;

    return std::negative_binomial_distribution<>(r,p)(standard_rng);
}

// Let theta=q/p, Lambda~Gamma(shape=r,scale=theta), and K|Lambda~Poisson(Lambda).
// Integrating Lambda gives Gamma(k+r)/(k!*Gamma(r))*theta^k/(1+theta)^(k+r).
// Since theta/(1+theta)=q and 1/(1+theta)=p, this is
// choose(k+r-1,k)*p^r*q^k, the required negative-binomial distribution.
int negative_binomial_from_logs(int r, double log_p, double log_q)
{
    assert(r >= 0);

    // NaN uses a finite placeholder whose density remains NaN; p=0 has all its mass beyond Int.
    if (std::isnan(log_p) or std::isnan(log_q)) return 0;
    if (r == 0 or log_q == -INFINITY) return 0;
    if (log_p == -INFINITY) return std::numeric_limits<int>::max();

    // A NegativeBinomial(r,p) variate is the sum of r independent Geometric(p) variates.
    // Accumulating in Int space allows saturation without entering a library rejection loop.
    int total = 0;
    for (int i = 0; i < r; i++)
    {
        int n = geometric_from_logs(log_p, log_q);
        if (n >= std::numeric_limits<int>::max() - total)
            return std::numeric_limits<int>::max();
        total += n;
    }
    return total;
}

int binomial(int n, double p) {
    assert(n >= 0);
    assert(0 <= p and p <= 1);

    // Maybe the library already does these optimizations?
    if (n == 0 or p == 0) return 0;
    if (p == 1) return n;

    return std::binomial_distribution<>(n,p)(standard_rng);
}

// Sample the less likely outcome and reflect failures back to successes when
// p>1/2, avoiding construction of a probability near one.
int binomial_from_logs(int n, double log_p, double log_q)
{
    if (log_p <= log_q)
        return binomial(n, exp(log_p));
    else
        return n - binomial(n, exp(log_q));
}

int beta_binomial(int n, double a, double b)
{
    double p = beta(a,b);
    return binomial(n,p);
}

unsigned bernoulli(double p)
{
    assert(0 <= p and p <= 1);

    double u = uniform();

    if (u<p)
	return 1;
    else
	return 0;
}

// Sample the less likely outcome so neither complementary probability is
// reconstructed by subtraction.
unsigned bernoulli_from_logs(double log_p, double log_q)
{
    if (log_p <= log_q)
        return bernoulli(exp(log_p));
    else
        return 1 - bernoulli(exp(log_q));
}

valarray<double> dirichlet(const valarray<double>& n) 
{
    valarray<double> x(n.size());
    for(int i=0;i<n.size();i++)
	x[i] = gamma(n[i],1.0);
    x /= x.sum();
    return x;
}
