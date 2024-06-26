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

#ifndef RNG_H
#define RNG_H

#include <valarray>
#include <random>
#include <cstdint>

std::uint64_t myrand_init();
std::uint64_t myrand_init(std::uint64_t);
 
// returns a value in [0,1)
double uniform();

// return the log of a variable that is uniform on (0,1)
double log_unif();

// return a normal random variate
double gaussian(double mu,double sigma);

// return a shifted laplacian random variate
double laplace(double mu,double sigma);

// return a shifted laplacian random variate
double cauchy(double l=0.0,double s=1.0);

// return an exponentially distributed variable
double exponential(double mu);

// return a beta-distributed variable
double beta(double a, double b);

// return an gamma-distributed variable
double gamma(double a, double b);

// return a poisson random variate
unsigned poisson(double mu);

// return a geometric random variate
unsigned geometric(double p);

// return a binomial random variate
int negative_binomial(int r, double p);

// return a binomial random variate
int binomial(int n, double p);

// return a binomial random variate
int beta_binomial(int n, double a, double b);

// return a binomial random variate
unsigned bernoulli(double p);

// return a dirichlet random vector
std::valarray<double> dirichlet(const std::valarray<double>& n);

std::uint64_t get_random_seed();

std::uint64_t myrandom(std::uint64_t max);
std::int64_t myrandom(std::int64_t min, std::int64_t max);

std::int64_t uniform_int(std::int64_t min, std::int64_t max);

template <typename T>
const T& uniform_element(const std::vector<T>& v)
{
    assert(not v.empty());
    int index = uniform_int(0, v.size()-1);
    return v[index];
}
#endif
