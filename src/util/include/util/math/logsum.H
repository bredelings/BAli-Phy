/*
  Copyright (C) 2004, 2010, 2014, 2016-2017 Benjamin Redelings

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

#ifndef LOGSUM_H
#define LOGSUM_H

#include <cmath>
#include <numbers>
#include "util/assert.hh"

// NATS is 52*log(2) for 52 bits of precision
// Although... the long doubles have 64 bits of precision...
constexpr double NATS = 40;

// log(1 + exp(x))
// domain: (-Inf, Inf)
inline double log1pexp(double x)
{
    if (x < 18.0)
	return log1p(exp(x));
    else if (x < 33.3)
	return x + exp(-x);
    else
	return x;
}

// log(1 - exp(x)) = log(-expm1(x)) = log1p(-exp(x))
// domain: (-Inf, 0]
inline double log1mexp(double x)
{
    assert(x <= 0);
    if (x > -std::numbers::ln2) // -log(2)
        return log(-expm1(x));
    else
        return log1p(-exp(x));
}

inline double logsum(double x, double y)
{
    if (x == -std::numeric_limits<double>::infinity()) return y;
    if (y == -std::numeric_limits<double>::infinity()) return x;
    
    const double hi = std::max(x, y); // could be branchless on some archs
    const double lo = std::min(x, y); // could be branchless on some archs
    const double diff = lo - hi;  // always ≤ 0
    
    if (diff < -NATS)
        return hi;
    else if (diff == 0.0)
        return hi + std::numbers::ln2;
    else
        return hi + std::log1p(std::exp(diff));
}

// log(exp(x) - exp(y)), requires x >= y
// domain: (-inf, +inf), with x >= y
inline double logdiff(double x, double y)
{
    assert(x >= y);
    
    // Handle -inf cases explicitly
    if (y == -std::numeric_limits<double>::infinity()) {
        // log(exp(x) - 0) = x
        return x;
    }
    if (x == -std::numeric_limits<double>::infinity()) {
        // Then y must also be -inf by precondition; log(0 - 0) = -inf
        return -std::numeric_limits<double>::infinity();
    }
    
    if (x == y) {
        return -std::numeric_limits<double>::infinity();
    }
    
    const double diff = y - x;  // always ≤ 0
    
    if (diff < -NATS) {
        // exp(y)/exp(x) is below double precision; exp(x) - exp(y) ≈ exp(x)
        return x;
    }
    
    // Accurate for all diff ∈ (-NATS, 0)
    // -expm1(diff) is in (0, 1] since diff ∈ (-NATS, 0)
    return x + std::log(-std::expm1(diff));
}

#endif
