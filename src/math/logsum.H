/*
   Copyright (C) 2004 Benjamin Redelings

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
#include <cassert>

using std::exp;
using std::log;

const double max_float = 3.40282347e+38F;
const double log_limit = -max_float/100;
const double log_0 = -max_float;

// For Pentium I (exp & log are base 2)
// fadd = 3,1
// fmul = 3,1
// f2xm1 = 13-57
// fyl2x = 22-111
// fdiv  = 39
// [ and fscale... ?]

// NATS is 52*log(2) for 52 bits of precision
// HMM... the long doubles have 64 bits of precision...
const double NATS = 40;

inline double logsum_nocheck(double x, double y) {
  if (std::abs(x-y) > NATS)
    return ((x > y) ? x : y);
  else
    return (x + log1p(exp(y - x)));
}

inline double logsum(double x, double y)
{
  double temp = y-x;
  if (temp > NATS or x < log_limit)
    return y;
  else if (temp < -NATS or y < log_limit)
    return x;
  else
    return (x + log1p(exp(temp)));
}

inline void loginc(double& x, double y)
{
  double temp = y-x;
  if (temp > NATS or x < log_limit)
    x=y;
  else if (temp < -NATS or y < log_limit)
    ;
  else
    x += log1p(exp(temp));
}

inline double logdiff(double x, double y) {
  assert(x >= y);
  double temp = y-x;
  if (temp < -NATS or y < log_limit)
    return x;
  else if (x == y)
    return log_0;
  else
    return (x + log1p(-exp(temp)));
}
#endif
