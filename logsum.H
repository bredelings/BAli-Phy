#ifndef LOGSUM_H
#define LOGSUM_H

#include <cmath>

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;
const double log_limit = log_0/100;

// For Pentium I (exp & log are base 2)
// fadd = 3,1
// fmul = 3,1
// f2xm1 = 13-57
// fyl2x = 22-111
// fdiv  = 39

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

inline double logdiff(double x, double y) {
  assert(x > y);
  double temp = y-x;
  if (temp < -NATS or x < log_limit)
    return x;
  else
    return (x + log1p(-exp(temp)));
}

inline double logsum(double x, double y, double z) {
  return logsum(x,logsum(y,z));
}
#endif
