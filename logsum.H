#ifndef LOGSUM_H
#define LOGSUM_H

#include <cmath>

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;
const double log_limit = log_0/100;

// For Pentium I (exp & log are base 2)
// fadd = 3.1
// fmul = 3,1
// f2xm1 = 13-57
// fyl2x = 22-111
// fdiv  = 39

// NATS is 52*log(2) for 52 bits of precision
// HMM... the long doubles have 64 bits of precision...
const double NATS = 303;

inline double logsum(double x, double y)
{
  double temp = y-x;
  if (temp > NATS or x < log_limit)
    return y;
  else if (temp < -NATS or y < log_limit)
    return x;
  else
    return (x + log(1 + exp(temp)));
}

inline void loginc(double& x,double y) {
  double temp = y-x;
  if (temp < -NATS or y < log_limit)
    ;
  else if (temp > NATS or x < log_limit)
    x=y;
  else
    x += log(1 + exp(temp));
}


inline double logsum_nocheck(double x, double y) {
  if (std::abs(x-y) > NATS)
    return ((x > y) ? x : y);
  else
    return (x + log(1 + exp(y - x)));
}

inline double logdiff1(double delta) {
  assert(delta <= 0);

  if (delta < -NATS)
    return 0;

  else if (delta < -NATS/3) {
    double x = exp(-delta);
    return -(x + x*x/2 + x*x*x/3);
  }

  else {
      double y=0;
      if (delta < -.00001) 
	y = 1.0 - exp(delta);
      else
	y = -(delta + delta*delta/2.0 + delta*delta*delta/6.0 + delta*delta*delta*delta/24.0);
      return log(y);
  }
}

inline double logdiff(double x, double y) {
  assert(x > y);
  return x + logdiff1(y-x);
}

inline double logsum(double x, double y, double z) {
  return logsum(x,logsum(y,z));
}
#endif
