#ifndef LOGSUM_H
#define LOGSUM_H

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;
const double log_limit = log_0/100;

inline double logsum(double x, double y)
{
  double temp = y-x;
  if (temp > 308 or x < log_limit)
    return y;
  else if (temp < -308 or y < log_limit)
    return x;
  else
    return (x + log(1 + exp(temp)));
}

inline void loginc(double& x,double y) {
  double temp = y-x;
  if (temp < -308 or y < log_limit)
    ;
  else if (temp > 308 or x < log_limit)
    x=y;
  else
    x += log(1 + exp(temp));
}


inline double logsum_nocheck(double x, double y) {
  if (std::abs(x-y) > 308)
    return ((x > y) ? x : y);
  else
    return (x + log(1 + exp(y - x)));
}


inline double logsum(double x, double y, double z) {
  return logsum(x,logsum(y,z));
}

#endif
