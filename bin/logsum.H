#ifndef LOGSUM_H
#define LOGSUM_H

const double max_float = 3.40282347e+38F;

const double log_0 = -max_float;

inline double logsum(double r, double q)
{
  if (std::abs(r-q) > 308)
    return ((r > q) ? r : q);
  
  return (r + log(1 + exp(q - r)));
}

inline double logsum(double x, double y, double z) {
  return logsum(x,logsum(y,z));
}

#endif
