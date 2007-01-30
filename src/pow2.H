#ifndef POW2_H
#define POW2_H

#include <cmath>
#include <cstdlib>
#include "config.h"

#ifndef HAVE_LOG2
#ifdef M_LOG2E
inline double log2(double x)
{
  return log(x)*M_LOG2E;
}
#elif M_LOG2_E
inline double log2(double x)
{
  return log(x)*M_LOG2_E;
}
#endif
#endif

namespace fp_scale {

  const unsigned max = 50;
  const unsigned shift = max;

  extern double table[max*2+1];

  inline double pow2(int i) {
    if (std::abs(i) > max)
      return exp2(i);
    else
      return table[i+shift];
  }

  void initialize();

  // 11 bits for exponent in double precision
  // 10 bits for negative exponents: exponent range in [0,1024)
  // 9 bits usable if we keep at half the exponent range: [0,512) 
  const double cutoff = 1.0e-154;  // 2**-512 == 10**-154
}

using fp_scale::pow2;

#endif
