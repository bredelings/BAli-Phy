#ifndef UTIL_POISSON_H
#define UTIL_POISSON_H

#include <cstdint>

// At and above this mean use the large-shape expansion of the incomplete gamma function.
inline constexpr double large_poisson_mean = 0x1p32;

// P(X <= k), or P(X > k) for the upper tail. Supply mu-k before rounding an integer k.
double large_poisson_tail(double mu, double k, double offset, bool upper_tail);

// Invert the large-mean CDF at 0 < u < 1, capping the result at the largest signed count.
std::int64_t capped_large_poisson_quantile(double mu, double u);

#endif
