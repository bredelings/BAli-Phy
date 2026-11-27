#ifndef UTIL_COUNT_PROBABILITY_H
#define UTIL_COUNT_PROBABILITY_H

#include <cmath>
#include <cstdint>
#include <numbers>
#include <boost/math/special_functions/log1p.hpp>

namespace count_probability {
inline const double log_two_pi = std::log(2 * std::numbers::pi);

// The remainder in log(x!) = (x+1/2)log(x) - x + log(2π)/2 + remainder.
// For x >= 32, the first omitted Stirling term, 1/(1188 x⁹), is below 3e-17.
inline double stirling_remainder(double x)
{
    if (x < 32)
        return std::lgamma(x + 1) - ((x + 0.5)*std::log(x) - x + 0.5*log_two_pi);
    const double t = 1 / (x*x);
    return (1.0/12 - t*(1.0/360 - t*(1.0/1260 - t/1680))) / x;
}

// Loader's deviance term D(x,m) = x log(x/m) + m - x, for x > 0. Near the mean,
// D = -x (log(1+d)-d), d=(m-x)/x. The supplied log mean survives underflow of m.
inline double count_deviance(double x, double mean, double log_mean)
{
    const double difference = mean - x;
    if (std::abs(difference) < 0.5*x)
        return -x * boost::math::log1pmx(difference/x);
    return x*(std::log(x) - log_mean) + difference;
}

// The separate log mean retains the probability of a positive count if its mean underflows.
inline double log_poisson_mass(std::uint64_t k, double mean, double log_mean)
{
    if (k == 0) return -mean;
    // At small counts the factorial is modest and the direct formula costs fewer operations.
    if (k < 32) return k*log_mean - mean - std::lgamma(double(k) + 1);

    // Substituting Stirling's formula into k log(mean) - mean - log(k!) leaves
    // the deviance and the normalization, even when k and mean are equal.
    return -count_deviance(k, mean, log_mean) - stirling_remainder(k)
           - 0.5*(log_two_pi + std::log(double(k)));
}

}

#endif
