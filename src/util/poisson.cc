#include "util/poisson.hh"

#include <cmath>
#include <limits>
#include <numbers>
#include <numeric>
#include <boost/math/special_functions/log1p.hpp>

// For a = k+1, the inclusive Poisson CDF is Q(a,mu). Evaluate either tail directly
// using Temme's expansion (DLMF 8.12.4), avoiding cancellation when that tail is small.
double large_poisson_tail(double mu, double k, double offset, bool upper_tail)
{
    // Beyond 40 standard deviations the Poisson Chernoff bounds give a rounded tail of 0 or 1.
    if (std::abs(offset) > 40*std::sqrt(mu))
        return upper_tail ? offset > 0 : offset < 0;

    // Subtract k before the inclusive one-count shift. In this window |d| < 0.00062,
    // so evaluate c0 = 1/d - 1/eta by its Taylor expansion, including at d=0.
    const double a = k + 1;
    const double d = (offset - 1)/a;
    const double z = std::copysign(std::sqrt(-a*boost::math::log1pmx(d)), d);
    const double c0 = -1.0/3 + d*(1.0/12 - d*23.0/540);
    const double correction = c0*std::exp(-z*z)/(std::sqrt(2*std::numbers::pi)*std::sqrt(a));
    if (upper_tail) return 0.5*std::erfc(-z) - correction;
    return 0.5*std::erfc(z) + correction;
}

// Find the first k with F(k) >= u. At the cap the sampled CDF is one, so the answer is
// always in [lo, hi]. In the upper tail F(k) >= u is equivalent to P(X > k) <= 1-u.
std::int64_t capped_large_poisson_quantile(double mu, double u)
{
    const bool upper_tail = u > 0.5;
    const double target = upper_tail ? 1 - u : u;
    std::int64_t lo = 0, hi = std::numeric_limits<std::int64_t>::max();
    while (lo < hi)
    {
        const auto k = std::midpoint(lo, hi);
        // Both parts convert exactly to double. Near the mean, subtracting the high part
        // first preserves the low bits that would disappear in mu - double(k).
        const auto low = k % (std::int64_t{1} << 32);
        const double offset = (mu - double(k - low)) - double(low);
        const double tail = large_poisson_tail(mu, double(k), offset, upper_tail);
        if (upper_tail ? tail <= target : tail >= target)
            hi = k;
        else
            lo = k + 1;
    }
    return lo;
}
