/*
   Copyright (C) 2004-2006,2009 Benjamin Redelings

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

#include "probability/probability.hh"
#include "util/math/neumaier.hh"
#include "util/math/count-probability.hh"
#include "util/poisson.hh"
#include <iostream>
#include <boost/math/special_functions/gamma.hpp>
#include <boost/math/special_functions/beta.hpp>
#include <boost/math/distributions.hpp>
#include "util/myexception.hh"
#include "util/log-level.hh"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include "util/wrap.hh"
#include "util/range.hh"

using std::valarray;
using std::vector;

using boost::math::quantile;

log_double_t double_factorial(int n) 
{
  log_double_t x = 1;
  for(int i=3;i<=n;i+=2)
    x *= i;
  return x;
}

log_double_t num_branches(int n) {
  return 2*n-3;
}

log_double_t num_topologies(int n) {
  return double_factorial(2*n-5);
}

log_double_t num_topologies_in_partition(int n1,int n2) 
{
  log_double_t total = num_topologies(n1) * num_topologies(n2);
  total *= num_branches(n1) * num_branches(n2);
  return total;
}

double log_gamma(double x) {
  assert(x>0.0);
  return lgamma(x);
}

using count_probability::log_two_pi;
using count_probability::stirling_remainder;
using count_probability::count_deviance;
using count_probability::log_poisson_mass;

log_double_t dirichlet_pdf(const valarray<double>& p,const valarray<double>& n) 
{
  assert(p.size() == n.size());

  log_double_t Pr = 1;
  for(int i=0;i<p.size();i++) 
    Pr *= pow(log_double_t(p[i]),n[i]-1.0);

  // This term is constant in p
  Pr.log() += log_gamma(n.sum());
  for(int i=0;i<p.size();i++)
    Pr.log() -= log_gamma(n[i]);

  return Pr;
}

log_double_t dirichlet_pdf(const vector<double>& p,const vector<double>& n) 
{
  assert(p.size() == n.size());

  log_double_t Pr = 1;
  for(int i=0;i<p.size();i++) 
    Pr *= pow(log_double_t(p[i]),n[i]-1.0);

  // This term is constant in p
  Pr.log() += log_gamma(sum(n));
  for(int i=0;i<p.size();i++)
    Pr.log() -= log_gamma(n[i]);

  return Pr;
}

log_double_t dirichlet_pdf(const valarray<double>& p,double N,const valarray<double>& q)
{
  return dirichlet_pdf(p,N*p.size()*q);
}

log_double_t dirichlet_pdf(const valarray<double>& p,double N) {
  return dirichlet_pdf(p,valarray<double>(N,p.size()));
}

valarray<double> safe_count(valarray<double> n)
{
  for(int i=0;i<n.size();i++)
    if (n[i] < 1.0) n[i] = 1.0;
  return n;
}

log_double_t dirichlet_safe_pdf(const valarray<double>& p,const valarray<double>& n) 
{
  return dirichlet_pdf(p,safe_count(n));
}

log_double_t dirichlet_safe_pdf(const valarray<double>& p,double N, const valarray<double>& q) 
{
  return dirichlet_safe_pdf(p,N*p.size()*q);
}

log_double_t uniform_pdf(double x, double a, double b)
{
  // A uniform distribution requires finite, strictly ordered bounds.  NaN distinguishes an
  // invalid conditional distribution from an ordinary value outside a valid support interval.
  if (not std::isfinite(a) or not std::isfinite(b) or not (a < b) or std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();

  if (x < a or x > b) return 0;

  // Finite endpoints can have an overflowing width. Its half remains representable;
  // divide in log space to retain densities smaller than the range of a Double.
  const double width = b-a;
  if (std::isfinite(width)) return 1.0/log_double_t(width);
  return 0.5/log_double_t(b/2-a/2);
}

// Interpolate the cumulative probability inside the interval. Scaling both differences
// by one half avoids overflow when finite endpoints have an unrepresentable width.
double uniform_cdf(double x, double a, double b)
{
    if (not std::isfinite(a) or not std::isfinite(b) or not (a < b) or std::isnan(x))
        return std::numeric_limits<double>::quiet_NaN();
    if (x <= a) return 0;
    if (x >= b) return 1;
    const double width = b-a;
    if (std::isfinite(width)) return (x-a)/width;
    return (x/2-a/2)/(b/2-a/2);
}

// Use the same overflow-safe interpolation as sampling, preserving the interval endpoints.
double uniform_quantile(double p, double a, double b)
{
    if (not std::isfinite(a) or not std::isfinite(b) or not (a < b) or std::isnan(p))
        return std::numeric_limits<double>::quiet_NaN();
    return std::lerp(a, b, p);
}

// For ordered signed endpoints the unsigned difference is exact, including across zero.
static double inclusive_count(std::int64_t a, std::int64_t b)
{
    const auto distance = std::uint64_t(b) - std::uint64_t(a);
    return distance == std::numeric_limits<std::uint64_t>::max() ? 0x1p64 : static_cast<double>(distance + 1);
}

log_double_t uniform_int_pdf(std::int64_t x, std::int64_t a, std::int64_t b)
{
  // Reversed bounds do not define a normalized distribution.  Keep that distinct from an
  // ordinary out-of-support value so exceptional-density MCMC can repair the bounds first.
  if (b < a)
    return std::numeric_limits<double>::quiet_NaN();

  if (x < a or x > b) return 0;

  return 1.0/log_double_t(inclusive_count(a, b));
}

// Count integers <= x using wide arithmetic. Bound checks keep flooring and conversion
// inside the Int support, even when the observation is infinite.
double uniform_int_cdf(double x, std::int64_t a, std::int64_t b)
{
    if (b < a or std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
    // Compare integer endpoints in integer space so adjacent values beyond 2^53 stay distinct.
    if (x < -0x1p63) return 0;
    if (x >= 0x1p63) return 1;
    auto k = static_cast<std::int64_t>(std::floor(x));
    if (k < a) return 0;
    if (k >= b) return 1;
    return inclusive_count(a,k) / inclusive_count(a,b);
}

/// log density for y if y=ln (x+delta), and x ~ Exp(mu)

/// f(x) = exp(-x/mu)/mu   g(y) = exp(-(exp(y)-delta)/mu)/mu * exp(y)
log_double_t exp_exponential_pdf(double y, double mu, double delta) {
  double x = exp(y)-delta;
  assert(x >= 0);

  return exp_to_log_space(-x/mu + y)/mu;
}

log_double_t exponential_pdf(double x, double mu) {
  if (x < 0) 
    return 0;

  return exp_to_log_space(-x/mu)/mu;
}

log_double_t laplace_pdf(double x, double mu, double b) 
{
  return exp_to_log_space(-std::abs(x-mu)/b)/(2.0*b);
}

// Integrate the two exponential halves of the Laplace density. Each branch uses
// a nonpositive exponent, including at infinite observations.
double laplace_cdf(double x, double m, double s)
{
    if (not std::isfinite(m) or not std::isfinite(s) or not (s > 0))
        return std::numeric_limits<double>::quiet_NaN();
    const double z = (x-m)/s;
    return z <= 0 ? 0.5*std::exp(z) : 1.0-0.5*std::exp(-z);
}

// Invert the two exponential CDF branches using logarithms of tail probabilities.
// Handle the infinite endpoints explicitly and return the median exactly.
double laplace_quantile(double p, double m, double s)
{
    if (not std::isfinite(m) or not std::isfinite(s) or not (s > 0) or not (p >= 0 and p <= 1))
        return std::numeric_limits<double>::quiet_NaN();
    if (p == 0) return -std::numeric_limits<double>::infinity();
    if (p == 1) return std::numeric_limits<double>::infinity();
    if (p == 0.5) return m;
    if (p < 0.5) return m + s*(std::log(p) + std::log(2.0));
    return m - s*(std::log1p(-p) + std::log(2.0));
}

log_double_t cauchy_pdf(double x, double l, double s)
{
  double C = (x-l)/s;
  C = M_PI*s*(1.0+C*C);
  log_double_t C2 = C;
  return pow(C2,-1.0);
}

log_double_t beta_pdf(double p_,double a, double b) 
{
  if (p_ <= 0.0 or p_ >= 1.0)
      return 0;

  log_double_t p = p_;
  log_double_t q = 1.0-p_;

  return pow(p,a-1)*pow(q,b-1)/boost::math::beta(a,b);
}

double beta_quantile(double p, double a, double b)
{
  using boost::math::beta_distribution;

  //  std::cerr<<" p = "<<p<<" a = "<<a<<" b = "<<b<<std::endl;
  if (a<0 or b<0)
    a=b=1;

  // Avoid values BOOST can't handle: could we approximate with e.g. normal here?
  if (a > 100 or b > 100 or a < 0.1 or b < 0.1)
    return a/(a+b);

  //  std::cerr<<" * p = "<<p<<" a = "<<a<<" b = "<<b<<std::endl;
  double x = quantile(beta_distribution<>(a,b),p);
  //  std::cerr<<" ** x = "<<x<<" p = "<<p<<" a = "<<a<<" b = "<<b<<std::endl;
  return x;
}

log_double_t gamma_pdf(double x,double a, double b) 
{
  if (x < 0) 
    return 0;

  log_double_t Pr = 1;

  // At shape 1 the power term is zero, including at x=0 where 0*log(0) would be NaN.
  double log_power = (a == 1) ? 0 : (a-1)*log(x);
  Pr.log() = log_power - x/b - log_gamma(a) - a*log(b);

  return Pr;
}

log_double_t normal_pdf(double x, double mu, double sigma)
{
  assert(sigma >= 0);

  log_double_t Pr = 1;
  // Standardize before squaring and take log(sigma) directly to avoid losing the
  // scale to overflow/underflow. Halve z first so its square need not be representable.
  const double z = (x-mu)/sigma;
  Pr.log() = -log(sigma) - 0.5*log(2.0*M_PI) - (0.5*z)*z;

  return Pr;
}

double normal_quantile(double p, double mu, double sigma)
{
  using boost::math::normal_distribution;

  assert(p >= 0);
  assert(p <= 1);
  assert(sigma >= 0);
  if (sigma == 0.0) return mu;
  // The unbounded support gives infinite endpoint quantiles; avoid Boost's overflow exception.
  if (std::isfinite(mu) and std::isfinite(sigma) and sigma > 0)
  {
      if (p == 0) return -std::numeric_limits<double>::infinity();
      if (p == 1) return std::numeric_limits<double>::infinity();
  }
  try
  {
      return quantile(normal_distribution<>(mu,sigma),p);
  }
  catch(const std::exception& e)
  {
      if (log_verbose >= 2) std::cerr<<"Warning: normal_quantile (p="<<p<<", mu="<<mu<<", sigma="<<sigma<<"), "<<e.what()<<std::endl;
      return 1;
  }
}

double cauchy_quantile(double p, double m, double s)
{
  using boost::math::cauchy_distribution;

  assert(p >= 0);
  assert(p <= 1);
  assert(s >= 0);
  if (s == 0.0) return m;
  // The unbounded support gives infinite endpoint quantiles; avoid Boost's overflow exception.
  if (std::isfinite(m) and std::isfinite(s) and s > 0)
  {
      if (p == 0) return -std::numeric_limits<double>::infinity();
      if (p == 1) return std::numeric_limits<double>::infinity();
  }
  try
  {
      return quantile(cauchy_distribution<>(m,s),p);
  }
  catch(const std::exception& e)
  {
      if (log_verbose >= 2) std::cerr<<"Warning: cauchy_quantile (p="<<p<<", m="<<m<<", s="<<s<<"), "<<e.what()<<std::endl;
      return m;
  }
}

log_double_t log_normal_pdf(double x, double mu, double sigma)
{
  assert(sigma >= 0);

  log_double_t Pr = normal_pdf(log(x),mu,sigma)/x;
  return Pr;
}

double log_normal_quantile(double p, double lmu, double lsigma)
{
  using boost::math::lognormal_distribution;

  assert(p >= 0);
  assert(p <= 1);
  assert(lsigma >= 0);

  // don't go crazy
  lsigma = minmax(lsigma, 1.0e-5, 1.0e5);

  return quantile(lognormal_distribution<>(lmu, lsigma),p);
}

static double pointChi2(double prob, double v)
{
  // Returns z so that Prob{x<z}=prob where x is Chi2 distributed with df
  // = v
  // RATNEST FORTRAN by
  // Best DJ & Roberts DE (1975) The percentage points of the
  // Chi2 distribution. Applied Statistics 24: 385-388. (AS91)

  double e = 0.5e-6, aa = 0.6931471805, p = prob, g;
  double xx, c, ch, a = 0, q = 0, p1 = 0, p2 = 0, t = 0, x = 0, b = 0, s1, s2, s3, s4, s5, s6;
    
  if (v <= 0)
    throw myexception()<<"Arguments out of range: v = "<<v;

  assert(p >= 0 and p <= 1);

  if (p < 0.000002) 
  {
    if (log_verbose >= 2) std::cerr<<"Warning: can't handle p = "<<p<<" in gamma quantile: using 0.000002\n";

    p = 0.000002;
  }

  if (p > 0.999998) 
  {
    if (log_verbose >= 2) std::cerr<<"Warning: can't handle p = "<<p<<" in gamma quantile: using 0.999998\n";

    p = 0.999998;
  }

  g = log_gamma(v / 2);
    
  xx = v / 2;
  c = xx - 1;
  if (v < -1.24 * log(p)) {
    ch = pow((p * xx * exp(g + xx * aa)), 1 / xx);
    if (ch - e < 0) {
      return ch;
    }
  } else {
    if (v > 0.32) {
      x = normal_quantile(p,0,1);
      p1 = 0.222222 / v;
      ch = v * pow((x * sqrt(p1) + 1 - p1), 3.0);
      if (ch > 2.2 * v + 6) {
	ch = -2 * (log(1 - p) - c * log(.5 * ch) + g);
      }
    } else {
      ch = 0.4;
      a = log(1 - p);

      do {
	q = ch;
	p1 = 1 + ch * (4.67 + ch);
	p2 = ch * (6.73 + ch * (6.66 + ch));
	t = -0.5 + (4.67 + 2 * ch) / p1
	  - (6.73 + ch * (13.32 + 3 * ch)) / p2;
	ch -= (1 - exp(a + g + .5 * ch + c * aa) * p2 / p1)
	  / t;
      } while (std::abs(q / ch - 1) - .01 > 0);
    }
  }
  do {
    q = ch;
    p1 = 0.5 * ch;

    t = boost::math::gamma_p(xx,p1);

    if (t < 0)
      throw myexception()<<"Arguments out of range: t < 0";

    p2 = p - t;
    t = p2 * exp(xx * aa + g + p1 - c * log(ch));
    b = t / ch;
    a = 0.5 * t - b * c;

    s1 = (210 + a * (140 + a * (105 + a * (84 + a * (70 + 60 * a))))) / 420;
    s2 = (420 + a * (735 + a * (966 + a * (1141 + 1278 * a)))) / 2520;
    s3 = (210 + a * (462 + a * (707 + 932 * a))) / 2520;
    s4 = (252 + a * (672 + 1182 * a) + c * (294 + a * (889 + 1740 * a))) / 5040;
    s5 = (84 + 264 * a + c * (175 + 606 * a)) / 2520;
    s6 = (120 + c * (346 + 127 * c)) / 5040;
    ch += t
      * (1 + 0.5 * t * s1 - b
	 * c
	 * (s1 - b
	    * (s2 - b
	       * (s3 - b
		  * (s4 - b * (s5 - b * s6))))));
  } while (std::abs(q / ch - 1) > e);

  if (std::isnan(prob)) throw myexception()<<"prob = "<<prob;
  if (std::isnan(v))    throw myexception()<<"v = "<<v;
  if (not (ch >= 0))    throw myexception()<<"ch = "<<ch;

  return (ch);
}

double gamma_quantile_no_approx(double p, double a, double b)
{
    assert(a >= 0);
    assert(b >= 0);

    assert(p >= 0);
    assert(p <= 1);

    return 0.5 * b * pointChi2(p, 2.0* a);
}

double gamma_quantile(double p, double a, double b)
{
    assert(a >= 0);
    assert(b >= 0);
    assert(p >= 0);
    assert(p <= 1);

    constexpr double infinity = std::numeric_limits<double>::infinity();

    try
    {
        if (std::isnan(p) or std::isnan(a) or std::isnan(b))
        {
            throw myexception()<<"bad value";
        }

        if (a == 0.0 && std::isinf(b) or (std::isinf(a) && b == 0.0))
        {
            throw myexception()<<"bad value";
            return 1;
        }

        if (a == 0.0 or b == 0.0) return 0.0;

        if (p == 0.0) return 0.0;

        if (p == 1.0) return infinity;

        if (std::isinf(a) or std::isinf(b))
        {
            throw myexception()<<"bad value";
            return infinity;
        }

        if (a < 10000)
            return gamma_quantile_no_approx(p,a,b);
        else
        {
            double sigma2 =  log1p(1.0/a); // log1p(V/(M*M)) , V = a*b*b
            double sigma = sqrt(sigma2);
            double mu = log(a*b) - sigma2/2.0; // log(M) - sigma2/2.0, M = a*b
      
            return log_normal_quantile(p, mu ,sigma);
        }
    }
    catch(const std::exception& e)
    {
        if (log_verbose >= 2) std::cerr<<"Warning: gamma_quantile (p="<<p<<", a="<<a<<", b="<<b<<"), "<<e.what()<<std::endl;
        return 1;
    }
}


// Loader's binomial formula groups the factorials and probability powers into two
// Poisson deviances and three Stirling remainders. Both observed counts are exact before
// conversion, and both log probabilities are available even when exp(log_p) underflows.
static double log_binomial_mass(std::uint64_t n, std::uint64_t k, double log_p, double log_q)
{
    const auto failures = n - k;
    if (k == 0) return n == 0 ? 0 : n*log_q;
    if (failures == 0) return n*log_p;

    // For small n the log factorials are modest, so direct evaluation also works in the tails.
    if (n < 32)
        return std::lgamma(double(n) + 1) - std::lgamma(double(k) + 1) - std::lgamma(double(failures) + 1)
            + k*log_p + failures*log_q;

    const double log_n = std::log(double(n));
    return stirling_remainder(n) - stirling_remainder(k) - stirling_remainder(failures)
        - count_deviance(k, n*std::exp(log_p), log_n + log_p)
        - count_deviance(failures, n*std::exp(log_q), log_n + log_q)
        - 0.5*(log_two_pi + std::log(double(k)) + std::log(double(failures)) - log_n);
}

// For r successes and k preceding failures, the last trial is a success. Conditional
// on r successes in r+k trials this has probability r/(r+k), by exchangeability.
log_double_t negative_binomial_pdf_from_logs(std::int64_t r, double log_p, double log_q, std::int64_t k)
{
    assert(r >= 0);
    assert(std::isnan(log_p) or log_p <= 0);
    assert(std::isnan(log_q) or log_q <= 0);

    if (k < 0) return 0;
    if (r == 0 or log_q == -INFINITY) return (k == 0) ? 1 : 0;
    if (log_p == -INFINITY) return 0;

    const auto n = std::uint64_t(r) + k;
    return exp_to_log_space(log_binomial_mass(n, r, log_p, log_q) - std::log1p(double(k)/r));
}

log_double_t negative_binomial_pdf(int r, double p, int k)
{
    assert(p > 0.0 and p <= 1.0);
    return negative_binomial_pdf_from_logs(r, std::log(p), std::log1p(-p), k);
}

// Evaluate the binomial density using the logarithms of both complementary probabilities.
log_double_t binomial_pdf_from_logs(std::int64_t n, double log_p, double log_q, std::int64_t k)
{
    assert(n >= 0);
    assert(std::isnan(log_p) or log_p <= 0);
    assert(std::isnan(log_q) or log_q <= 0);

    if (k < 0 or k > n) return 0;
    if (log_p == -INFINITY) return (k == 0) ? 1 : 0;
    if (log_q == -INFINITY) return (k == n) ? 1 : 0;
    if (std::isnan(log_p) or std::isnan(log_q))
        return exp_to_log_space(std::numeric_limits<double>::quiet_NaN());
    return exp_to_log_space(log_binomial_mass(n, k, log_p, log_q));
}

log_double_t binomial_pdf(int n, double p, int k)
{
    assert(p >= 0.0 and p <= 1.0);
    return binomial_pdf_from_logs(n, std::log(p), std::log1p(-p), k);
}

log_double_t L_gamma(double x)
{
    return exp_to_log_space(std::lgamma(x));
}

log_double_t L_factorial(std::int64_t k)
{
    return L_gamma(double(k)+1);
}

log_double_t L_beta(double x, double y)
{
    return L_gamma(x) * L_gamma(y) / L_gamma(x+y);
}

// Remove the probability powers from the binomial log mass at its mode, where the
// deviance is zero. log1p retains the smaller contribution even if n-k rounds to n.
static double log_choose_count(std::int64_t n, std::int64_t k)
{
    const auto rest = n - k;
    if (k == 0 or rest == 0) return 0;
    if (k == 1 or rest == 1) return std::log(double(n));
    const double log_p = -std::log1p(double(rest)/k);
    const double log_q = -std::log1p(double(k)/rest);
    return log_binomial_mass(n, k, log_p, log_q) - (k*log_p + rest*log_q);
}

// Conditional on their total n, independent Poisson counts with means n*p[i] have
// multinomial probabilities. Evaluate their log masses with the same deviance as the Poisson.
ProbDensity multinomial_pdf(std::int64_t n, std::span<const log_double_t> weights,
                            std::span<const std::int64_t> ks)
{
    assert(weights.size() == ks.size());
    std::int64_t sum = 0;
    size_t peak = 0;
    bool invalid_weight = false;
    for (size_t i = 0; i < ks.size(); ++i)
    {
        const auto k = ks[i];
        if (k < 0 or k > n - sum) return 0;
        sum += k;
        const double w = weights[i].log();
        if (w > weights[peak].log()) peak = i;
        if (std::isnan(w) or w == INFINITY) invalid_weight = true;
    }
    if (n != sum) return 0;
    if (n == 0) return 1;

    // Removing k observations at zero weight leaves choose(remaining,k) arrangements
    // and k symbolic zeros. For undefined normalizations retain the supplied factors.
    ProbDensity coefficient = 1;
    auto remaining = n;
    std::int64_t zero_factors = 0;
    const double max_log_weight = weights[peak].log();
    NeumaierAccumulator<double> other_weights;
    for (size_t i = 0; i < weights.size(); ++i)
    {
        const double w = weights[i].log();
        if (invalid_weight or w == -INFINITY)
        {
            if (ks[i] == 0) continue;
            coefficient *= exp_to_log_space(log_choose_count(remaining, ks[i]));
            if (w == -INFINITY) zero_factors += ks[i];
            else coefficient *= pow(ProbDensity(weights[i]), ks[i]);
            remaining -= ks[i];
        }
        else if (i != peak)
            other_weights += std::exp(w - max_log_weight);
    }
    // Count zeros in Int space before conversion to the multiplicity used by ProbDensity.
    if (zero_factors) coefficient *= pow(ProbDensity(0), zero_factors);
    if (remaining == 0) return coefficient;

    // Exclude one largest weight from the sum so that log1p retains a small tail.
    // Subtract the maximum before the log normalizer to cancel a common weight scale.
    const double log_total = std::log1p(other_weights.total());
    const double log_n = std::log(double(remaining));
    NeumaierAccumulator<double> mass;
    for (size_t i = 0; i < weights.size(); ++i)
    {
        if (weights[i].log() == -INFINITY) continue;
        const double log_p = (weights[i].log() - max_log_weight) - log_total;
        // A single occupied category has mass p^remaining, including when p is close to one.
        if (ks[i] == remaining)
        {
            double log_mass = remaining*log_p;
            if (i == peak and log_total < std::numeric_limits<double>::min())
            {
                // For a subnormal tail t, n*log1p(t) equals n*t to Double precision.
                // Scale each tail weight by n before exponentiating to avoid losing that product.
                NeumaierAccumulator<double> tail;
                for (size_t j = 0; j < weights.size(); ++j)
                    if (j != peak) tail += std::exp(log_n + (weights[j].log() - max_log_weight));
                log_mass = -tail.total();
            }
            return coefficient * ProbDensity(exp_to_log_space(log_mass));
        }
        const double log_mean = log_n + log_p;
        const double mean = remaining * std::exp(log_p);
        mass += log_poisson_mass(ks[i], mean, log_mean);
    }
    mass -= log_poisson_mass(remaining, remaining, log_n);
    return coefficient * ProbDensity(exp_to_log_space(mass.total()));
}

// For failures k before a positive real shape s, expand the three log gammas using
// Stirling remainders. The two deviance offsets are opposite: k*p - s*q and its negative.
static double log_negative_binomial_mass(double k, double shape, double log_p, double log_q, double offset)
{
    if (k == 0) return shape * log_p;
    const double total = k + shape;
    const double log_total = std::log(total);
    const double log_k = std::log(k);
    const double log_shape_fraction = -log1pexp(log_k - std::log(shape));
    return stirling_remainder(total) - stirling_remainder(shape) - stirling_remainder(k)
        + 0.5*(log_shape_fraction - log_k - log_two_pi)
        - count_probability::count_deviance_from_offset(k, -offset, log_total + log_q)
        - count_probability::count_deviance_from_offset(shape, offset, log_total + log_p);
}

// Independent negative-binomial counts with shapes a and b and a common success probability
// have a beta-binomial split conditional on their sum n. Choose p so the sum has mean n.
log_double_t beta_binomial_pdf(std::int64_t n, double a, double b, std::int64_t k)
{
    assert(n >= 0);
    assert(std::isnan(a) or a >= 0);
    assert(std::isnan(b) or b >= 0);
    if (k < 0 or k > n) return 0;
    const auto rest = n - k;

    // Keep the original beta-function behavior for degenerate or nonfinite shapes.
    if (not (a > 0 and b > 0 and std::isfinite(a) and std::isfinite(b)))
    {
        auto pr = L_beta(k+a, rest+b) / L_beta(a,b);
        return pr / L_beta(double(rest)+1, double(k)+1) / (double(n)+1);
    }
    if (n == 0) return 1;
    if (a == 1 and b == 1) return exp_to_log_space(-std::log1p(double(n)));

    const double log_n = std::log(double(n));
    const double log_odds = logsum(std::log(a), std::log(b)) - log_n;
    const double log_p = -log1pexp(-log_odds);
    const double log_q = -log1pexp(log_odds);

    // k*p - a*q = p*(k - n*a/(a+b)). Use the rarer shape's fraction and its count
    // to retain a small expected count even if the other fraction rounds to one.
    const double ratio = std::min(a,b) / std::max(a,b);
    const double fraction = ratio / (1 + ratio);
    const double offset = std::exp(log_p) * (a < b ? -std::fma(double(n), fraction, -double(k))
                                                    : std::fma(double(n), fraction, -double(rest)));

    // For the sum both deviance terms vanish. Its shape fraction is exactly p.
    const double shape = a + b;
    const double log_sum_mass = stirling_remainder(double(n)+shape) - stirling_remainder(shape)
        - stirling_remainder(n) + 0.5*(log_p - log_n - log_two_pi);
    return exp_to_log_space(log_negative_binomial_mass(k, a, log_p, log_q, offset)
                            + log_negative_binomial_mass(rest, b, log_p, log_q, -offset) - log_sum_mass);
}

log_double_t bernoulli_pdf(double p, int k)
{
  if (k == 0)
    return (1.0-p);
  else if (k == 1)
    return p;
  else
    return 0.0;
}

log_double_t geometric_pdf(double p, std::int64_t n)
{
  return geometric_pdf(1.0-p, p, n);
}

log_double_t geometric_pdf(log_double_t p, std::int64_t n)
{
  return geometric_pdf(1.0-p, p, n);
}

log_double_t geometric_pdf(log_double_t p_fail, log_double_t p_success, std::int64_t n)
{
  if (n < 0)
    return 0.0;

  log_double_t pr = p_success;
  if (n>0)
    pr *= pow(p_fail,double(n));

  return pr;
}


// For failures before the first success, F(x) = 1-q^(floor(x)+1). Using log(q)
// and expm1 preserves small CDFs without subtracting nearly equal numbers.
double geometric_cdf_from_logs(double log_p, double log_q, double x)
{
    if (std::isnan(x) or not (log_p <= 0) or not (log_q <= 0)
        or (log_p == -INFINITY and log_q == -INFINITY))
        return std::numeric_limits<double>::quiet_NaN();
    if (x < 0) return 0;
    if (x == INFINITY or log_q == -INFINITY) return 1;
    if (log_p == -INFINITY) return 0;
    return -std::expm1((std::floor(x) + 1)*log_q);
}

// The inclusive Poisson CDF is Q(floor(x)+1, mu), the regularized upper incomplete
// gamma function. Handle support endpoints and the point mass at zero explicitly.
double poisson_cdf(double mu, double x)
{
    if (not std::isfinite(mu) or mu < 0 or std::isnan(x))
        return std::numeric_limits<double>::quiet_NaN();
    if (x < 0) return 0;
    if (x == INFINITY or mu == 0) return 1;
    const double k = std::floor(x);
    if (mu < large_poisson_mean) return boost::math::gamma_q(k + 1, mu);
    return large_poisson_tail(mu, k, mu - k, false);
}

// For k=floor(x), F(x)=I_q(n-k,k+1)=1-I_p(k+1,n-k). Choose the smaller
// probability argument so rounding its complement to one does not erase a tail.
double binomial_cdf_from_logs(std::int64_t n, double log_p, double log_q, double x)
{
    if (n < 0 or std::isnan(x) or not (log_p <= 0) or not (log_q <= 0)
        or (log_p == -INFINITY and log_q == -INFINITY))
        return std::numeric_limits<double>::quiet_NaN();
    if (x < 0) return 0;
    if (x >= 0x1p63 or log_p == -INFINITY) return 1;
    // For nonnegative x, truncation gives floor(x). Subtract before conversion to Double
    // so that a threshold one trial below n stays one trial below it above 2⁵³.
    auto k = static_cast<std::int64_t>(x);
    if (k >= n) return 1;
    if (log_q == -INFINITY) return 0;
    if (log_p < log_q)
        return boost::math::ibetac(k + 1, n - k, std::exp(log_p));
    return boost::math::ibeta(n - k, k + 1, std::exp(log_q));
}

// Counting failures before r successes gives F(x)=I_p(r,floor(x)+1).
// The complementary beta identity lets us use the smaller of p and q directly.
double negative_binomial_cdf_from_logs(std::int64_t r, double log_p, double log_q, double x)
{
    if (r < 0 or std::isnan(x) or not (log_p <= 0) or not (log_q <= 0)
        or (log_p == -INFINITY and log_q == -INFINITY))
        return std::numeric_limits<double>::quiet_NaN();
    if (x < 0) return 0;
    if (x == INFINITY or r == 0 or log_q == -INFINITY) return 1;
    if (log_p == -INFINITY) return 0;
    double k = std::floor(x);
    if (log_p < log_q)
        return boost::math::ibeta(double(r), k + 1, std::exp(log_p));
    return boost::math::ibetac(k + 1, double(r), std::exp(log_q));
}

log_double_t poisson_pdf(double mu, std::int64_t k)
{
    assert(mu >= 0);

    if (k < 0) return 0;
    if (not std::isfinite(mu))
        return exp_to_log_space(std::numeric_limits<double>::quiet_NaN());
    if (k == 0) return exp_to_log_space(-mu);
    if (mu == 0) return 0;

    return exp_to_log_space(log_poisson_mass(k, mu, std::log(mu)));
}

// With d=|lambda-mu|, K=rho*lambda and A=(1-exp(-d*t))/d (A=t at d=0),
// B=exp(-d*t)+K*A for a supercritical process, and B=1+K*A otherwise.
// Then p1=rho*exp(-d*t)/B^2 and q=K*A/B describe the sampled descendant count.
// Sum nonnegative terms when K<d: writing B=1+(K-d)*A would lose its small tail.
static double birth_death_log_d(double lambda, double mu, double rho, double t)
{
    double d = std::abs(lambda-mu);
    double a = d == 0 ? t : -std::expm1(-d*t)/d;
    double k = rho*lambda;
    if (rho != 1 and lambda > mu and k < d)
        return logsum(-d*t, std::log(k) + std::log(a));
    // Preserve the original complete-sampling calculation, including very small mu.
    double coefficient = rho == 1 ? std::min(lambda,mu) : (lambda > mu ? k-d : k);
    double product = coefficient*a;
    if (std::isfinite(product)) return std::log1p(product);
    return std::log(coefficient) + std::log(a);
}

double birth_death_log_p1(double lambda, double mu, double rho, double t)
{
    return std::log(rho) - std::abs(lambda-mu)*t - 2*birth_death_log_d(lambda,mu,rho,t);
}

double birth_death_log_survival(double lambda, double mu, double rho, double t)
{
    return std::log(rho) - std::max(mu-lambda,0.0)*t - birth_death_log_d(lambda,mu,rho,t);
}

// H=-log(1-F) for the proper branching-age CDF F=q/qInfinity. For small rho in
// a supercritical process, use H=log(1+K*A*exp(d*t)) to avoid cancellation.
double birth_death_hazard(double lambda, double mu, double rho, double t)
{
    double d = std::abs(lambda-mu);
    if (rho != 1 and lambda > mu)
    {
        double a = -std::expm1(-d*t)/d;
        return log1pexp(std::log(rho*lambda) + std::log(a) + d*t);
    }
    return d*t + birth_death_log_d(lambda,mu,rho,t);
}

// qInfinity=K/(K+max(mu-lambda,0)); compute log F from its log tail rather than
// subtracting probabilities near one. This also supplies the sampled-count normalizer.
double birth_death_log_q(double lambda, double mu, double rho, double t)
{
    double h = birth_death_hazard(lambda,mu,rho,t);
    double log_q_infinity = rho == 1 ? std::log(lambda/std::max(lambda,mu))
                                    : -std::log1p(std::max(mu-lambda,0.0)/(rho*lambda));
    return log_q_infinity + std::log(-std::expm1(-h));
}

// Invert h=-log(1-F(t)): t=log1p((d/M)*expm1(h))/d, M=K+max(mu-lambda,0).
// The critical limit is expm1(h)/M. Evaluate large arguments in log space;
// M can be less than d, so the old complete-sampling log-sum rearrangement does not apply.
double birth_death_inverse_hazard(double lambda, double mu, double rho, double h)
{
    double d = std::abs(lambda-mu);
    double M = rho == 1 ? std::max(lambda,mu) : rho*lambda + std::max(mu-lambda,0.0);
    if (h == 0) return 0;
    if (d == 0) return h < 500 ? std::expm1(h)/M : std::exp(h-std::log(M));
    if (h < 500)
    {
        double z = (d/M)*std::expm1(h);
        if (std::isfinite(z)) return std::log1p(z)/d;
    }
    return log1pexp(std::log(d)-std::log(M) + h + std::log(-std::expm1(-h)))/d;
}
