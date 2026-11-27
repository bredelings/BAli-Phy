#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.hh"
#include "computation/haskell/Integer.hh"

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

extern "C" R::Exp simple_function_exp(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"exp: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();

    return exp(xx);
}

extern "C" R::Exp simple_function_expm1(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"expm1: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();
    return expm1(xx);
}

extern "C" R::Exp simple_function_log(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (x.is_double())
        return log(x.as_double());

    throw myexception()<<"log: object '"<<x.print()<<"' is not double";
}

extern "C" R::Exp simple_function_log1p(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return log1p(xx);
}

extern "C" R::Exp simple_function_log1pexp(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return log1pexp(xx);
}

extern "C" R::Exp simple_function_log1mexp(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return log1mexp(xx);
}

// Add two log-domain values with the established precision cutoff.
extern "C" R::Exp simple_function_logsum(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    if (not x.is_double() or not y.is_double())
        throw myexception()<<"logsum: arguments must be double";

    return logsum(x.as_double(), y.as_double());
}

// Evaluate log(exp(x) - 1) without cancellation near zero or overflow for large x.
extern "C" R::Exp simple_function_logexpm1(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"logexpm1: argument must be double";

    return logexpm1(x.as_double());
}

extern "C" R::Exp simple_function_pow(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    double yy = 0;
    if (y.is_double())
	yy = y.as_double();
    else if (y.is_int())
	yy = y.as_int();
    else
	throw myexception()<<"pow: exponent '"<<y.print()<<"' is not double or int";
    
    if (x.is_double())
    {
	double xx = x.as_double();
	return pow(xx,yy);
    }
    else if (x.is_int())
    {
	double xx = x.as_int();
	return pow(xx,yy);
    }
    throw myexception()<<"pow: object '"<<x.print()<<"' is not double or int";
}

extern "C" R::Exp simple_function_sqrt(vector<R::Exp>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"sqrt: object '"<<x.print()<<"' is not double";

    return sqrt(x.as_double());
}

extern "C" R::Exp simple_function_logBase(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto base = get_arg(args);

    if (not x.is_double() or not base.is_double())
        throw myexception()<<"logBase: arguments must be double";

    return log(x.as_double())/log(base.as_double());
}

extern "C" R::Exp simple_function_sin(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return sin(x);
}

extern "C" R::Exp simple_function_tan(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return tan(x);
}

extern "C" R::Exp simple_function_cos(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return cos(x);
}

extern "C" R::Exp simple_function_asin(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return asin(x);
}

extern "C" R::Exp simple_function_atan(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return atan(x);
}

extern "C" R::Exp simple_function_acos(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return acos(x);
}

extern "C" R::Exp simple_function_sinh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return sinh(x);
}

extern "C" R::Exp simple_function_tanh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return tanh(x);
}

extern "C" R::Exp simple_function_cosh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return cosh(x);
}

extern "C" R::Exp simple_function_asinh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return asinh(x);
}

extern "C" R::Exp simple_function_atanh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return atanh(x);
}

extern "C" R::Exp simple_function_acosh(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return acosh(x);
}

extern "C" R::Exp simple_function_isDoubleNaN(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return std::isnan(x);
}

extern "C" R::Exp simple_function_isDoubleFinite(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return std::isfinite(x);
}

extern "C" R::Exp simple_function_isDoubleInfinite(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return std::isinf(x);
}

extern "C" R::Exp simple_function_isDoubleDenormalized(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return std::fpclassify(x) == FP_SUBNORMAL;
}

extern "C" R::Exp simple_function_isDoubleNegativeZero(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();

    return x == 0 and std::signbit(x);
}

extern "C" R::Exp simple_function_atan2_double(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();
    double y = get_arg(args).as_double();

    return atan2(x,y);
}

extern "C" R::Exp simple_function_decodeDoubleRaw(vector<R::Exp>& args)
{
    double x = get_arg(args).as_double();
    int64_t sig = 0;
    int exp = 0;
    if (x)
    {
	sig = (int64_t)std::scalbn(std::frexp(x,&exp), DBL_MANT_DIG);
	exp -= DBL_MANT_DIG;
    }

    return R::RPair(R::Integer(integer(sig)), exp);
}

// Encode m * 2^e for a positive magnitude larger than 2^53. Choose the final spacing
// before rounding: subnormal Doubles all have the same spacing. Rounding to 53 bits first
// can discard the bit that distinguishes a midpoint from a value just above it.
static double encode_large_magnitude(const integer& m, haskell_int e)
{
    using limits = std::numeric_limits<double>;
    using boost::multiprecision::int128_t;
    constexpr int subnormal_exponent = limits::min_exponent - limits::digits;
    const int128_t top_exponent = int128_t(boost::multiprecision::msb(m)) + e;
    if (top_exponent >= limits::max_exponent) return limits::infinity();
    if (top_exponent < subnormal_exponent - 1) return 0.0;

    const int spacing_exponent = std::max(static_cast<int>(top_exponent) - (limits::digits - 1), subnormal_exponent);
    const auto discarded = static_cast<std::uint64_t>(int128_t(spacing_exponent) - e);
    // At least one bit is discarded. A set rounding bit means at least halfway;
    // at an exact tie (no lower set bit), increment only an odd retained significand.
    auto retained = (m >> discarded).convert_to<std::uint64_t>();
    const auto rounding_bit = static_cast<decltype(boost::multiprecision::msb(m))>(discarded - 1);
    if (boost::multiprecision::bit_test(m, rounding_bit) and
        ((retained & 1) or boost::multiprecision::lsb(m) < rounding_bit))
        ++retained;

    // Even a rounding carry to 2^53 converts exactly. Scaling places the rounded bits.
    return std::ldexp(static_cast<double>(retained), spacing_exponent);
}

extern "C" R::Exp simple_function_encodeDouble(vector<R::Exp>& args)
{
    auto significand = get_arg(args);
    const integer& m = significand.as_integer();
    const auto e = get_arg(args).as_int();
    if (m == 0) return 0.0;

    // Includes every significand returned by decodeFloat. Conversion is exact here,
    // so ldexp supplies the only rounding, including when the result is subnormal.
    constexpr auto exact_limit = std::int64_t{1} << std::numeric_limits<double>::digits;
    if (m >= -exact_limit and m <= exact_limit)
    {
        using limits = std::numeric_limits<int>;
        const auto exponent = std::clamp<haskell_int>(e, limits::min(), limits::max());
        return std::ldexp(static_cast<double>(m.convert_to<std::int64_t>()), static_cast<int>(exponent));
    }

    return m < 0 ? -encode_large_magnitude(-m, e) : encode_large_magnitude(m, e);
}

// For x>1, the IOdds parameter is log(x-1) = log(x)+log1p(-1/x).
// After shifting a huge x, the correction and discarded bits are below double precision.
extern "C" R::Exp simple_function_integerToLogExcess(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    double result = 0;

    int extra = boost::multiprecision::msb(x) - 1019;
    if (extra > 0)
    {
        x >>= extra;

        double p = (double)x;
        result = extra*log(2) + log(p);
    }
    else
    {
	double p = (double)x;
        if (p == 1)
            result = std::numeric_limits<double>::infinity();
        else
            result = log(p) + log1p(-1/p);
    }

    return { result };
}
