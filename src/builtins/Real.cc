#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/bool.H"
#include "computation/haskell/Integer.H"

#include <cmath>
#include <cstdint>

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

extern "C" closure builtin_function_exp(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (not x.is_double())
        throw myexception()<<"exp: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();

    return {exp(xx)};
}

extern "C" closure builtin_function_expm1(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (not x.is_double())
        throw myexception()<<"expm1: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();
    return {expm1(xx)};
}

extern "C" closure builtin_function_log(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (x.is_double())
    {
	double xx = x.as_double();
	assert(xx > 0.0);
	return {log(xx)};
    }
    else if (x.is_log_double())
    {
	log_double_t xx = x.as_log_double();
	return {log(xx)};
    }

    throw myexception()<<"log: object '"<<x.print()<<"' is not double or log_double";
}

extern "C" closure builtin_function_log1p(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    assert(xx >= -1.0);
    return {log1p(xx)};
}

extern "C" closure builtin_function_log1pexp(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return {log1pexp(xx)};
}

extern "C" closure builtin_function_log1mexp(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    assert(xx >= 0);
    return {log1mexp(xx)};
}

extern "C" closure builtin_function_pow(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    double yy = 0;
    if (y.is_double())
	yy = y.as_double();
    else if (y.is_int())
	yy = y.as_int();
    else if (y.is_log_double())
        yy = double(y.as_log_double());
    else
	throw myexception()<<"pow: exponent '"<<x.print()<<"' is not double, int, or log_double";
    
    if (x.is_double())
    {
	double xx = x.as_double();
	assert(xx > 0.0);
	return {pow(xx,yy)};
    }
    else if (x.is_int())
    {
	double xx = x.as_int();
	assert(xx > 0.0);
	return {pow(xx,yy)};
    }
    else if (x.is_log_double())
    {
	log_double_t xx = x.as_log_double();
	return {pow(xx,yy)};
    }

    throw myexception()<<"pow: object '"<<x.print()<<"' is not double, int, or log_double";
}

extern "C" closure builtin_function_sqrt(OperationArgs& Args)
{
    auto x = Args.evaluate(0);

    if (x.is_double())
	return {sqrt(x.as_double())};
    else if (x.is_log_double())
	return {sqrt(x.as_log_double())};
    else
	std::abort();
}

extern "C" closure builtin_function_logBase(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto base = Args.evaluate(1);

    if (x.is_double())
	return {log(x.as_double())/log(base.as_double())};
    else if (x.is_log_double())
	return {log(x.as_log_double())/log(base.as_log_double())};
    else
	std::abort();
}

extern "C" closure builtin_function_sin(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {sin(x)};
}

extern "C" closure builtin_function_tan(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {tan(x)};
}

extern "C" closure builtin_function_cos(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {cos(x)};
}

extern "C" closure builtin_function_asin(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {asin(x)};
}

extern "C" closure builtin_function_atan(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {atan(x)};
}

extern "C" closure builtin_function_acos(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {acos(x)};
}

extern "C" closure builtin_function_sinh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {sinh(x)};
}

extern "C" closure builtin_function_tanh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {tanh(x)};
}

extern "C" closure builtin_function_cosh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {cosh(x)};
}

extern "C" closure builtin_function_asinh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {asinh(x)};
}

extern "C" closure builtin_function_atanh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {atanh(x)};
}

extern "C" closure builtin_function_acosh(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {acosh(x)};
}

extern "C" closure builtin_function_isDoubleNaN(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {std::isnan(x)};
}

extern "C" closure builtin_function_isDoubleFinite(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {std::isfinite(x)};
}

extern "C" closure builtin_function_isDoubleInfinite(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {std::isinf(x)};
}

extern "C" closure builtin_function_isDoubleDenormalized(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {std::fpclassify(x) == FP_SUBNORMAL};
}

extern "C" closure builtin_function_isDoubleNegativeZero(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();

    return {x == 0 and std::signbit(x)};
}

extern "C" closure builtin_function_atan2_double(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();
    double y = Args.evaluate(1).as_double();

    return {atan2(x,y)};
}

extern "C" closure builtin_function_decodeDoubleRaw(OperationArgs& Args)
{
    double x = Args.evaluate(0).as_double();
    int64_t sig = 0;
    int exp = 0;
    if (x)
    {
	sig = (int64_t)std::scalbn(std::frexp(x,&exp), DBL_MANT_DIG);
	exp -= DBL_MANT_DIG;
    }
    return EPair(Integer(sig),exp);
}

extern "C" closure builtin_function_encodeDouble(OperationArgs& Args)
{
    auto sig = (int64_t)Args.evaluate(0).as_<Integer>();
    auto exp = Args.evaluate(1).as_int();

    return { std::ldexp(sig,exp) };
}

extern "C" closure builtin_function_integerToInvLogOdds(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    double result = 0;

    int extra = boost::multiprecision::msb(x) - 1019;
    if (extra > 0)
    {
	integer d = boost::multiprecision::pow(integer(2),extra);
	x /= d;

	std::cerr<<"extra = "<<extra<<"\n";

	double p = (double)x;
	result = -extra*log(2) -log(p); // - log1p(-1/(p*d))) which is approximately +1/(p*d)
    }
    else
    {
	double p = (double)x;
	result = (-log(p) - log1p(-1/p));
    }

    return { result };
}
