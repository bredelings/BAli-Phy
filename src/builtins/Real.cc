#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/bool.H"
#include "computation/haskell/Integer.H"

#include <cmath>
#include <cstdint>

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

extern "C" expression_ref simple_function_exp(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"exp: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();

    return exp(xx);
}

extern "C" expression_ref simple_function_expm1(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"expm1: object '"<<x.print()<<"' is not double!";

    double xx = x.as_double();
    return expm1(xx);
}

extern "C" expression_ref simple_function_log(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (x.is_double())
    {
	double xx = x.as_double();
	assert(xx > 0.0);
	return log(xx);
    }
    else if (x.is_log_double())
    {
	log_double_t xx = x.as_log_double();
	return log(xx);
    }

    throw myexception()<<"log: object '"<<x.print()<<"' is not double or log_double";
}

extern "C" expression_ref simple_function_log1p(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    assert(xx >= -1.0);
    return log1p(xx);
}

extern "C" expression_ref simple_function_log1pexp(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return log1pexp(xx);
}

extern "C" expression_ref simple_function_log1mexp(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (not x.is_double())
        throw myexception()<<"log1p: object '"<<x.print()<<"' is not double!";

    // QUESTION: should we implement this for logdouble?

    double xx = x.as_double();
    return log1mexp(xx);
}

extern "C" expression_ref simple_function_pow(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

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
	return pow(xx,yy);
    }
    else if (x.is_int())
    {
	double xx = x.as_int();
	return pow(xx,yy);
    }
    else if (x.is_log_double())
    {
	log_double_t xx = x.as_log_double();
	return pow(xx,yy);
    }

    throw myexception()<<"pow: object '"<<x.print()<<"' is not double, int, or log_double";
}

extern "C" expression_ref simple_function_sqrt(vector<expression_ref>& args)
{
    auto x = get_arg(args);

    if (x.is_double())
	return sqrt(x.as_double());
    else if (x.is_log_double())
	return sqrt(x.as_log_double());
    else
	std::abort();
}

extern "C" expression_ref simple_function_logBase(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto base = get_arg(args);

    if (x.is_double())
	return log(x.as_double())/log(base.as_double());
    else if (x.is_log_double())
	return log(x.as_log_double())/log(base.as_log_double());
    else
	std::abort();
}

extern "C" expression_ref simple_function_sin(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return sin(x);
}

extern "C" expression_ref simple_function_tan(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return tan(x);
}

extern "C" expression_ref simple_function_cos(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return cos(x);
}

extern "C" expression_ref simple_function_asin(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return asin(x);
}

extern "C" expression_ref simple_function_atan(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return atan(x);
}

extern "C" expression_ref simple_function_acos(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return acos(x);
}

extern "C" expression_ref simple_function_sinh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return sinh(x);
}

extern "C" expression_ref simple_function_tanh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return tanh(x);
}

extern "C" expression_ref simple_function_cosh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return cosh(x);
}

extern "C" expression_ref simple_function_asinh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return asinh(x);
}

extern "C" expression_ref simple_function_atanh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return atanh(x);
}

extern "C" expression_ref simple_function_acosh(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return acosh(x);
}

extern "C" expression_ref simple_function_isDoubleNaN(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return std::isnan(x);
}

extern "C" expression_ref simple_function_isDoubleFinite(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return std::isfinite(x);
}

extern "C" expression_ref simple_function_isDoubleInfinite(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return std::isinf(x);
}

extern "C" expression_ref simple_function_isDoubleDenormalized(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return std::fpclassify(x) == FP_SUBNORMAL;
}

extern "C" expression_ref simple_function_isDoubleNegativeZero(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();

    return x == 0 and std::signbit(x);
}

extern "C" expression_ref simple_function_atan2_double(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();
    double y = get_arg(args).as_double();

    return atan2(x,y);
}

extern "C" expression_ref simple_function_decodeDoubleRaw(vector<expression_ref>& args)
{
    double x = get_arg(args).as_double();
    int64_t sig = 0;
    int exp = 0;
    if (x)
    {
	sig = (int64_t)std::scalbn(std::frexp(x,&exp), DBL_MANT_DIG);
	exp -= DBL_MANT_DIG;
    }
    return EPair(Integer(sig),exp);
}

extern "C" expression_ref simple_function_encodeDouble(vector<expression_ref>& args)
{
    auto sig = (int64_t)get_arg(args).as_<Integer>();
    auto exp = get_arg(args).as_int();

    return { std::ldexp(sig,exp) };
}

extern "C" expression_ref simple_function_integerToInvLogOdds(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

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
