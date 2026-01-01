#include "computation/machine/args.H"
#include "computation/haskell/Integer.H"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

//********** Builtins for Num Int ****************//

extern "C" expression_ref simple_function_add_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return Integer( x + y );
}

extern "C" expression_ref simple_function_subtract_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return Integer( x - y );
}

extern "C" expression_ref simple_function_multiply_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();
    integer y = get_arg(args).as_<Integer>();

    return Integer( x * y );
}

extern "C" expression_ref simple_function_abs_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    return Integer( (x < 0) ? -x : x );
}


extern "C" expression_ref simple_function_negate_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    return Integer( -x );
}

extern "C" expression_ref simple_function_signum_integer(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    integer result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return Integer( result );
}

//********** Builtins for Num Int ****************//

extern "C" expression_ref simple_function_add_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x + y };
}

extern "C" expression_ref simple_function_subtract_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x - y };
}

extern "C" expression_ref simple_function_multiply_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x * y };
}

extern "C" expression_ref simple_function_abs_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();

    return { std::abs(x) };
}


extern "C" expression_ref simple_function_negate_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();

    return { -x };
}

extern "C" expression_ref simple_function_signum_int(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();

    auto result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return { result };
}


extern "C" expression_ref simple_function_integerToInt(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    int result = x.convert_to<int>();

    return expression_ref( result );
}


extern "C" expression_ref simple_function_intToInteger(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();

    return Integer(x);
}


//********** Builtins for Num Char ****************//

extern "C" expression_ref simple_function_add_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return { char(x + y) };
}

extern "C" expression_ref simple_function_subtract_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return { char(x - y) };
}

extern "C" expression_ref simple_function_multiply_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return { char(x * y) };
}

extern "C" expression_ref simple_function_abs_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();

    return { char(std::abs(x)) };
}


extern "C" expression_ref simple_function_negate_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();

    return { char(-x) };
}

extern "C" expression_ref simple_function_signum_char(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_char();

    auto result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return { char(result) };
}

extern "C" expression_ref simple_function_integerToChar(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    char result = x.convert_to<char>();

    return expression_ref( result );
}

// UNUSED - 2026
extern "C" expression_ref simple_function_charToInteger(vector<expression_ref>& args)
{
    char x = get_arg(args).as_char();

    return Integer(x);
}


extern "C" expression_ref simple_function_intToChar(vector<expression_ref>& args)
{
    int x = get_arg(args).as_int();

    return { char(x) };
}


extern "C" expression_ref simple_function_charToInt(vector<expression_ref>& args)
{
    char x = get_arg(args).as_char();

    return {int(x)};
}


//********** Builtins for Num Double ****************//

extern "C" expression_ref simple_function_add_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x + y };
}

extern "C" expression_ref simple_function_subtract_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x - y };
}

extern "C" expression_ref simple_function_multiply_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x * y };
}

extern "C" expression_ref simple_function_abs_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();

    return { std::abs(x) };
}


extern "C" expression_ref simple_function_negate_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();

    return { -x };
}

extern "C" expression_ref simple_function_signum_double(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_double();

    double result = (x > 0.0 ? 1.0 : 0.0) - (x < 0.0 ? -1.0 : 0.0);

    return {result};
}

extern "C" expression_ref simple_function_integerToDouble(vector<expression_ref>& args)
{
    integer i = get_arg(args).as_<Integer>();
    return {i.convert_to<double>()};
}

extern "C" expression_ref simple_function_intToDouble(vector<expression_ref>& args)
{
    int i = get_arg(args).as_int();
    return {double(i)};
}

//********** Builtins for Num LogDouble ****************//

extern "C" expression_ref simple_function_add_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() + y.as_log_double()};
}

extern "C" expression_ref simple_function_subtract_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() - y.as_log_double()};
}

extern "C" expression_ref simple_function_multiply_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() * y.as_log_double()};
}

extern "C" expression_ref simple_function_signum_logdouble(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_log_double();

    log_double_t result = (x > 0.0 ? 1.0 : 0.0);

    return {result};
}


extern "C" expression_ref simple_function_intToLogDouble(vector<expression_ref>& args)
{
    auto x = get_arg(args).as_int();

    log_double_t result(x);

    return {result};
}


extern "C" expression_ref simple_function_integerToLogDouble(vector<expression_ref>& args)
{
    integer x = get_arg(args).as_<Integer>();

    log_double_t result(x.convert_to<double>());

    return {result};
}




