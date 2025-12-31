#include "computation/machine/args.H"
#include "computation/haskell/Integer.H"
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

//********** Builtins for Num Int ****************//

extern "C" expression_ref simple_function_add_integer(vector<expression_ref>& args)
{
    integer y = get_arg(args).as_<Integer>();
    integer x = get_arg(args).as_<Integer>();

    return Integer( x + y );
}

extern "C" closure builtin_function_subtract_integer(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();
    integer y = Args.evaluate(1).as_<Integer>();

    return Integer( x - y );
}

extern "C" closure builtin_function_multiply_integer(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();
    integer y = Args.evaluate(1).as_<Integer>();

    return Integer( x * y );
}

extern "C" closure builtin_function_abs_integer(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    return Integer( (x < 0) ? -x : x );
}


extern "C" closure builtin_function_negate_integer(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    return Integer( -x );
}

extern "C" closure builtin_function_signum_integer(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    integer result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return Integer( result );
}

//********** Builtins for Num Int ****************//

extern "C" expression_ref simple_function_add_int(vector<expression_ref>& args)
{
    int y = get_arg(args).as_int();
    int x = get_arg(args).as_int();

    return { x + y };
}

extern "C" expression_ref simple_function_subtract_int(vector<expression_ref>& args)
{
    int y = get_arg(args).as_int();
    int x = get_arg(args).as_int();

    return { x - y };
}

extern "C" expression_ref simple_function_multiply_int(vector<expression_ref>& args)
{
    int y = get_arg(args).as_int();
    int x = get_arg(args).as_int();

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

extern "C" closure builtin_function_add_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();
    auto y = Args.evaluate(1).as_char();

    return { char(x + y) };
}

extern "C" closure builtin_function_subtract_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();
    auto y = Args.evaluate(1).as_char();

    return { char(x - y) };
}

extern "C" closure builtin_function_multiply_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();
    auto y = Args.evaluate(1).as_char();

    return { char(x * y) };
}

extern "C" closure builtin_function_abs_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();

    return { char(std::abs(x)) };
}


extern "C" closure builtin_function_negate_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();

    return { char(-x) };
}

extern "C" closure builtin_function_signum_char(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_char();

    auto result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return { char(result) };
}


extern "C" closure builtin_function_integerToChar(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    char result = x.convert_to<char>();

    return expression_ref( result );
}


extern "C" closure builtin_function_charToInteger(OperationArgs& Args)
{
    char x = Args.evaluate(0).as_char();

    return Integer(x);
}


extern "C" closure builtin_function_intToChar(OperationArgs& Args)
{
    int x = Args.evaluate(0).as_int();

    return { char(x) };
}


extern "C" closure builtin_function_charToInt(OperationArgs& Args)
{
    char x = Args.evaluate(0).as_char();

    return {int(x)};
}


//********** Builtins for Num Double ****************//

extern "C" closure builtin_function_add_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();
    auto y = Args.evaluate(1).as_double();

    return { x + y };
}

extern "C" closure builtin_function_subtract_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();
    auto y = Args.evaluate(1).as_double();

    return { x - y };
}

extern "C" closure builtin_function_multiply_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();
    auto y = Args.evaluate(1).as_double();

    return { x * y };
}

extern "C" closure builtin_function_abs_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();

    return { std::abs(x) };
}


extern "C" closure builtin_function_negate_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();

    return { -x };
}

extern "C" closure builtin_function_signum_double(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_double();

    double result = (x > 0.0 ? 1.0 : 0.0) - (x < 0.0 ? -1.0 : 0.0);

    return {result};
}

extern "C" closure builtin_function_integerToDouble(OperationArgs& Args)
{
    integer i = Args.evaluate(0).as_<Integer>();
    return {i.convert_to<double>()};
}

extern "C" closure builtin_function_intToDouble(OperationArgs& Args)
{
    int i = Args.evaluate(0).as_int();
    return {double(i)};
}

//********** Builtins for Num LogDouble ****************//

extern "C" closure builtin_function_add_logdouble(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    return {x.as_log_double() + y.as_log_double()};
}

extern "C" closure builtin_function_subtract_logdouble(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    return {x.as_log_double() - y.as_log_double()};
}

extern "C" closure builtin_function_multiply_logdouble(OperationArgs& Args)
{
    auto x = Args.evaluate(0);
    auto y = Args.evaluate(1);

    return {x.as_log_double() * y.as_log_double()};
}

extern "C" closure builtin_function_signum_logdouble(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_log_double();

    log_double_t result = (x > 0.0 ? 1.0 : 0.0);

    return {result};
}


extern "C" closure builtin_function_intToLogDouble(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();

    log_double_t result(x);

    return {result};
}


extern "C" closure builtin_function_integerToLogDouble(OperationArgs& Args)
{
    integer x = Args.evaluate(0).as_<Integer>();

    log_double_t result(x.convert_to<double>());

    return {result};
}




