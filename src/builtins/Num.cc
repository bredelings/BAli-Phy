#include "computation/machine/args.H"
#include "computation/haskell/Integer.H"
#include "util/utf8.H"
#include <cstdint>
#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

// Convert integer-valued Char arithmetic back to a Unicode scalar value.
// This keeps old Num Char hooks from wrapping through byte-sized chars.
static char32_t checked_char_code(const integer& x, const char* function_name)
{
    if (x < 0 or x > 0x10FFFF)
        throw myexception()<<function_name<<": result "<<x<<" is not a Unicode scalar value.";

    auto c = static_cast<char32_t>(x.convert_to<std::uint32_t>());
    if (not utf8::is_scalar_value(c))
        throw myexception()<<function_name<<": result "<<x<<" is not a Unicode scalar value.";

    return c;
}

//********** Builtins for Num Int ****************//

extern "C" R::Exp simple_function_add_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return integer(x + y);
}

extern "C" R::Exp simple_function_subtract_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return integer(x - y);
}

extern "C" R::Exp simple_function_multiply_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();
    integer y = get_arg(args).as_integer();

    return integer(x * y);
}

extern "C" R::Exp simple_function_abs_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    return integer((x < 0) ? -x : x);
}


extern "C" R::Exp simple_function_negate_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    return integer(-x);
}

extern "C" R::Exp simple_function_signum_integer(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    integer result = (x > 0 ? 1 : 0) - (x < 0 ? 1 : 0);

    return result;
}

//********** Builtins for Num Int ****************//

extern "C" R::Exp simple_function_add_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x + y };
}

extern "C" R::Exp simple_function_subtract_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x - y };
}

extern "C" R::Exp simple_function_multiply_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();
    int y = get_arg(args).as_int();

    return { x * y };
}

extern "C" R::Exp simple_function_abs_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();

    return { std::abs(x) };
}


extern "C" R::Exp simple_function_negate_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();

    return { -x };
}

extern "C" R::Exp simple_function_signum_int(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();

    auto result = (x > 0 ? 1 : 0) - (x < 0 ? 1 : 0);

    return { result };
}


extern "C" R::Exp simple_function_integerToInt(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    int result = x.convert_to<int>();

    return result;
}


extern "C" R::Exp simple_function_intToInteger(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();

    return integer(x);
}


//********** Builtins for Num Char ****************//

extern "C" R::Exp simple_function_add_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return checked_char_code(integer(static_cast<std::uint32_t>(x)) + integer(static_cast<std::uint32_t>(y)), "add Char");
}

extern "C" R::Exp simple_function_subtract_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return checked_char_code(integer(static_cast<std::uint32_t>(x)) - integer(static_cast<std::uint32_t>(y)), "subtract Char");
}

extern "C" R::Exp simple_function_multiply_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();
    auto y = get_arg(args).as_char();

    return checked_char_code(integer(static_cast<std::uint32_t>(x)) * integer(static_cast<std::uint32_t>(y)), "multiply Char");
}

extern "C" R::Exp simple_function_abs_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();

    return checked_char_code(integer(static_cast<std::uint32_t>(x)), "abs Char");
}


extern "C" R::Exp simple_function_negate_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();

    return checked_char_code(-integer(static_cast<std::uint32_t>(x)), "negate Char");
}

extern "C" R::Exp simple_function_signum_char(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();

    auto result = x > 0 ? 1 : 0;

    return checked_char_code(integer(result), "signum Char");
}

extern "C" R::Exp simple_function_integerToChar(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    return checked_char_code(x, "integerToChar");
}

// UNUSED - 2026
extern "C" R::Exp simple_function_charToInteger(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();

    return integer(static_cast<std::uint32_t>(x));
}


extern "C" R::Exp simple_function_intToChar(vector<R::Exp>& args)
{
    int x = get_arg(args).as_int();

    return checked_char_code(integer(x), "intToChar");
}


extern "C" R::Exp simple_function_charToInt(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_char();

    return { static_cast<int>(x) };
}


//********** Builtins for Num Double ****************//

extern "C" R::Exp simple_function_add_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x + y };
}

extern "C" R::Exp simple_function_subtract_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x - y };
}

extern "C" R::Exp simple_function_multiply_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();
    auto y = get_arg(args).as_double();

    return { x * y };
}

extern "C" R::Exp simple_function_abs_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();

    return { std::abs(x) };
}


extern "C" R::Exp simple_function_negate_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();

    return { -x };
}

extern "C" R::Exp simple_function_signum_double(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_double();

    double result = (x > 0.0 ? 1.0 : 0.0) - (x < 0.0 ? 1.0 : 0.0);

    return {result};
}

extern "C" R::Exp simple_function_integerToDouble(vector<R::Exp>& args)
{
    integer i = get_arg(args).as_integer();
    return {i.convert_to<double>()};
}

extern "C" R::Exp simple_function_intToDouble(vector<R::Exp>& args)
{
    int i = get_arg(args).as_int();
    return {double(i)};
}

//********** Builtins for Num LogDouble ****************//

extern "C" R::Exp simple_function_add_logdouble(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() + y.as_log_double()};
}

extern "C" R::Exp simple_function_subtract_logdouble(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() - y.as_log_double()};
}

extern "C" R::Exp simple_function_multiply_logdouble(vector<R::Exp>& args)
{
    auto x = get_arg(args);
    auto y = get_arg(args);

    return {x.as_log_double() * y.as_log_double()};
}

extern "C" R::Exp simple_function_signum_logdouble(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_log_double();

    log_double_t result = (x > 0.0 ? 1.0 : 0.0);

    return {result};
}


extern "C" R::Exp simple_function_intToLogDouble(vector<R::Exp>& args)
{
    auto x = get_arg(args).as_int();

    log_double_t result(x);

    return {result};
}


extern "C" R::Exp simple_function_integerToLogDouble(vector<R::Exp>& args)
{
    integer x = get_arg(args).as_integer();

    log_double_t result(x.convert_to<double>());

    return {result};
}
