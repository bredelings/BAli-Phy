#include "computation/machine/args.H"

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"

using boost::dynamic_pointer_cast;
using std::string;
using std::vector;

//********** Builtins for Num Int ****************//

extern "C" closure builtin_function_add_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();
    auto y = Args.evaluate(1).as_int();

    return { x + y };
}

extern "C" closure builtin_function_subtract_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();
    auto y = Args.evaluate(1).as_int();

    return { x - y };
}

extern "C" closure builtin_function_multiply_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();
    auto y = Args.evaluate(1).as_int();

    return { x * y };
}

extern "C" closure builtin_function_abs_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();

    return { std::abs(x) };
}


extern "C" closure builtin_function_negate_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();

    return { -x };
}

extern "C" closure builtin_function_signum_int(OperationArgs& Args)
{
    auto x = Args.evaluate(0).as_int();

    auto result = (x > 0 ? 1 : 0) - (x < 0 ? -1 : 0);

    return { result };
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




