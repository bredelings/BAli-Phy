#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/bounds.H"

extern "C" closure builtin_function_get_bounds(OperationArgs& Args)
{
    auto L = Args.evaluate(0);
    auto U = Args.evaluate(1);

    auto has_lower = L.is_double();
    auto has_upper = U.is_double();

    Box<bounds<double>> b;

    if (has_lower)
        b.lower_bound = L.as_double();
    if (has_upper)
        b.upper_bound = U.as_double();
  
    return b;
}

extern "C" closure builtin_function_get_integer_bounds(OperationArgs& Args)
{
    auto L = Args.evaluate(0);
    auto U = Args.evaluate(1);

    auto has_lower = L.is_int();
    auto has_upper = U.is_int();

    Box<bounds<int>> b;

    if (has_lower)
        b.lower_bound = L.as_int();
    if (has_upper)
        b.upper_bound = U.as_int();
  
    return b;
}
