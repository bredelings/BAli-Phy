#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/bounds.H"

extern "C" closure builtin_function_get_bounds(OperationArgs& Args)
{
    auto L = Args.evaluate(0).as_<EMaybe>();
    auto U = Args.evaluate(1).as_<EMaybe>();

    Box<bounds<double>> b;

    if (L)
        b.lower_bound = L->as_double();
    if (U)
        b.upper_bound = U->as_double();
  
    return b;
}

extern "C" closure builtin_function_get_integer_bounds(OperationArgs& Args)
{
    auto L = Args.evaluate(0).as_<EMaybe>();
    auto U = Args.evaluate(1).as_<EMaybe>();

    Box<bounds<int>> b;

    if (L)
        b.lower_bound = L->as_int();
    if (U)
        b.upper_bound = U->as_int();
  
    return b;
}
