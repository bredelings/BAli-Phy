#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/bounds.H"

extern "C" closure builtin_function_get_bounds(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& L = arg0.as_<R::RMaybe>();
    auto arg1 = Args.evaluate_slot_to_value(1);
    const auto& U = arg1.as_<R::RMaybe>();

    Box<bounds<double>> b;

    if (L)
        b.lower_bound = L->as_double();
    if (U)
        b.upper_bound = U->as_double();
  
    return b;
}

extern "C" closure builtin_function_get_integer_bounds(OperationArgs& Args)
{
    auto arg0 = Args.evaluate_slot_to_value(0);
    const auto& L = arg0.as_<R::RMaybe>();
    auto arg1 = Args.evaluate_slot_to_value(1);
    const auto& U = arg1.as_<R::RMaybe>();

    Box<bounds<int>> b;

    if (L)
        b.lower_bound = L->as_int();
    if (U)
        b.upper_bound = U->as_int();
  
    return b;
}
