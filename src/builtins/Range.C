#include "computation/computation.H"
#include "bounds.H"

extern "C" closure builtin_function_get_bounds(OperationArgs& Args)
{
  auto L = Args.evaluate(0);
  auto U = Args.evaluate(1);

  auto has_lower = L.is_double();
  auto has_upper = U.is_double();

  double lower = 0;
  double upper = 0;

  if (has_lower)
    lower = L.as_double();
  if (has_upper)
    upper = U.as_double();
  
  return Bounds<double>((bool)has_lower, lower, (bool)has_upper, upper);
}

extern "C" closure builtin_function_get_integer_bounds(OperationArgs& Args)
{
  auto L = Args.evaluate(0);
  auto U = Args.evaluate(1);

  auto has_lower = L.is_int();
  auto has_upper = U.is_int();

  int lower = 0;
  int upper = 0;

  if (has_lower)
    lower = L.as_int();
  if (has_upper)
    upper = U.as_int();
  
  return Bounds<int>((bool)has_lower, lower, (bool)has_upper, upper);
}
