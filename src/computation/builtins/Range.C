#include "computation/computation.H"
#include "bounds.H"

extern "C" closure builtin_function_get_bounds(OperationArgs& Args)
{
  auto L = Args.evaluate(0);
  auto U = Args.evaluate(1);

  auto has_lower = boost::dynamic_pointer_cast<const Double>(L);
  auto has_upper = boost::dynamic_pointer_cast<const Double>(U);

  double lower = 0;
  double upper = 0;

  if (has_lower)
    lower = *has_lower;
  if (has_upper)
    upper = *has_upper;
  
  return Bounds<double>(has_lower, lower, has_upper, upper);
}
