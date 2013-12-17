#include "computation/computation.H"
#include "sequence/alphabet.H"
#include "dp/2way.H"
#include "imodel/imodel.H"

using std::vector;

extern "C" closure builtin_function_rs07_lengthp(OperationArgs& Args)
{
  double e = *Args.evaluate_as<Double>(0);
  int l = *Args.evaluate_as<Int>(1);

  if (l < 0)
    return Double(0);
  else if (l==0)
    return Double(1.0);
  else
    return Double(1.0-e);
}
