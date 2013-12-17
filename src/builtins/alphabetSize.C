#include "computation/computation.H"
#include "sequence/alphabet.H"

extern "C" closure builtin_function_alphabetSize(OperationArgs& Args)
{
  const alphabet& a = *Args.evaluate_as<alphabet>(0);

  return Int(a.size());
}
