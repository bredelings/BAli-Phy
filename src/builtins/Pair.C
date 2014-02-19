#include "computation/computation.H"
#include "myexception.H"
#include <algorithm>

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_c_fst(OperationArgs& Args)
{
  return Args.evaluate_as<OPair>(0)->first;
}

extern "C" closure builtin_function_c_snd(OperationArgs& Args)
{
  return Args.evaluate_as<OPair>(0)->second;
}

extern "C" closure builtin_function_c_pair(OperationArgs& Args)
{
  const object_ref fst = Args.evaluate(0);
  const object_ref snd = Args.evaluate(1);

  return OPair(Opair{fst,snd});
}
