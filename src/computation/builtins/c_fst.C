#include "computation/computation.H"
#include "myexception.H"
#include <algorithm>

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_c_fst(OperationArgs& Args)
{
  const OPair& p = *Args.evaluate_as<OPair>(0);

  return p.t.first;
}
