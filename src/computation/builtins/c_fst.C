#include "computation/computation.H"
#include "myexception.H"
#include <algorithm>

using boost::dynamic_pointer_cast;

typedef Box<std::pair<object_ref,object_ref>> Pair;

extern "C" closure builtin_function_c_fst(OperationArgs& Args)
{
  const Pair& p = *Args.evaluate_as<Pair>(0);

  return p.t.first;
}
