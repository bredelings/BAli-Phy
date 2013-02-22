#include "computation/computation.H"
#include "myexception.H"
#include <algorithm>

using boost::dynamic_pointer_cast;

typedef Box<std::pair<object_ref,object_ref>> Pair;

extern "C" closure builtin_function_c_pair(OperationArgs& Args)
{
  const object_ref fst = Args.evaluate(0);
  const object_ref snd = Args.evaluate(1);

  return Pair({fst,snd});
}
