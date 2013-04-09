#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_vector_index(OperationArgs& Args)
{
  int i = *Args.evaluate_as<Int>(1);
  const OVector& v = *Args.evaluate_as<OVector>(0);

  return v[i];
}
