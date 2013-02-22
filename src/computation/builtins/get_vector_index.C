#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_get_vector_index(OperationArgs& Args)
{
  const Vector<object_ref>& v = *Args.evaluate_as<Vector<object_ref>>(0);
  int i = *Args.evaluate_as<Int>(1);

  return v.t[i];
}
