#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_vector_size(OperationArgs& Args)
{
  const Vector<object_ref>& v = *Args.evaluate_as<Vector<object_ref>>(0);

  return Int(v->t.size());
}
