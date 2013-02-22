#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_new_vector(OperationArgs& Args)
{
  int length = *Args.evaluate_as<Int>(0);

  object_ptr<Vector<object_ref>> v (new Vector<object_ref>);

  v->t.resize(length);

  return v;
}
