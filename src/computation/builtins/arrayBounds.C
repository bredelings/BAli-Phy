#include "computation/computation.H"
#include "computation/graph_register.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  object_ptr<const expression> A = convert<const expression>( Args.lazy_evaluate(0).exp );
  int N = A->sub.size()-1;

  //\todo FIXME:wrong_way! We shouldn't be calling graph_normalize here.

  return graph_normalize(Tuple(0,N-1));
}
