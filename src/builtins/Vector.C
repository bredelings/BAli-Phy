#include <vector>
#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;
using std::vector;

template<typename T>
closure VectorSize(OperationArgs& Args)
{
  object_ptr<const Box<std::vector<T> > > v = Args.evaluate_as<Box<std::vector<T> > >(0);
  
  const std::vector<T>& v2 = *v;
  object_ptr<const Int> r ( new Int(v2.size() ) );
  
  return r;
}

extern "C" closure builtin_function_sizeOfVectorUnsigned(OperationArgs& Args)
{
  return VectorSize<unsigned>(Args);
}

extern "C" closure builtin_function_sizeOfVectorInt(OperationArgs& Args)
{
  return VectorSize<int>(Args);
}

extern "C" closure builtin_function_sizeOfVectorVectorInt(OperationArgs& Args)
{
  return VectorSize<Vector<int>>(Args);
}

extern "C" closure builtin_function_sizeOfVectorvectorInt(OperationArgs& Args)
{
  return VectorSize<vector<int>>(Args);
}
