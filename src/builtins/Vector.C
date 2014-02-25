#include <vector>
#include "computation/computation.H"
#include "myexception.H"
#include "matrix.H"

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

template<typename T, typename U>
closure GetVectorElement(OperationArgs& Args)
{
  object_ptr<const Vector<T>> v = Args.evaluate_as<Vector<T>>(0);
  int i = *Args.evaluate_as<Int>(1);
  
  return U((*v)[i]);
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

extern "C" closure builtin_function_getVectorIntElement(OperationArgs& Args)
{
  return GetVectorElement<int,Int>(Args);
}

extern "C" closure builtin_function_getVectorVectorIntElement(OperationArgs& Args)
{
  return GetVectorElement<Vector<int>,Vector<int>>(Args);
}

extern "C" closure builtin_function_getVectorvectorIntElement(OperationArgs& Args)
{
  return GetVectorElement<vector<int>,Vector<int>>(Args);
}

extern "C" closure builtin_function_sizeOfString(OperationArgs& Args)
{
  const std::string& s = *Args.evaluate_as<String>(0);
  
  return Int(s.size());
}

extern "C" closure builtin_function_getStringElement(OperationArgs& Args)
{
  const std::string& s = *Args.evaluate_as<String>(0);
  int i = *Args.evaluate_as<Int>(1);
  
  return Char(s[i]);
}

extern "C" closure builtin_function_NewString(OperationArgs& Args)
{
  const int& length = *Args.evaluate_as<Int>(0);

  object_ptr<String> v (new String);

  v->resize(length);

  return v;
}

extern "C" closure builtin_function_SetStringIndex(OperationArgs& Args)
{
  object_ptr<const String> v = Args.evaluate_as<String>(0);
  int i = *Args.evaluate_as<Int>(1);
  char x = *Args.evaluate_as<Char>(2);

  const String* vv = &(*v);
  String* vvv = const_cast<String*>(vv);
  (*vvv)[i] = x;

  return constructor("()",0);
}

template <class T>
closure NewVector(OperationArgs& Args)
{
  object_ptr<const Int> length = Args.evaluate_as<Int>(0);

  object_ptr<Vector<T>> v (new Vector<T>);

  v->resize(*length);

  return v;
}

extern "C" closure builtin_function_NewVectorInt(OperationArgs& Args)
{
  return NewVector<int>(Args);
}

extern "C" closure builtin_function_NewVectorDouble(OperationArgs& Args)
{
  return NewVector<double>(Args);
}

extern "C" closure builtin_function_NewVectorMatrix(OperationArgs& Args)
{
  return NewVector<Matrix>(Args);
}

template <typename T, typename U>
closure SetVectorIndex(OperationArgs& Args)
{
  object_ptr<const Vector<T>> v = Args.evaluate_as<Vector<T>>(0);
  int i = *Args.evaluate_as<Int>(1);
  U x = *Args.evaluate_as<U>(2);

  const Vector<T>* vv = &(*v);
  Vector<T>* vvv = const_cast<Vector<T>*>(vv);
  (*vvv)[i] = x;

  return constructor("()",0);
}

extern "C" closure builtin_function_SetVectorIndexInt(OperationArgs& Args)
{
  return SetVectorIndex<int,Int>(Args);
}

extern "C" closure builtin_function_SetVectorIndexDouble(OperationArgs& Args)
{
  return SetVectorIndex<double,Double>(Args);
}

extern "C" closure builtin_function_SetVectorIndexMatrix(OperationArgs& Args)
{
  return SetVectorIndex<Matrix,MatrixObject>(Args);
}

template<typename T, typename U>
closure Vector_From_List(OperationArgs& Args)
{
  object_ptr<Box<std::vector<T> > > v (new Box<std::vector<T> >);

  const closure* top = &Args.evaluate_slot_to_closure(0);
  while(top->exp->size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp->size() == 2);

    int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    v->push_back( *convert<const U>(Args.evaluate_reg_to_object(element_reg)) );
    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  return v;
}

extern "C" closure builtin_function_Vector_Matrix_From_List(OperationArgs& Args)
{
  return Vector_From_List<Matrix,MatrixObject>(Args);
}

extern "C" closure builtin_function_new_vector(OperationArgs& Args)
{
  int length = *Args.evaluate_as<Int>(0);

  object_ptr<OVector> v = new OVector(length);

  return v;
}

extern "C" closure builtin_function_vector_size(OperationArgs& Args)
{
  const Vector<object_ref>& v = *Args.evaluate_as<Vector<object_ref>>(0);

  return Int(v.size());
}

extern "C" closure builtin_function_set_vector_index(OperationArgs& Args)
{
  const Vector<object_ref>& v = *Args.evaluate_as<Vector<object_ref>>(0);
  int i = *Args.evaluate_as<Int>(1);
  object_ref x = Args.evaluate(2);

  const Vector<object_ref>* vv = &v;
  Vector<object_ref>* vvv = const_cast<Vector<object_ref>*>(vv);
  (*vvv)[i] = x;

  return constructor("()",0);
}

extern "C" closure builtin_function_get_vector_index(OperationArgs& Args)
{
  int i = *Args.evaluate_as<Int>(1);
  const OVector& v = *Args.evaluate_as<OVector>(0);

  return v[i];
}

