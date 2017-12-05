#include <vector>
#include "computation/computation.H"
#include "myexception.H"
#include "matrix.H"
#include "computation/expression/expression.H"

using boost::dynamic_pointer_cast;
using std::vector;

template<typename T>
closure VectorSize(OperationArgs& Args)
{
    auto v = Args.evaluate(0);
    return {(int)v.as_<Box<std::vector<T> > >().size()};
}

template<typename T>
closure GetVectorElement(OperationArgs& Args)
{
    auto v = Args.evaluate(0);
    int i = Args.evaluate(1).as_int();

    const T& t = v.as_<Vector<T>>()[i];
    return {t};
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
    auto arg0 = Args.evaluate(0);
    int i = Args.evaluate(1).as_int();

    return {arg0.as_<Vector<int>>()[i]};
}

extern "C" closure builtin_function_getVectorVectorIntElement(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    int i = Args.evaluate(1).as_int();

    return {arg0.as_<Vector<Vector<int>>>()[i]};
}

extern "C" closure builtin_function_getVectorvectorIntElement(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    int i = Args.evaluate(1).as_int();

    return {Vector<int>(arg0.as_<Vector<vector<int>>>()[i])};
}

extern "C" closure builtin_function_sizeOfString(OperationArgs& Args)
{
    const std::string& s = Args.evaluate(0).as_<String>();
  
    return {(int)s.size()};
}

extern "C" closure builtin_function_getStringElement(OperationArgs& Args)
{
    const std::string& s = Args.evaluate(0).as_<String>();
    int i = Args.evaluate(1).as_int();
  
    return {s[i]};
}

extern "C" closure builtin_function_NewString(OperationArgs& Args)
{
    const int& length = Args.evaluate(0).as_int();

    object_ptr<String> v (new String);

    v->resize(length);

    return v;
}

extern "C" closure builtin_function_SetStringIndex(OperationArgs& Args)
{
    object_ptr<const String> v = Args.evaluate(0).assert_is_a<String>();
    int i = Args.evaluate(1).as_int();
    char x = Args.evaluate(2).as_char();

    const String* vv = &(*v);
    String* vvv = const_cast<String*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

template <class T>
closure NewVector(OperationArgs& Args)
{
    int length = Args.evaluate(0).as_int();

    auto v = new Vector<T>;

    v->resize(length);

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

extern "C" closure builtin_function_SetVectorIndexInt(OperationArgs& Args)
{
    object_ptr<const Vector<int>> v = Args.evaluate(0).assert_is_a<Vector<int>>();
    int i = Args.evaluate(1).as_int();
    int x = Args.evaluate(2).as_int();

    const Vector<int>* vv = &(*v);
    Vector<int>* vvv = const_cast<Vector<int>*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

extern "C" closure builtin_function_SetVectorIndexDouble(OperationArgs& Args)
{
    object_ptr<const Vector<double>> v = Args.evaluate(0).assert_is_a<Vector<double>>();
    int i = Args.evaluate(1).as_int();
    double x = Args.evaluate(2).as_double();

    const Vector<double>* vv = &(*v);
    Vector<double>* vvv = const_cast<Vector<double>*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

extern "C" closure builtin_function_SetVectorIndexMatrix(OperationArgs& Args)
{
    object_ptr<const Vector<Matrix>> v = Args.evaluate(0).assert_is_a<Vector<Matrix>>();
    int i = Args.evaluate(1).as_int();
    Box<Matrix> x = Args.evaluate(2).as_<Box<Matrix>>();

    const Vector<Matrix>* vv = &(*v);
    Vector<Matrix>* vvv = const_cast<Vector<Matrix>*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

extern "C" closure builtin_function_new_vector(OperationArgs& Args)
{
    int length = Args.evaluate(0).as_int();

    object_ptr<EVector> v = new EVector(length);

    return v;
}

extern "C" closure builtin_function_vector_size(OperationArgs& Args)
{
    const Vector<expression_ref>& v = Args.evaluate(0).as_<Vector<expression_ref>>();

    return {(int)v.size()};
}

extern "C" closure builtin_function_set_vector_index(OperationArgs& Args)
{
    const EVector& v = Args.evaluate(0).as_<EVector>();
    int i = Args.evaluate(1).as_int();
    auto x = Args.evaluate(2);

    const EVector* vv = &v;
    EVector* vvv = const_cast<EVector*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

extern "C" closure builtin_function_get_vector_index(OperationArgs& Args)
{
    int i = Args.evaluate(1).as_int();
    const EVector& v = Args.evaluate(0).as_<EVector>();

    return v[i];
}

