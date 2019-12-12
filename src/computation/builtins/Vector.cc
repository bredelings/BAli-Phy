#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include "computation/operation.H"
#include "util/myexception.H"
#include "util/matrix.H"
#include "computation/machine/graph_register.H"
#include "computation/machine/args.H"
#include "computation/expression/reg_var.H"
#include "computation/expression/index_var.H"
#include "computation/expression/list.H"
#include "computation/expression/constructor.H"

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

extern "C" closure builtin_function_vector_size(OperationArgs& Args)
{
    const EVector& v = Args.evaluate(0).as_<EVector>();

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

extern "C" closure builtin_function_list_to_vector(OperationArgs& Args)
{
    auto xs = Args.evaluate(0);

    object_ptr<EVector> v (new EVector);

    auto E2 = xs;
    while(E2.is_a<EPair>())
    {
        v->push_back(E2.as_<EPair>().first);
        E2 = E2.as_<EPair>().second;
    }
    return v;
}

extern "C" closure builtin_function_list_to_string(OperationArgs& Args)
{
    auto xs = Args.evaluate(0);

    object_ptr<String> s (new String);

    auto E2 = xs;
    while(E2.is_a<EPair>())
    {
        int c = E2.as_<EPair>().first.as_char();
        (*s) += c;
        E2 = E2.as_<EPair>().second;
    }
    return s;
}

extern "C" closure builtin_function_fromVectors(OperationArgs& Args)
{
    // This doesn't distinguish between a 0x0, 2x0 or 2x0 matrix.

    // If I really want something like the Haskell matrix, I could use an EVector of EVectors.
    // Then I could get a matrix of anything -- integers, doubles, log_doubles, etc.

    auto arg = Args.evaluate(0);
    auto& V = arg.as_<EVector>();
    int I = V.size();
    if (I <= 0)
        return Box<Matrix>();

    int J = V[0].as_<EVector>().size();
    if (J <= 0)
        return Box<Matrix>();

    auto M = new Box<Matrix>(I,J);
    for(int i=0;i<I;i++)
        for(int j=0;j<J;j++)
            (*M)(i,j) = V[i].as_<EVector>()[j].as_double();

    return M;
}
