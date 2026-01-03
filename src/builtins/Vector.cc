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

extern "C" expression_ref simple_function_sizeOfString(vector<expression_ref>& args)
{
    return (int)get_arg(args).as_<String>().size();
}

extern "C" expression_ref simple_function_getStringElement(vector<expression_ref>& args)
{
    auto arg0 = get_arg(args);
    const std::string& s = arg0.as_<String>();
    int i = get_arg(args).as_int();

    return s[i];
}

extern "C" closure builtin_function_cppSubString(OperationArgs& Args)
{
    int offset = Args.evaluate(1).as_int();
    int length = Args.evaluate(2).as_int();
    const std::string& s = Args.evaluate(0).as_<String>();

    if (offset == 0 and length == s.size())
	return {index_var(0),{Args.reg_for_slot(0)}};
    else
	return {String(s.substr(offset,length))};
}

extern "C" expression_ref simple_function_vector_size(vector<expression_ref>& args)
{
    auto arg0 = get_arg(args);
    const EVector& v = arg0.as_<EVector>();

    return (int)v.size();
}

extern "C" closure builtin_function_set_vector_index(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    const EVector& v = arg0.as_<EVector>();
    int i = Args.evaluate(1).as_int();
    auto x = Args.evaluate(2);

    const EVector* vv = &v;
    EVector* vvv = const_cast<EVector*>(vv);
    (*vvv)[i] = x;

    return constructor("()",0);
}

extern "C" expression_ref simple_function_get_vector_index(vector<expression_ref>& args)
{
    auto arg0 = get_arg(args);
    int i = get_arg(args).as_int();
    const EVector& v = arg0.as_<EVector>();

    return v[i];
}

extern "C" closure builtin_function_clist_to_vector(OperationArgs& Args)
{
    expression_ref xs = Args.evaluate(0);

    object_ptr<EVector> v (new EVector);

    for(auto E2 = xs; not E2.is_int(); E2 = E2.as_<EPair>().second)
    {
        assert(E2.is_a<EPair>());
        v->push_back(E2.as_<EPair>().first);
    }

    return v;
}

extern "C" closure builtin_function_clist_to_string(OperationArgs& Args)
{
    expression_ref xs = Args.evaluate(0);

    object_ptr<String> s (new String);

    for(auto E2 = xs; not E2.is_int(); E2 = E2.as_<EPair>().second)
    {
        assert(E2.is_a<EPair>());
        (*s) += E2.as_<EPair>().first.as_char();
    }

    return s;
}

extern "C" closure builtin_function_emptyString(OperationArgs& /*Args*/)
{
    object_ptr<String> s (new String);

    return s;
}

extern "C" closure builtin_function_showObject(OperationArgs& Args)
{
    auto arg = Args.evaluate(0);
    String result = arg.print();
    return result;
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

extern "C" closure builtin_function_matrixToVector(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& M = arg0.as_<Box<Matrix>>();

    object_ptr<EVector> Vptr = new EVector;
    auto& V = *Vptr;

    for(int i=0;i<M.size1();i++)
        for(int j=0;j<M.size2();j++)
            V.push_back(M(i,j));

    return V;
}

extern "C" closure builtin_function_vectorToMatrix(OperationArgs& Args)
{
    int s1 = Args.evaluate(0).as_int();
    int s2 = Args.evaluate(1).as_int();
    auto arg2 = Args.evaluate(2);
    auto& V = arg2.as_<EVector>();

    if (V.size() != s1*s2)
        throw myexception()<<"vectorToMatrix: size = ("<<s1<<", "<<s2<<") so expected "<<s1*s2<<" elements, but got "<<V.size()<<"!";

    auto Mptr = new Box<Matrix>(s1, s2);
    auto& M = *Mptr;

    int k=0;
    for(int i=0;i<s1;i++)
        for(int j=0;j<s2;j++)
            M(i,j) = V[k++].as_double();

    return Mptr;
}
