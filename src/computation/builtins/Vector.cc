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

extern "C" closure builtin_function_NewString(OperationArgs& Args)
{
    int length = Args.evaluate(0).as_int();

    int state = Args.evaluate(1).as_int();

    object_ptr<String> v (new String);

    v->resize(length);

    // Are we copying the v here?  Because that is wasteful and feels wrong.
    return EPair(state+1, v);
}

extern "C" closure builtin_function_SetStringIndex(OperationArgs& Args)
{
    object_ptr<const String> v = Args.evaluate(0).assert_is_a<String>();
    int i = Args.evaluate(1).as_int();
    char x = Args.evaluate(2).as_char();
    int state = Args.evaluate(3).as_int();

    const String* vv = &(*v);
    String* vvv = const_cast<String*>(vv);
    (*vvv)[i] = x;

    return EPair(state+1,constructor("()",0));
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

extern "C" closure builtin_function_new_vector(OperationArgs& Args)
{
    int length = Args.evaluate(0).as_int();

    object_ptr<EVector> v = new EVector(length);

    return v;
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
    object_ptr<EVector> v (new EVector);

    const closure* top = &Args.evaluate_slot_to_closure(0);
    while(top->exp.size())
    {
	assert(has_constructor(top->exp,":"));
	assert(top->exp.size() == 2);

	int element_reg = top->reg_for_slot(0);

	int next_reg = top->reg_for_slot(1);

	// Add the element to the list.
	v->push_back( Args.evaluate_reg_to_object(element_reg) );
	// Move to the next element or end
	top = &Args.evaluate_reg_to_closure(next_reg);
    }
    assert(has_constructor(top->exp,"[]"));

    return v;
}

// Hmm... maybe we need something to make applying C functions more of a thing.

int allocate_closure(OperationArgs& Args, const expression_ref& E)
{
    if (is_reg_var(E))
        return E.as_<reg_var>().target;

    if (not E.size())
        return Args.allocate({E});

    vector<expression_ref> args;
    for(int i=0;i<E.size();i++)
        args.push_back(index_var(int(E.size())-1-i));
    expression_ref E2(E.head(),args);

    closure C(E2);
    for(int i=0;i<E.size();i++)
    {
        int r = allocate_closure(Args, E.sub()[i]);
        C.Env.push_back(r);
    }
    return Args.allocate(std::move(C));
}

extern Operation unpack_cpp_string;

extern "C" closure builtin_function_unpack_cpp_string(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    int i = Args.evaluate(1).as_int();
    int r_string = Args.evaluate_slot_to_reg(0);
    auto& s = M[r_string].exp.as_checked<String>();

    if (i >= s.size())
        return constructor("[]",0);

    expression_ref all = cons(s[i], expression_ref(unpack_cpp_string,{reg_var(r_string),i+1}));

    int r = allocate_closure(Args, all);

    return {index_var(0),{r}};
}

Operation unpack_cpp_string(2, builtin_function_unpack_cpp_string, "unpack_cpp_string");

