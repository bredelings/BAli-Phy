#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/bool.H"
#include "computation/runtime/ast.H"

namespace
{
    const Runtime::App& array_app(const closure& C)
    {
        auto app = C.get_code().to<Runtime::App>();
        if (not app)
            throw myexception()<<"Expression is not an Array:   "<<C.get_code();

        auto constructor_app = std::get_if<Runtime::ConstructorApp>(&app->head);
        if (not constructor_app or constructor_app->head.name() != "Array")
            throw myexception()<<"Expression is not an Array:   "<<C.get_code();

        return *app;
    }
}

extern "C" closure builtin_function_mkArray(OperationArgs& Args)
{
    int n = Args.evaluate_slot_to_value(0).as_int();

    // We can't do negative-sized arrays
    assert(n >= 0);
    // The function should be represented as a heap variable...
    int f_reg = Args.reg_for_slot(1);
  
    std::vector<Runtime::Exp> elements(n);

    closure result;
    result.Env.resize(n);
    for(int i=0;i<n;i++)
    {
	// i
	int i_reg = Args.allocate(i);

	// %1 %0 {f,i}
	int apply_reg = Args.allocate(closure(Runtime::apply(Runtime::IndexVar(1), {Runtime::IndexVar(0)}),
                                              {f_reg, i_reg}));

	// change to result.exp <<= index_var(i)
	elements[i] = Runtime::IndexVar(n - 1 - i);

	// Add the var to the environment
	result.Env[i] = apply_reg;
    }
    result.set_code(Runtime::App(Runtime::ConstructorApp("Array", n), std::move(elements)));
  
    return result;
}

extern "C" closure builtin_function_arraySize(OperationArgs& Args)
{
    int N = array_app(Args.evaluate_slot_to_closure(0)).args.size();

    return {N};
}

extern "C" closure builtin_function_getIndex(OperationArgs& Args)
{
    extern long total_index_op;
    total_index_op++;

    int n = Args.evaluate_slot_to_value(1).as_int();
    // Do this second, so that evaluation of the 1st argument can't call expand_memory afterwards.
    const closure& C = Args.evaluate_slot_to_closure(0);
    const auto& app = array_app(C);

    int N = app.args.size();
    assert(C.Env.size() == app.args.size());

    if (n < 0 or n >= N)
	throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";

    // Return a reference to the heap variable pointed to by the nth entry
    return closure(Runtime::IndexVar(0), {C.Env[n]});
}

extern "C" closure builtin_function_removeElement(OperationArgs& Args)
{
    int idx = Args.evaluate_slot_to_value(0).as_int();

    // Do this second, so that evaluation of the 1st argument can't call expand_memory afterwards.
    const closure& C = Args.evaluate_slot_to_closure(1);
    const auto& app = array_app(C);

    int n = app.args.size();
    if (idx < 0 or idx >= n)
        throw myexception()<<"Trying to remove element '"<<idx<<"' from an Array of size "<<n<<"!";

    std::vector<Runtime::Exp> elements(n-1);

    closure result;
    result.Env.resize(n-1);
    for(int i=0,j=0; i<n-1; i++,j++)
    {
        if (i==idx) j++;

	// change to result.exp <<= index_var(i)
	elements[i] = Runtime::IndexVar(n - 2 - i);

	// Add the var to the environment
	result.Env[i] = C.Env[j];
    }
    result.set_code(Runtime::App(Runtime::ConstructorApp("Array", n-1), std::move(elements)));

    return result;
}
