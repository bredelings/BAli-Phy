#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"

extern "C" closure builtin_function_mkArray(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    // We can't do negative-sized arrays
    assert(n >= 0);
    // The function should be represented as a heap variable...
    int f_reg = Args.reg_for_slot(1);
  
    object_ptr<expression> exp = new expression(constructor("Array",n));
    exp->sub.resize(n);

    expression_ref apply_E;
    {
	expression_ref fE = index_var(1);
	expression_ref argE = index_var(0);
	apply_E = {fE, argE};
    }

    closure result;
    result.Env.resize(n);
    for(int i=0;i<n;i++)
    {
	// i
	int i_reg = Args.allocate(expression_ref(i));

	// %1 %0 {f,i}
	int apply_reg = Args.allocate({apply_E,{f_reg, i_reg}});

	// change to result.exp <<= index_var(i)
	exp->sub[i] = index_var(n - 1 - i);

	// Add the var to the environment
	result.Env[i] = apply_reg;
    }
    result.exp = exp;
  
    return result;
}

extern "C" closure builtin_function_arraySize(OperationArgs& Args)
{
    int N = Args.evaluate_slot_to_closure(0).exp.size();

    return {N};
}

extern "C" closure builtin_function_getIndex(OperationArgs& Args)
{
    extern long total_index_op;
    total_index_op++;

    int n = Args.evaluate(1).as_int();
    // Do this second, so that evaluation of the 1st argument can't call expand_memory afterwards.
    const closure& C = Args.evaluate_slot_to_closure(0);

    if (not is_constructor(C.exp.head()) or C.exp.head().as_<constructor>().f_name != "Array")
	throw myexception()<<"Trying to index expression that is not an Array:   "<<C.exp;

    int N = C.exp.size();
    assert(C.Env.size() == C.exp.size());

    if (n < 0 or n >= N)
	throw myexception()<<"Trying to access index "<<n<<" in array of size "<<N<<".";

    // Return a reference to the heap variable pointed to by the nth entry
    return {index_var(0), {C.Env[n]} };
}

extern "C" closure builtin_function_removeElement(OperationArgs& Args)
{
    int idx = Args.evaluate(0).as_int();

    // Do this second, so that evaluation of the 1st argument can't call expand_memory afterwards.
    const closure& C = Args.evaluate_slot_to_closure(1);

    if (not is_constructor(C.exp.head()) or C.exp.head().as_<constructor>().f_name != "Array")
	throw myexception()<<"Trying to remove element from expression that is not an Array:   "<<C.exp;

    int n = C.exp.size();
    if (idx < 0 or idx >= n)
        throw myexception()<<"Trying to remove element '"<<idx<<"' from an Array of size "<<n<<"!";

    object_ptr<expression> exp = new expression(constructor("Array",n-1));
    exp->sub.resize(n-1);

    closure result;
    result.Env.resize(n-1);
    for(int i=0,j=0; i<n-1; i++,j++)
    {
        if (i==idx) j++;

	// change to result.exp <<= index_var(i)
	exp->sub[i] = index_var(n - 2 - i);

	// Add the var to the environment
	result.Env[i] = C.Env[j];
    }
    result.exp = exp;

    return result;
}

