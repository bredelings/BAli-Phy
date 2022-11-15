#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "computation/expression/index_var.H"
#include "computation/expression/constructor.H"
#include "computation/expression/bool.H"

extern "C" closure builtin_function_mkArray(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    const closure& C = Args.current_closure();

    // We can't do negative-sized arrays
    assert(n >= 0);
    // The function should be represented as a heap variable...
    int f_reg = C.reg_for_slot(1);
  
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
	// Look at the right splot in the environment
	exp->sub[i] = index_var(n - 1 - i);

	// Allocate a reg for i
	int i_reg = Args.allocate(expression_ref(i));

	// Add the var to the environment
	result.Env[i] = Args.allocate({apply_E,{f_reg, i_reg}});
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

extern "C" closure builtin_function_arrayMap(OperationArgs& Args)
{
    // 1. Get the location of the function
    int f_reg = Args.reg_for_slot(0);

    // 2. Get a copy of the input array
    auto result = Args.evaluate_slot_to_closure(1);
    int n = result.Env.size();

    // 3. Create the expression part of an "apply <1> <0>" closure.
    //    Indexing is from the end, which is why <1> comes before <1>.
    expression_ref apply_E;
    {
	expression_ref fE = index_var(1);
	expression_ref argE = index_var(0);
	apply_E = {fE, argE};
    }
    expression_ref apply_1_0 = {index_var(1),index_var(0)};

    // 4. Replace each x with (f x)
    for(int i=0;i<n;i++)
    {
        // The current entry should already be an index_var pointing to the right slot.
	int x_reg = result.Env[i];

	// Allocate a reg for (f x)
	int apply_reg = Args.allocate({apply_E, {f_reg, x_reg}});

	// Add the reg to the environment
	result.Env[i] = apply_reg;
    }

    return result;
}

