#include "computation/computation.H"

extern "C" closure builtin_function_mkArray(OperationArgs& Args)
{
  int n = *Args.evaluate_as<Int>(0);
  expression_ref f = Args.reference(1);

  const closure& C = Args.current_closure();

  // We can't do negative-sized arrays
  assert(n >= 0);
  // The function should be represented as a heap variable...
  object_ptr<const index_var> V = is_a<index_var>(f);
  int f_reg = C.lookup_in_env(V->index);
  
  object_ptr<expression> exp = new expression(constructor("Array",n));
  exp->sub.resize(n);

  expression_ref apply_E;
  {
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    apply_E = (fE, argE);
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
