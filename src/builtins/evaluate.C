#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_evaluate(OperationArgs& Args)
{
  auto& M = Args.memory();

  int c = *Args.evaluate_as<Int>(0);

#ifndef NDEBUG
  if (Args.evaluate_changeables() and c >= 0)
    throw myexception()<<"Calling builtin_function_evaluate( ) when evaluate_changeables=true and c >= 0";
#endif

  int R1 = Args.reg_for_slot(1);

  int R2 = 0;

  if (c < 0)
    R2 = M.incremental_evaluate(R1, 0);
  else
    R2 = M.incremental_evaluate_in_context(R1, c);

  assert( R2 );

  return {index_var(0),{R2}};
}
