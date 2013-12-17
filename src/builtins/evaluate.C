#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_evaluate(OperationArgs& Args)
{
#ifndef NDEBUG
  if (Args.evaluate_changeables())
    throw myexception()<<"Calling builtin_function_evaluate( ) when evaluate_changeables=true";
#endif

  int token = *Args.evaluate_as<Int>(0);

  int R1 = Args.evaluate_slot_to_reg(1,true);

  int R2 = Args.memory().result_for_reg(token,R1);

  assert( R2 );

  return {index_var(0),{R2}};
}
