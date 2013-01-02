#include "computation/computation.H"
#include "myexception.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  std::string message = *Args.evaluate_as<String>(0);
  
  throw myexception()<<message;
}
