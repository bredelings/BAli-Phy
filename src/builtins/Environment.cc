#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"
#include "probability/choose.H"

using boost::dynamic_pointer_cast;
using namespace std;

extern "C" closure builtin_function_getArgs(OperationArgs& Args)
{
  reg_heap& M = Args.memory();

  EVector V;
  for(const auto& arg: M.args)
    V.push_back(String(arg));

  return V;
}
