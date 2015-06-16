#include "computation/computation.H"
#include "myexception.H"
#include "computation/graph_register.H"
#include "rng.H"
#include "util.H"
#include "probability/choose.H"

using boost::dynamic_pointer_cast;
using namespace std;

extern "C" closure builtin_function_getArgs(OperationArgs& Args)
{
  assert(not Args.evaluate_changeables());

  reg_heap& M = Args.memory();

  EVector V;
  for(const auto& arg: M.args)
    V.push_back(String(arg));

  return V;
}
