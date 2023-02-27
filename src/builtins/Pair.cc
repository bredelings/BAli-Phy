#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include <algorithm>

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_c_fst(OperationArgs& Args)
{
  return Args.evaluate(0).as_<EPair>().first;
}

extern "C" closure builtin_function_c_snd(OperationArgs& Args)
{
  return Args.evaluate(0).as_<EPair>().second;
}

extern "C" closure builtin_function_c_pair(OperationArgs& Args)
{
  auto fst = Args.evaluate(0);
  auto snd = Args.evaluate(1);

  return EPair(fst,snd);
}

extern "C" closure builtin_function_c_nil(OperationArgs& /*Args*/)
{
    expression_ref nil;
    nil = 42;
    return nil;
}

