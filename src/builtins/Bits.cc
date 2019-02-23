#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "dp/2way.H"
#include "imodel/imodel.H"
#include "computation/expression/bool.H"
#include "computation/expression/constructor.H"
#include <boost/dynamic_bitset.hpp>
#include "alignment/alignment.H"

#include <boost/dynamic_bitset.hpp>

typedef Box<boost::dynamic_bitset<>> bitvector;

extern "C" closure builtin_function_empty_bitvector(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    return { bitvector(n) };
}

extern "C" closure builtin_function_empty_bitset(OperationArgs& Args)
{
    int n = Args.evaluate(0).as_int();

    return { bitvector(n) };
}
