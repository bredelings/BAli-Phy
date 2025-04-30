#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include <vector>
#include <string>
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "util/io.H"
#include "util/string/split.H"

using boost::dynamic_pointer_cast;
using std::vector;
using std::string;

typedef Box<std::map<int,expression_ref>> EIntMap;

// coalescentTreePr :: [(Double,Double)] -> Tree -> LogDouble
// rawCoalescent
extern "C" closure builtin_function_rawCoalescentTreePr(OperationArgs& Args)
{
    // population sizes: [(time,size)] -> EVector (EPair Double Double)
    auto arg0 = Args.evaluate(0);
    auto& popSizes = arg0.as_<EVector>();

    // What do we actually need to know here?
    // Can we avoid doing tree walking in this function?
    // For each node we need to know (Internal/Leaf, time)
    auto arg1 = Args.evaluate(1);
    auto& nodeTimes = arg1.as_<EIntMap>();

    std::abort();
}
