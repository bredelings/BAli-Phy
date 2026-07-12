#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"
#include "probability/choose.H"
#include <cstdlib> // for getenv

using namespace std;

extern "C" closure builtin_function_getArgsRaw(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    R::RVector V;
    for(const auto& arg: M.args)
        V.push_back(arg);

    return V;
}

extern "C" closure builtin_function_getEnvRaw(OperationArgs& Args)
{
    auto x = Args.evaluate_slot_to_value(0).as_string();

    return std::string(getenv(x.c_str()));
}
