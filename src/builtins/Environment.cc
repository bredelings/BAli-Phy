#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include "computation/machine/graph_register.H"
#include "util/rng.H"
#include "probability/choose.H"
#include <cstdlib> // for getenv

using namespace std;

namespace
{
    string environment_string(const Runtime::Exp& E)
    {
        if (E.to<Runtime::String>())
            return E.as_string();
        else
            return E.as_<String>().value();
    }
}

extern "C" closure builtin_function_getArgs(OperationArgs& Args)
{
    reg_heap& M = Args.memory();

    R::RVector V;
    for(const auto& arg: M.args)
        V.push_back(String(arg));

    return V;
}

extern "C" closure builtin_function_getEnvRaw(OperationArgs& Args)
{
    auto x = environment_string(Args.evaluate_slot_to_value(0));

    return String(getenv(x.c_str()));
}
