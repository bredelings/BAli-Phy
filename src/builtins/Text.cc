#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"

extern "C" closure builtin_function_pack(OperationArgs& Args)
{
    auto etext = Args.evaluate(0).as_<EVector>();

    String s;
    s.resize(etext.size());
    for(int i=0;i<etext.size();i++)
        s[i] = etext[i].as_char();
    return {s};
}

