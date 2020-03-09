#include <cctype>

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

extern "C" closure builtin_function_isDigit(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    if (std::isdigit(c))
        return {expression_ref(1)};
    else
        return {expression_ref(0)};
}
