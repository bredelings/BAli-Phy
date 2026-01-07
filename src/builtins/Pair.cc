#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include <algorithm>

using std::vector;

extern "C" expression_ref simple_function_c_fst(vector<expression_ref>& args)
{
    return get_arg(args).as_<EPair>().first;
}

extern "C" expression_ref simple_function_c_snd(vector<expression_ref>& args)
{
    return get_arg(args).as_<EPair>().second;
}

extern "C" expression_ref simple_function_c_pair(vector<expression_ref>& args)
{
    auto fst = get_arg(args);
    auto snd = get_arg(args);

    return EPair(fst, snd);
}

extern "C" expression_ref simple_function_c_nil(vector<expression_ref>& /*args*/)
{
    expression_ref nil;
    nil = 42;
    return nil;
}

