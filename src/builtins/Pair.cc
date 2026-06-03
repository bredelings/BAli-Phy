#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include <algorithm>

using std::vector;

extern "C" R::Exp simple_function_c_fst(vector<R::Exp>& args)
{
    return get_arg(args).as_<EPair>().first;
}

extern "C" R::Exp simple_function_c_snd(vector<R::Exp>& args)
{
    return get_arg(args).as_<EPair>().second;
}

extern "C" R::Exp simple_function_c_pair(vector<R::Exp>& args)
{
    auto fst = get_arg(args);
    auto snd = get_arg(args);

    return EPair(R::to_expression_ref(fst), R::to_expression_ref(snd));
}

extern "C" R::Exp simple_function_c_nil(vector<R::Exp>& /*args*/)
{
    expression_ref nil;
    nil = 42;
    return nil;
}
