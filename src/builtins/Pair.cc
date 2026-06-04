#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include <algorithm>

using std::vector;

R::Exp pair_first(const R::Exp& pair)
{
    return R::rpair_first(pair);
}

R::Exp pair_second(const R::Exp& pair)
{
    return R::rpair_second(pair);
}

extern "C" R::Exp simple_function_c_fst(vector<R::Exp>& args)
{
    return pair_first(get_arg(args));
}

extern "C" R::Exp simple_function_c_snd(vector<R::Exp>& args)
{
    return pair_second(get_arg(args));
}

extern "C" R::Exp simple_function_c_pair(vector<R::Exp>& args)
{
    auto fst = get_arg(args);
    auto snd = get_arg(args);

    return R::RPair(std::move(fst), std::move(snd));
}

extern "C" R::Exp simple_function_c_nil(vector<R::Exp>& /*args*/)
{
    return 42;
}
