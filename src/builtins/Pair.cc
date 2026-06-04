#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/myexception.H"
#include <algorithm>

using std::vector;

R::Exp pair_first(const R::Exp& pair)
{
    if (auto runtime_pair = pair.to<R::RPair>())
        return runtime_pair->first;
    else
        return R::e_op_value(pair.as_<EPair>().first);
}

R::Exp pair_second(const R::Exp& pair)
{
    if (auto runtime_pair = pair.to<R::RPair>())
        return runtime_pair->second;
    else
        return R::e_op_value(pair.as_<EPair>().second);
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

    object_ptr<R::RPair> result(new R::RPair);
    result->first = std::move(fst);
    result->second = std::move(snd);
    return R::ObjectValue(result);
}

extern "C" R::Exp simple_function_c_nil(vector<R::Exp>& /*args*/)
{
    return 42;
}
