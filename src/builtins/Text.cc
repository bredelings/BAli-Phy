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

extern "C" closure builtin_function_singleton(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    String s;
    s.push_back(c);

    return {s};
}

extern "C" closure builtin_function_empty(OperationArgs& Args)
{
    Args.evaluate(0);

    String s;

    return {s};
}

extern "C" closure builtin_function_cons(OperationArgs& Args)
{
    char c = Args.evaluate(0).as_char();

    String s  = Args.evaluate(1).as_<String>();

    s = c + s;

    return {s};
}

extern "C" closure builtin_function_snoc(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    char c = Args.evaluate(1).as_char();

    s += c;

    return {s};
}

extern "C" closure builtin_function_append(OperationArgs& Args)
{
    String s1  = Args.evaluate(0).as_<String>();

    String s2  = Args.evaluate(1).as_<String>();

    String s3 = s1 + s2;

    return {s3};
}

extern "C" closure builtin_function_head(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.head: empty Text!";

    char c = s[0];

    return {c};
}

extern "C" closure builtin_function_last(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.last: empty Text!";

    char c = s.back();

    return {c};
}

extern "C" closure builtin_function_init(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.init: empty Text!";

    s.pop_back();

    return {s};
}

extern "C" closure builtin_function_tail(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.tail: empty Text!";

    s = s.substr(1);

    return {s};
}

extern "C" closure builtin_function_length(OperationArgs& Args)
{
    String s  = Args.evaluate(0).as_<String>();

    int length = s.size();

    return {length};
}

extern "C" closure builtin_function_equals(OperationArgs& Args)
{
    String s1  = Args.evaluate(0).as_<String>();
    String s2  = Args.evaluate(1).as_<String>();

    return {s1 == s2};
}

extern "C" closure builtin_function_less_than(OperationArgs& Args)
{
    String s1  = Args.evaluate(0).as_<String>();
    String s2  = Args.evaluate(1).as_<String>();

    for(int i=0;i<std::min(s1.size(),s2.size());i++)
    {
        if (s1[i] < s2[i])
            return {true};
        else if (s1[i] > s2[i])
            return {false};
    }

    return {s1.size() < s2.size()};
}

extern "C" closure builtin_function_concatRaw(OperationArgs& Args)
{
    auto arg0 = Args.evaluate(0);
    auto& strings = arg0.as_<EVector>();

    String result;
    for(auto& string: strings)
        result += string.as_<String>();

    return result;
}

