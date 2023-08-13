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
    auto s1 = Args.evaluate(0);
    int offset1 = Args.evaluate(1).as_int();
    int length1 = Args.evaluate(2).as_int();

    auto s2 = Args.evaluate(3);
    int offset2 = Args.evaluate(4).as_int();
    int length2 = Args.evaluate(5).as_int();

    auto result = new String;
    String& s3 = *result;

    s3.reserve(length1+length2);

    s3 = s1.as_<String>().substr(offset1,length1);
    s3 += s2.as_<String>().substr(offset2,length2);

    return result;
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
    auto s1 = Args.evaluate(0);
    int offset1 = Args.evaluate(1).as_int();
    int length1 = Args.evaluate(2).as_int();

    auto s2 = Args.evaluate(3);
    int offset2 = Args.evaluate(4).as_int();
    int length2 = Args.evaluate(5).as_int();

    std::string_view S1 = s1.as_<String>();
    std::string_view S2 = s2.as_<String>();

    return {S1.substr(offset1,length1) == S2.substr(offset2,length2)};
}

extern "C" closure builtin_function_less_than(OperationArgs& Args)
{
    auto s1 = Args.evaluate(0);
    int offset1 = Args.evaluate(1).as_int();
    int length1 = Args.evaluate(2).as_int();

    auto s2 = Args.evaluate(3);
    int offset2 = Args.evaluate(4).as_int();
    int length2 = Args.evaluate(5).as_int();

    std::string_view S1 = s1.as_<String>();
    std::string_view S2 = s2.as_<String>();

    return {S1.substr(offset1,length1) < S2.substr(offset2,length2)};
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

// defined in tree/tree.cc
std::string escape_for_newick(const std::string& s);

extern "C" closure builtin_function_quoteLabelRaw(OperationArgs& Args)
{
    String label = Args.evaluate(0).as_<String>();
    label = escape_for_newick(label);
    return label;
}
