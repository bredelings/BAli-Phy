#include <cctype>

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"

using std::vector;

extern "C" R::Exp simple_function_pack(vector<R::Exp>& args)
{
    auto etext = get_arg(args).as_<R::RVector>();

    std::string s;
    s.resize(etext.size());
    for(int i=0;i<etext.size();i++)
        s[i] = etext[i].as_char();
    return s;
}

extern "C" R::Exp simple_function_singleton(vector<R::Exp>& args)
{
    char c = get_arg(args).as_char();

    std::string s;
    s.push_back(c);

    return s;
}

extern "C" R::Exp simple_function_empty(vector<R::Exp>&)
{
    std::string s;

    return s;
}

extern "C" R::Exp simple_function_cons(vector<R::Exp>& args)
{
    char c = get_arg(args).as_char();

    std::string s  = get_arg(args).as_string();

    return (c + s);
}

extern "C" R::Exp simple_function_snoc(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    char c = get_arg(args).as_char();

    return (s + c);
}

extern "C" R::Exp simple_function_append(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string s3;

    s3.reserve(length1+length2);

    s3 = s1.as_string().substr(offset1,length1);
    s3 += s2.as_string().substr(offset2,length2);

    return s3;
}

extern "C" R::Exp simple_function_head(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.head: empty Text!";

    char c = s[0];

    return c;
}

extern "C" R::Exp simple_function_last(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.last: empty Text!";

    char c = s.back();

    return c;
}

extern "C" R::Exp simple_function_init(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.init: empty Text!";

    s.pop_back();

    return s;
}

extern "C" R::Exp simple_function_tail(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.tail: empty Text!";

    s = s.substr(1);

    return s;
}

extern "C" R::Exp simple_function_length(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    int length = s.size();

    return length;
}

extern "C" R::Exp simple_function_equals(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string_view S1 = s1.as_string();
    std::string_view S2 = s2.as_string();

    return S1.substr(offset1,length1) == S2.substr(offset2,length2);
}

extern "C" R::Exp simple_function_less_than(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string_view S1 = s1.as_string();
    std::string_view S2 = s2.as_string();

    return S1.substr(offset1,length1) < S2.substr(offset2,length2);
}

extern "C" R::Exp simple_function_concatRaw(vector<R::Exp>& args)
{
    auto arg0 = get_arg(args);
    auto& strings = arg0.as_<R::RVector>();

    std::string result;
    for(auto& string: strings)
        result += string.as_string();

    return result;
}

// defined in tree/tree.cc
std::string escape_for_newick(const std::string& s);

extern "C" R::Exp simple_function_quoteLabelRaw(vector<R::Exp>& args)
{
    std::string label = get_arg(args).as_string();
    label = escape_for_newick(label);
    return label;
}
