#include <cctype>

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"

using std::vector;

extern "C" expression_ref simple_function_pack(vector<expression_ref>& args)
{
    auto etext = get_arg(args).as_<EVector>();

    String s;
    s.resize(etext.size());
    for(int i=0;i<etext.size();i++)
        s[i] = etext[i].as_char();
    return s;
}

extern "C" expression_ref simple_function_singleton(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    String s;
    s.push_back(c);

    return s;
}

extern "C" expression_ref simple_function_empty(vector<expression_ref>& args)
{
    String s;

    return s;
}

extern "C" expression_ref simple_function_cons(vector<expression_ref>& args)
{
    char c = get_arg(args).as_char();

    String s  = get_arg(args).as_<String>();

    return String(c + s);
}

extern "C" expression_ref simple_function_snoc(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    char c = get_arg(args).as_char();

    return String(s + c);
}

extern "C" expression_ref simple_function_append(vector<expression_ref>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    auto result = new String;
    String& s3 = *result;

    s3.reserve(length1+length2);

    s3 = s1.as_<String>().substr(offset1,length1);
    s3 += s2.as_<String>().substr(offset2,length2);

    return result;
}

extern "C" expression_ref simple_function_head(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.head: empty Text!";

    char c = s[0];

    return c;
}

extern "C" expression_ref simple_function_last(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.last: empty Text!";

    char c = s.back();

    return c;
}

extern "C" expression_ref simple_function_init(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.init: empty Text!";

    s.pop_back();

    return s;
}

extern "C" expression_ref simple_function_tail(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    if (s.empty())
        throw myexception()<<"Text.tail: empty Text!";

    s = s.substr(1);

    return s;
}

extern "C" expression_ref simple_function_length(vector<expression_ref>& args)
{
    String s  = get_arg(args).as_<String>();

    int length = s.size();

    return length;
}

extern "C" expression_ref simple_function_equals(vector<expression_ref>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string_view S1 = s1.as_<String>();
    std::string_view S2 = s2.as_<String>();

    return S1.substr(offset1,length1) == S2.substr(offset2,length2);
}

extern "C" expression_ref simple_function_less_than(vector<expression_ref>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string_view S1 = s1.as_<String>();
    std::string_view S2 = s2.as_<String>();

    return S1.substr(offset1,length1) < S2.substr(offset2,length2);
}

extern "C" expression_ref simple_function_concatRaw(vector<expression_ref>& args)
{
    auto arg0 = get_arg(args);
    auto& strings = arg0.as_<EVector>();

    String result;
    for(auto& string: strings)
        result += string.as_<String>();

    return result;
}

// defined in tree/tree.cc
std::string escape_for_newick(const std::string& s);

extern "C" expression_ref simple_function_quoteLabelRaw(vector<expression_ref>& args)
{
    String label = get_arg(args).as_<String>();
    label = escape_for_newick(label);
    return label;
}
