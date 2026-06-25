#include <cctype>
#include <optional>
#include <string_view>

#pragma clang diagnostic ignored "-Wreturn-type-c-linkage"
#include "computation/machine/args.H"
#include "util/utf8.H"

using std::vector;

struct decoded_text_char
{
    char32_t code_point;
    std::size_t begin_byte;
    std::size_t next_byte;
};

// Decode the first UTF-8 scalar in Text.  Text storage is std::string bytes,
// but Char-facing APIs should expose Unicode scalar values.
static decoded_text_char decode_first_text_char(std::string_view text, const char* function_name)
{
    auto decoded = utf8::decode_next(text, 0);
    if (not decoded)
        throw myexception()<<function_name<<": invalid UTF-8 Text.";
    return {decoded->code_point, 0, decoded->next_byte};
}

// Find the final UTF-8 scalar in Text by validating from the front.  This
// avoids treating continuation bytes as standalone characters.
static decoded_text_char decode_last_text_char(std::string_view text, const char* function_name)
{
    std::optional<decoded_text_char> last;
    for(std::size_t byte_offset = 0; byte_offset < text.size(); )
    {
        auto decoded = utf8::decode_next(text, byte_offset);
        if (not decoded)
            throw myexception()<<function_name<<": invalid UTF-8 Text.";
        last = decoded_text_char{decoded->code_point, byte_offset, decoded->next_byte};
        byte_offset = decoded->next_byte;
    }

    if (not last)
        throw myexception()<<function_name<<": empty Text!";
    return *last;
}

extern "C" R::Exp simple_function_pack(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& etext = arg.as_<R::RVector>();

    std::string s;
    for(int i=0;i<etext.size();i++)
        s += utf8::encode(etext[i].as_char());
    return s;
}

extern "C" R::Exp simple_function_singleton(vector<R::Exp>& args)
{
    auto c = get_arg(args).as_char();

    return utf8::encode(c);
}

extern "C" R::Exp simple_function_empty(vector<R::Exp>&)
{
    std::string s;

    return s;
}

extern "C" R::Exp simple_function_cons(vector<R::Exp>& args)
{
    auto c = get_arg(args).as_char();

    std::string s  = get_arg(args).as_string();

    return utf8::encode(c) + s;
}

extern "C" R::Exp simple_function_snoc(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    auto c = get_arg(args).as_char();

    return s + utf8::encode(c);
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

    // FIXME-UNICODE: Text slices still use byte offsets and byte lengths.
    // Convert this with the rest of the Data.Text offset API.
    s3 = s1.as_string().substr(offset1,length1);
    s3 += s2.as_string().substr(offset2,length2);

    return s3;
}

extern "C" R::Exp simple_function_head(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.head: empty Text!";

    return decode_first_text_char(s, "Text.head").code_point;
}

extern "C" R::Exp simple_function_last(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.last: empty Text!";

    return decode_last_text_char(s, "Text.last").code_point;
}

extern "C" R::Exp simple_function_init(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.init: empty Text!";

    auto last = decode_last_text_char(s, "Text.init");
    s.resize(last.begin_byte);

    return s;
}

extern "C" R::Exp simple_function_tail(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    if (s.empty())
        throw myexception()<<"Text.tail: empty Text!";

    auto first = decode_first_text_char(s, "Text.tail");
    s = s.substr(first.next_byte);

    return s;
}

extern "C" R::Exp simple_function_length(vector<R::Exp>& args)
{
    std::string s  = get_arg(args).as_string();

    // FIXME-UNICODE: This currently reports bytes, not Unicode scalar values.
    // Convert together with Text slicing and comparison offsets.
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

    // FIXME-UNICODE: Text comparison offsets and lengths are still byte-based.
    // Convert this with the rest of the Data.Text offset API.
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

    // FIXME-UNICODE: Text comparison offsets and lengths are still byte-based.
    // Convert this with the rest of the Data.Text offset API.
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
