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

struct text_slice
{
    std::string_view bytes;
    std::size_t begin_byte;
    std::size_t end_byte;
};

static bool is_utf8_continuation_byte(unsigned char byte)
{
    return (byte & 0xC0) == 0x80;
}

static bool is_utf8_boundary(std::string_view bytes, std::size_t byte_offset)
{
    return byte_offset == bytes.size() or not is_utf8_continuation_byte(static_cast<unsigned char>(bytes[byte_offset]));
}

// Validate a Data.Text byte slice boundary.  The full UTF-8 payload is
// validated by constructors and by operations that decode the slice.
static text_slice checked_text_slice(const R::Exp& arg, int offset, int length, const char* function_name)
{
    if (offset < 0 or length < 0)
        throw myexception()<<function_name<<": negative Text offset or length.";

    std::string_view bytes = arg.as_string();
    auto begin = static_cast<std::size_t>(offset);
    auto byte_length = static_cast<std::size_t>(length);
    if (begin > bytes.size() or byte_length > bytes.size() - begin)
        throw myexception()<<function_name<<": Text slice is out of bounds.";

    auto end = begin + byte_length;
    if (not is_utf8_boundary(bytes, begin) or not is_utf8_boundary(bytes, end))
        throw myexception()<<function_name<<": Text slice does not start and end on UTF-8 boundaries.";

    return {bytes, begin, end};
}

// Validate a full CPPString as UTF-8 before it becomes Text storage.  This
// keeps CPPString itself semantic-free while enforcing the Text invariant.
static void validate_text_storage(std::string_view bytes, const char* function_name)
{
    std::size_t byte_offset = 0;
    while(byte_offset < bytes.size())
    {
        auto decoded = utf8::decode_next(bytes, byte_offset);
        if (not decoded)
            throw myexception()<<function_name<<": invalid UTF-8 Text.";
        byte_offset = decoded->next_byte;
    }
}

// Decode the first UTF-8 scalar in a valid Text slice.  Text storage is
// std::string bytes, but Char-facing APIs expose Unicode scalar values.
static decoded_text_char decode_first_text_char(text_slice slice, const char* function_name)
{
    if (slice.begin_byte == slice.end_byte)
        throw myexception()<<function_name<<": empty Text!";

    auto decoded = utf8::decode_next(slice.bytes, slice.begin_byte);
    if (not decoded or decoded->next_byte > slice.end_byte)
        throw myexception()<<function_name<<": invalid UTF-8 Text.";
    return {decoded->code_point, slice.begin_byte, decoded->next_byte};
}

// Find the final UTF-8 scalar in a valid Text slice by validating from the
// front.  This avoids treating continuation bytes as standalone characters.
static decoded_text_char decode_last_text_char(text_slice slice, const char* function_name)
{
    std::optional<decoded_text_char> last;
    for(std::size_t byte_offset = slice.begin_byte; byte_offset < slice.end_byte; )
    {
        auto decoded = utf8::decode_next(slice.bytes, byte_offset);
        if (not decoded or decoded->next_byte > slice.end_byte)
            throw myexception()<<function_name<<": invalid UTF-8 Text.";
        last = decoded_text_char{decoded->code_point, byte_offset, decoded->next_byte};
        byte_offset = decoded->next_byte;
    }

    if (not last)
        throw myexception()<<function_name<<": empty Text!";
    return *last;
}

// Validate a generic CPPString before treating it as Text storage.  CPPString
// itself remains an opaque transport type with caller-defined semantics.
extern "C" R::Exp simple_function_validate(vector<R::Exp>& args)
{
    auto arg = get_arg(args);
    const auto& bytes = arg.as_string();
    validate_text_storage(bytes, "Text.validate");
    return bytes;
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

extern "C" R::Exp simple_function_append(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    std::string s3;

    auto slice1 = checked_text_slice(s1, offset1, length1, "Text.append");
    auto slice2 = checked_text_slice(s2, offset2, length2, "Text.append");

    s3.reserve(length1 + length2);

    s3 = slice1.bytes.substr(slice1.begin_byte, slice1.end_byte - slice1.begin_byte);
    s3 += slice2.bytes.substr(slice2.begin_byte, slice2.end_byte - slice2.begin_byte);

    return s3;
}

extern "C" R::Exp simple_function_head(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.head");
    return decode_first_text_char(slice, "Text.head").code_point;
}

extern "C" R::Exp simple_function_last(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.last");
    return decode_last_text_char(slice, "Text.last").code_point;
}

// Count Unicode scalar values in a Text byte slice.  Public Text length uses
// code points even though the internal slice length is stored in bytes.
extern "C" R::Exp simple_function_codePointLength(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.codePointLength");
    int n = 0;
    for(std::size_t byte_offset = slice.begin_byte; byte_offset < slice.end_byte; n++)
    {
        auto decoded = utf8::decode_next(slice.bytes, byte_offset);
        if (not decoded or decoded->next_byte > slice.end_byte)
            throw myexception()<<"Text.codePointLength: invalid UTF-8 Text.";
        byte_offset = decoded->next_byte;
    }

    return n;
}

// Decode a Text byte slice into runtime Char values.  This keeps Text.unpack
// independent from the generic CPPString byte-unpacking bridge.
extern "C" R::Exp simple_function_unpack(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.unpack");
    R::RVector chars;
    for(std::size_t byte_offset = slice.begin_byte; byte_offset < slice.end_byte; )
    {
        auto decoded = utf8::decode_next(slice.bytes, byte_offset);
        if (not decoded or decoded->next_byte > slice.end_byte)
            throw myexception()<<"Text.unpack: invalid UTF-8 Text.";
        chars.push_back(decoded->code_point);
        byte_offset = decoded->next_byte;
    }

    return chars;
}

// Return the absolute byte offset immediately after the first scalar in a Text
// slice.  Data.Text.tail uses this to construct a new byte slice.
extern "C" R::Exp simple_function_tailOffset(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.tailOffset");
    auto first = decode_first_text_char(slice, "Text.tailOffset");
    return static_cast<int>(first.next_byte);
}

// Return the byte length excluding the final scalar in a Text slice.  The
// caller keeps the original byte offset and replaces only the length.
extern "C" R::Exp simple_function_initLength(vector<R::Exp>& args)
{
    auto s = get_arg(args);
    int offset = get_arg(args).as_int();
    int length = get_arg(args).as_int();

    auto slice = checked_text_slice(s, offset, length, "Text.initLength");
    auto last = decode_last_text_char(slice, "Text.initLength");
    return static_cast<int>(last.begin_byte - slice.begin_byte);
}

extern "C" R::Exp simple_function_equals(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    auto slice1 = checked_text_slice(s1, offset1, length1, "Text.equals");
    auto slice2 = checked_text_slice(s2, offset2, length2, "Text.equals");

    return slice1.bytes.substr(slice1.begin_byte, slice1.end_byte - slice1.begin_byte) ==
           slice2.bytes.substr(slice2.begin_byte, slice2.end_byte - slice2.begin_byte);
}

extern "C" R::Exp simple_function_less_than(vector<R::Exp>& args)
{
    auto s1 = get_arg(args);
    int offset1 = get_arg(args).as_int();
    int length1 = get_arg(args).as_int();

    auto s2 = get_arg(args);
    int offset2 = get_arg(args).as_int();
    int length2 = get_arg(args).as_int();

    auto slice1 = checked_text_slice(s1, offset1, length1, "Text.less_than");
    auto slice2 = checked_text_slice(s2, offset2, length2, "Text.less_than");

    return slice1.bytes.substr(slice1.begin_byte, slice1.end_byte - slice1.begin_byte) <
           slice2.bytes.substr(slice2.begin_byte, slice2.end_byte - slice2.begin_byte);
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
