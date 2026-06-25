#include "util/utf8.H"
#include "util/myexception.H"
#include <cstdint>

namespace utf8
{

bool is_scalar_value(char32_t code_point)
{
    return code_point <= 0x10FFFF and not (0xD800 <= code_point and code_point <= 0xDFFF);
}

static bool is_continuation_byte(unsigned char byte)
{
    return (byte & 0xC0) == 0x80;
}

// Convert an unsigned UTF-8 output byte to the storage type used by
// std::string.  This is byte emission, not runtime Char narrowing.
static char output_byte(std::uint32_t byte)
{
    return static_cast<char>(static_cast<unsigned char>(byte));
}

// Decode one UTF-8 scalar starting at a byte offset.  Invalid UTF-8,
// overlong encodings, surrogates, and out-of-range values return nullopt.
std::optional<decoded_char> decode_next(std::string_view bytes, std::size_t byte_offset)
{
    if (byte_offset >= bytes.size())
        return {};

    auto byte0 = static_cast<unsigned char>(bytes[byte_offset]);
    if (byte0 <= 0x7F)
        return decoded_char{static_cast<char32_t>(byte0), byte_offset + 1};

    int length = 0;
    char32_t code_point = 0;
    char32_t minimum_value = 0;
    if (0xC2 <= byte0 and byte0 <= 0xDF)
    {
        length = 2;
        code_point = byte0 & 0x1F;
        minimum_value = 0x80;
    }
    else if (0xE0 <= byte0 and byte0 <= 0xEF)
    {
        length = 3;
        code_point = byte0 & 0x0F;
        minimum_value = 0x800;
    }
    else if (0xF0 <= byte0 and byte0 <= 0xF4)
    {
        length = 4;
        code_point = byte0 & 0x07;
        minimum_value = 0x10000;
    }
    else
        return {};

    if (bytes.size() - byte_offset < static_cast<std::size_t>(length))
        return {};

    for(int i=1; i<length; i++)
    {
        auto byte = static_cast<unsigned char>(bytes[byte_offset + i]);
        if (not is_continuation_byte(byte))
            return {};
        code_point = (code_point << 6) | (byte & 0x3F);
    }

    if (code_point < minimum_value or not is_scalar_value(code_point))
        return {};

    return decoded_char{code_point, byte_offset + static_cast<std::size_t>(length)};
}

// Encode one Unicode scalar as UTF-8.  Invalid scalar values are rejected
// before byte emission so callers do not accidentally serialize bad Char data.
std::string encode(char32_t code_point)
{
    if (not is_scalar_value(code_point))
        throw myexception()<<"Invalid Unicode scalar value: "<<static_cast<std::uint32_t>(code_point);

    std::string bytes;
    if (code_point <= 0x7F)
        bytes.push_back(output_byte(code_point));
    else if (code_point <= 0x7FF)
    {
        bytes.push_back(output_byte(0xC0 | (code_point >> 6)));
        bytes.push_back(output_byte(0x80 | (code_point & 0x3F)));
    }
    else if (code_point <= 0xFFFF)
    {
        bytes.push_back(output_byte(0xE0 | (code_point >> 12)));
        bytes.push_back(output_byte(0x80 | ((code_point >> 6) & 0x3F)));
        bytes.push_back(output_byte(0x80 | (code_point & 0x3F)));
    }
    else
    {
        bytes.push_back(output_byte(0xF0 | (code_point >> 18)));
        bytes.push_back(output_byte(0x80 | ((code_point >> 12) & 0x3F)));
        bytes.push_back(output_byte(0x80 | ((code_point >> 6) & 0x3F)));
        bytes.push_back(output_byte(0x80 | (code_point & 0x3F)));
    }
    return bytes;
}

// Count Unicode code points in a UTF-8 string.  Invalid UTF-8 returns nullopt
// so callers can choose the right diagnostic for their boundary.
std::optional<std::size_t> count_code_points(std::string_view bytes)
{
    std::size_t count = 0;
    for(std::size_t byte_offset = 0; byte_offset < bytes.size(); count++)
    {
        auto decoded = decode_next(bytes, byte_offset);
        if (not decoded)
            return {};
        byte_offset = decoded->next_byte;
    }
    return count;
}

// Translate a public code-point offset into the internal UTF-8 byte offset.
// Invalid UTF-8 or offsets past the end of the string return nullopt.
std::optional<std::size_t> byte_offset_for_code_point_offset(std::string_view bytes, std::size_t code_point_offset)
{
    std::size_t byte_offset = 0;
    for(std::size_t count = 0; count < code_point_offset; count++)
    {
        if (byte_offset == bytes.size())
            return {};
        auto decoded = decode_next(bytes, byte_offset);
        if (not decoded)
            return {};
        byte_offset = decoded->next_byte;
    }
    return byte_offset;
}

}
