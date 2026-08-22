#ifndef UTIL_UTF8_H
#define UTIL_UTF8_H

#include <cstddef>
#include <optional>
#include <string>
#include <string_view>

namespace utf8
{

struct decoded_char
{
    char32_t code_point;
    std::size_t next_byte;
};

bool is_scalar_value(char32_t code_point);

std::optional<decoded_char> decode_next(std::string_view bytes, std::size_t byte_offset);

std::string encode(char32_t code_point);

std::optional<std::size_t> count_code_points(std::string_view bytes);

std::optional<std::size_t> byte_offset_for_code_point_offset(std::string_view bytes, std::size_t code_point_offset);

}

#endif
