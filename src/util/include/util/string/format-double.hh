#ifndef UTIL_STRING_FORMAT_DOUBLE_HH
#define UTIL_STRING_FORMAT_DOUBLE_HH

#include <array>
#include <charconv>
#include <string>
#include <system_error>

// Use the shortest locale-independent spelling that reads back to the same finite double.
inline std::string format_double(double value)
{
    std::array<char, 64> buffer;
    auto [end, error] = std::to_chars(buffer.data(), buffer.data() + buffer.size(), value);
    if (error != std::errc())
        throw std::system_error(std::make_error_code(error));
    return {buffer.data(), end};
}

#endif
