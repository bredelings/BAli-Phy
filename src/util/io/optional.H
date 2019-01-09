#ifndef UTIL_OPTIONAL_IO_H
#define UTIL_OPTIONAL_IO_H

#include <iostream>
#include <optional>

template <typename T>
std::ostream& operator<<(std::ostream& o, const std::optional<T>& v)
{
    if (v)
       return o<<*v;
    else
       return o<<"--";
}

#endif
