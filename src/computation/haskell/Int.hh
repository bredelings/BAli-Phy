#ifndef HASKELL_INT_H
#define HASKELL_INT_H

#include <cstdint>
#include <utility>
#include "util/myexception.hh"

using haskell_int = std::int64_t;

// Tree nodes, registers and several native array interfaces use C++ int indices.
inline int checked_cpp_int(haskell_int value)
{
    if (not std::in_range<int>(value))
        throw myexception()<<"Haskell Int "<<value<<" exceeds the supported C++ int range";
    return static_cast<int>(value);
}

#endif
