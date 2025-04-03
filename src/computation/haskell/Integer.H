#ifndef HASKELL_INTEGER_H
#define HASKELL_INTEGER_H

#include "computation/haskell/integer_container.H"
#include "computation/object.H"

typedef Box<integer> Integer;

template<>
inline std::string Box<integer>::print() const
{
    return str();
}
#endif
