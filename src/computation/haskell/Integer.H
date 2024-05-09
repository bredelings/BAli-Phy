#ifndef HASKELL_INTEGER_H
#define HASKELL_INTEGER_H

#include <boost/multiprecision/cpp_int.hpp>
#include <boost/rational.hpp>
#include "computation/object.H"

typedef boost::multiprecision::cpp_int integer;
typedef Box<integer> Integer;

typedef boost::rational<integer> rational;


template<>
inline std::string Box<integer>::print() const
{
    return str();
}

#endif
