#ifndef HASKELL_INTEGER_H
#define HASKELL_INTEGER_H

#include <boost/multiprecision/cpp_int.hpp>
#include "computation/object.H"

typedef boost::multiprecision::cpp_int integer;
typedef Box<integer> Integer;

#endif
