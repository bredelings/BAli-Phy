#ifndef EXPRESSION_CORE_H
#define EXPRESSION_CORE_H

#include <string>
#include "expression/expression_ref.H"
#include "expression/var.H"

namespace Core
{
    var error();
    var unpack_cpp_string();
    var unsafePerformIO();

    expression_ref error(const std::string&);
    expression_ref unpack_cpp_string(const std::string& s);
    expression_ref unsafePerformIO(const expression_ref&);
}

#endif
