#ifndef RUNTIME_TRIM_H
#define RUNTIME_TRIM_H

#include "computation/runtime/ast.H"

namespace Runtime
{
    Exp trim_normalize(const Exp&);
}

#endif
