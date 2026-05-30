#ifndef RUNTIME_TRIM_H
#define RUNTIME_TRIM_H

#include "computation/runtime/ast.H"

namespace Runtime
{
    ExpPtr trim_normalize(const ExpPtr&);
}

#endif
