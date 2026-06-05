#ifndef INDEXIFY_H
#define INDEXIFY_H

#include <memory>
#include "computation/core/ast.H"

namespace Runtime
{
    struct Exp;
}

Runtime::Exp indexify(const Core::Exp<>&);

#endif
