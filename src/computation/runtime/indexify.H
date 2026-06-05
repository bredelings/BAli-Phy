#ifndef INDEXIFY_H
#define INDEXIFY_H

#include <memory>
#include <vector>
#include "computation/core/ast.H"

namespace Runtime
{
    struct Exp;
}

struct closure;

Runtime::Exp indexify(const Core::Exp<>&);

Core::Exp<> deindexify(const Runtime::Exp&, const std::vector<Core::Var<>>&);

Core::Exp<> deindexify(const Runtime::Exp&);

Core::Exp<> deindexify(const closure&);

#endif
