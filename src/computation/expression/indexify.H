#ifndef INDEXIFY_H
#define INDEXIFY_H

#include <memory>
#include <vector>
#include "expression_ref.H"
#include "computation/core/ast.H"

namespace Runtime
{
    struct Exp;
}

Runtime::Exp runtime_indexify(const Core::Exp<>&);

expression_ref deindexify(const expression_ref&, const std::vector<expression_ref>&);
expression_ref deindexify(const expression_ref&);


#endif
