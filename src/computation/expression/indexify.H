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

expression_ref indexify(const expression_ref&);

Runtime::Exp runtime_indexify(const Core2::Exp<>&);

expression_ref deindexify(const expression_ref&, const std::vector<expression_ref>&);
expression_ref deindexify(const expression_ref&);


#endif
