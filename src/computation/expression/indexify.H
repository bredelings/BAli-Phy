#ifndef INDEXIFY_H
#define INDEXIFY_H

#include <memory>
#include <vector>
#include "expression_ref.H"
#include "computation/core/ast.H"

namespace Runtime
{
    struct Exp;
    using ExpPtr = std::shared_ptr<const Exp>;
}

expression_ref indexify(const expression_ref&);

Runtime::ExpPtr runtime_indexify(const expression_ref&);

Runtime::ExpPtr runtime_indexify(const Core2::Exp<>&);

expression_ref deindexify(const expression_ref&, const std::vector<expression_ref>&);
expression_ref deindexify(const expression_ref&);


#endif
