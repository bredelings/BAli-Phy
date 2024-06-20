#ifndef INDEXIFY_H
#define INDEXIFY_H

#include <vector>
#include "expression_ref.H"
#include "computation/haskell/core2.H"

expression_ref indexify(const expression_ref&);

expression_ref indexify(const Core2::Exp<>&);

expression_ref deindexify(const expression_ref&, const std::vector<expression_ref>&);
expression_ref deindexify(const expression_ref&);


#endif
