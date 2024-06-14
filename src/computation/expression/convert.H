#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/haskell/core2.H"

Core2::Exp<> to_core_exp(const expression_ref& E);

Core2::Decls<> to_core(const CDecls& decls);

expression_ref to_expression_ref(const Core2::Exp<>& E);

CDecls to_expression_ref(const Core2::Decls<>& decls);
