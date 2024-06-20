#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/haskell/core2.H"
#include "computation/loader.H"

Core2::Exp<> to_core_exp(const expression_ref& E);

Core2::Decls<> to_core(const CDecls& decls);

var to_var(const Core2::Var<>& V);

expression_ref to_expression_ref(const Core2::Exp<>& E);

CDecls to_expression_ref(const Core2::Decls<>& decls);

Core2::Exp<> load_builtins(const module_loader& L, const Core2::Exp<>& E);

Core2::Decls<> load_builtins(const module_loader& L, Core2::Decls<> E);
