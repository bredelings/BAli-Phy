#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/loader.H"
#include "computation/optimization/occurrence_info.H"
#include "computation/optimization/set-levels.H"

var to_var(const Core2::Var<>& V);

expression_ref to_expression_ref(const Core2::Constant& C);

//-----------------------------------------------------------------------

std::optional<Core2::Constant> to_core_constant(const expression_ref& E);

//-----------------------------------------------------------------------

Core2::Var<> to_core_var(const Occ::Var& x);

Core2::Decls<> to_core(const Occ::Decls& decls);

Core2::Exp<> to_core_exp(const Occ::Exp& E);
