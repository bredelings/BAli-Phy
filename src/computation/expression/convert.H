#include "computation/expression/expression_ref.H"
#include "computation/expression/var.H"
#include "computation/loader.H"
#include "computation/optimization/occurrence_info.H"
#include "computation/optimization/set-levels.H"

//-----------------------------------------------------------------------

Core::Var<> to_core_var(const Occ::Var& x);

Core::Decls<> to_core(const Occ::Decls& decls);

Core::Exp<> to_core_exp(const Occ::Exp& E);
