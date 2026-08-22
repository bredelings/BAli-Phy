#include "computation/loader.hh"
#include "computation/optimization/occurrence_info.hh"
#include "computation/optimization/set-levels.hh"

//-----------------------------------------------------------------------

Core::Var<> to_core_var(const Occ::Var& x);

Core::Decls<> to_core(const Occ::Decls& decls);
Core::Bind<> to_core(const Occ::Bind& bind);

Core::Exp<> to_core_exp(const Occ::Exp& E);
