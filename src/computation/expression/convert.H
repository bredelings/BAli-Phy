#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/loader.H"
#include "computation/optimization/occurrence_info.H"
#include "computation/optimization/set-levels.H"

var to_var(const Core2::Var<>& V);

expression_ref to_expression_ref(const Core2::Exp<>& E);

expression_ref maybe_to_expression_ref(const std::optional<Core2::Exp<>>& E);

CDecl to_expression_ref(const Core2::Decl<>& decl);

CDecls to_expression_ref(const Core2::Decls<>& decls);

std::vector<CDecls> decl_groups_to_expression_ref(const std::vector<Core2::Decls<>>& decls);

//-----------------------------------------------------------------------

expression_ref occ_to_expression_ref(const Occ::Exp& E);

expression_ref maybe_occ_to_expression_ref(const std::optional<Occ::Exp>& E);

//-----------------------------------------------------------------------

Core2::Decls<> to_core(const Occ::Decls& decls);

Core2::Exp<> to_core_exp(const Occ::Exp& E);

//-----------------------------------------------------------------------

Core2::Exp<> load_builtins(const module_loader& L, const Core2::Exp<>& E);

Core2::Decls<> load_builtins(const module_loader& L, Core2::Decls<> E);


