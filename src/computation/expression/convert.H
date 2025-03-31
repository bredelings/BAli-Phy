#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/expression/case.H"
#include "computation/loader.H"
#include "computation/optimization/occurrence_info.H"
#include "computation/optimization/set-levels.H"

Core2::Exp<> to_core_exp(const expression_ref& E);

Core2::Pattern<> to_core_pattern(const expression_ref& E);

std::vector<Core2::Decls<>> decl_groups_to_core(const std::vector<CDecls>& decls);

Core2::Var<> to_core(const var& x);

Core2::Decls<> to_core(const CDecls& decls);

//-----------------------------------------------------------------------

Levels::Exp to_levels_exp(const expression_ref& E);

Levels::Pattern to_levels_pattern(const expression_ref& E);

std::vector<Levels::Decls> decl_groups_to_levels(const std::vector<CDecls>& decls);

Levels::Var to_levels(const var& x);

Levels::Decls to_levels(const CDecls& decls);

//-----------------------------------------------------------------------
var to_var(const Core2::Var<>& V);

expression_ref to_expression_ref(const Core2::Exp<>& E);

expression_ref maybe_to_expression_ref(const std::optional<Core2::Exp<>>& E);

CDecl to_expression_ref(const Core2::Decl<>& decl);

CDecls to_expression_ref(const Core2::Decls<>& decls);

std::vector<CDecls> decl_groups_to_expression_ref(const std::vector<Core2::Decls<>>& decls);

//-----------------------------------------------------------------------

Occ::Var to_occ_var(const var& V);

Occ::Exp to_occ_exp(const expression_ref& E);

Occ::Pattern to_occ_pattern(const expression_ref& P);

Occ::Alts to_occ_alts(const expression_ref& E);

Occ::Decls to_occ(const CDecls& decls);

Occ::Decl to_occ(const CDecl& decl);

var occ_to_var(const Occ::Var& V);

expression_ref occ_to_expression_ref(const Occ::Exp& E);

Core::Alts occ_to_expression_ref(const Occ::Alts& A);

expression_ref occ_to_expression_ref(const Occ::Pattern& P);

expression_ref maybe_occ_to_expression_ref(const std::optional<Occ::Exp>& E);

CDecl occ_to_cdecl(const Occ::Decl& decl);

CDecls occ_to_cdecls(const Occ::Decls& decls);

//-----------------------------------------------------------------------

Core2::Decls<> to_core(const Occ::Decls& decls);

Core2::Exp<> to_core_exp(const Occ::Exp& E);

//-----------------------------------------------------------------------

Core2::Exp<> load_builtins(const module_loader& L, const Core2::Exp<>& E);

Core2::Decls<> load_builtins(const module_loader& L, Core2::Decls<> E);


