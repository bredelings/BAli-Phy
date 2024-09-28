#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/haskell/core2.H"
#include "computation/loader.H"
#include "computation/optimization/occurrence_info.H"

Core2::Exp<> to_core_exp(const expression_ref& E);

Core2::Decls<> to_core(const CDecls& decls);

var to_var(const Core2::Var<>& V);

expression_ref to_expression_ref(const Core2::Exp<>& E);

expression_ref maybe_to_expression_ref(const std::optional<Core2::Exp<>>& E);

CDecls to_expression_ref(const Core2::Decls<>& decls);

//-----------------------------------------------------------------------

namespace Occ
{
    using Var = Core2::Var<occurrence_info>;
    using Lambda = Core2::Lambda<occurrence_info>;
    using Apply = Core2::Apply<occurrence_info>;
    using Let = Core2::Let<occurrence_info>;
    using Decl = Core2::Decl<occurrence_info>;
    using Decls = Core2::Decls<occurrence_info>;
    using Case = Core2::Case<occurrence_info>;
    using WildcardPat = Core2::WildcardPat;
    using ConPat = Core2::ConPat<occurrence_info>;
    using VarPat = Core2::VarPat<occurrence_info>;
    using VarOrWildcardPattern = Core2::VarOrWildcardPattern<occurrence_info>;
    using Pattern = Core2::Pattern<occurrence_info>;
    using Alt = Core2::Alt<occurrence_info>;
    using Alts = Core2::Alts<occurrence_info>;
    using ConApp = Core2::ConApp<occurrence_info>;
    using BuiltinOp = Core2::BuiltinOp<occurrence_info>;
    using Constant = Core2::Constant;
    using Exp = Core2::Exp<occurrence_info>;
};

Occ::Var to_occ_var(const var& V);

Occ::Exp to_occ_exp(const expression_ref& E);

Occ::Decls to_occ(const CDecls& decls);

Occ::Decl to_occ(const CDecl& decl);

var occ_to_var(const Occ::Var& V);

expression_ref occ_to_expression_ref(const Occ::Exp& E);

expression_ref maybe_occ_to_expression_ref(const std::optional<Occ::Exp>& E);

CDecls occ_to_cdecls(const Occ::Decls& decls);

//-----------------------------------------------------------------------

Core2::Exp<> load_builtins(const module_loader& L, const Core2::Exp<>& E);

Core2::Decls<> load_builtins(const module_loader& L, Core2::Decls<> E);


