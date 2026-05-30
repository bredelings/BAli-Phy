#ifndef FREE_VARS_H
#define FREE_VARS_H

#include <vector>
#include "object.H"       // for Box
#include "immer/set.hpp"  // for immer::set
#include "computation/core/ast.H"

typedef immer::set<Core::Var<>> FreeVars;

namespace FV
{
    using Var = Core::Var<>;
    using Lambda = Core::Lambda<std::monostate, FreeVars>;
    using Apply = Core::Apply<std::monostate, FreeVars>;
    using Let = Core::Let<std::monostate, FreeVars>;
    using Decl = Core::Decl<std::monostate, FreeVars>;
    using Decls = Core::Decls<std::monostate, FreeVars>;
    using Case = Core::Case<std::monostate, FreeVars>;
    using Pattern = Core::Pattern<>;
    using Alt = Core::Alt<std::monostate, FreeVars>;
    using ConApp = Core::ConApp<std::monostate, FreeVars>;
    using BuiltinOp = Core::BuiltinOp<std::monostate, FreeVars>;
    using Constant = Core::Constant;
    using Exp = Core::Exp<std::monostate, FreeVars>;
};


FV::Exp add_free_variable_annotations(const Core::Exp<>& E);

FreeVars get_union(const FreeVars& s1, const FreeVars& s2);

[[nodiscard]] FreeVars erase(const FreeVars& s, const std::vector<FV::Var>& xs);

const FreeVars& get_free_vars(const FV::Exp& E);
FreeVars& get_free_vars(FV::Exp& E);
#endif
