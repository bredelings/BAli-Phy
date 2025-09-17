#ifndef FREE_VARS_H
#define FREE_VARS_H

#include <vector>
#include "object.H"       // for Box
#include "immer/set.hpp"  // for immer::set
#include "computation/core/ast.H"

typedef immer::set<Core2::Var<>> FreeVars;

namespace FV
{
    using Var = Core2::Var<>;
    using Lambda = Core2::Lambda<std::monostate, FreeVars>;
    using Apply = Core2::Apply<std::monostate, FreeVars>;
    using Let = Core2::Let<std::monostate, FreeVars>;
    using Decl = Core2::Decl<std::monostate, FreeVars>;
    using Decls = Core2::Decls<std::monostate, FreeVars>;
    using Case = Core2::Case<std::monostate, FreeVars>;
    using Pattern = Core2::Pattern<>;
    using Alt = Core2::Alt<std::monostate, FreeVars>;
    using ConApp = Core2::ConApp<std::monostate, FreeVars>;
    using BuiltinOp = Core2::BuiltinOp<std::monostate, FreeVars>;
    using Constant = Core2::Constant;
    using Exp = Core2::Exp<std::monostate, FreeVars>;
};


FV::Exp add_free_variable_annotations(const Core2::Exp<>& E);

FreeVars get_union(const FreeVars& s1, const FreeVars& s2);

[[nodiscard]] FreeVars erase(const FreeVars& s, const std::vector<FV::Var>& xs);

const FreeVars& get_free_vars(const FV::Exp& E);
FreeVars& get_free_vars(FV::Exp& E);
#endif
