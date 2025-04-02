#ifndef SET_LEVELS_H
#define SET_LEVELS_H

#include "computation/expression/expression_ref.H"
#include "computation/expression/let.H"
#include "computation/fresh_vars.H"
#include "computation/core/ast.H"

namespace Levels
{
    using Var = Core2::Var<int>;
    using Lambda = Core2::Lambda<int>;
    using Apply = Core2::Apply<int>;
    using Let = Core2::Let<int>;
    using Decl = Core2::Decl<int>;
    using Decls = Core2::Decls<int>;
    using Case = Core2::Case<int>;
    using WildcardPat = Core2::WildcardPat;
    using ConPat = Core2::ConPat<int>;
    using Pattern = Core2::Pattern<int>;
    using Alt = Core2::Alt<int>;
    using Alts = Core2::Alts<int>;
    using ConApp = Core2::ConApp<int>;
    using BuiltinOp = Core2::BuiltinOp<int>;
    using Constant = Core2::Constant;
    using Exp = Core2::Exp<int>;
};

var strip_level(var x);
Core2::Var<> strip_level(const Levels::Var&);
std::vector<Core2::Var<>> strip_levels(const std::vector<Levels::Var>& xs);
Core2::Pattern<> strip_levels_from_pattern(const Levels::Pattern&);
std::vector<Levels::Decls> set_level_for_module(FreshVarState& fresh_var_state, const std::vector<Core2::Decls<>>& decl_groups);

#endif
