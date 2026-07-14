#ifndef SET_LEVELS_H
#define SET_LEVELS_H

#include "computation/core/ast.H"

class FreshVarState;

namespace Levels
{
    using Var = Core::Var<int>;
    using Lambda = Core::Lambda<int>;
    using Apply = Core::Apply<int>;
    using Let = Core::Let<int>;
    using Decl = Core::Decl<int>;
    using Decls = Core::Decls<int>;
    using NonRec = Core::NonRec<int>;
    using Rec = Core::Rec<int>;
    using Bind = Core::Bind<int>;
    using Binds = Core::Binds<int>;
    using Case = Core::Case<int>;
    using Pattern = Core::Pattern<int>;
    using Alt = Core::Alt<int>;
    using ConApp = Core::ConApp<int>;
    using BuiltinOp = Core::BuiltinOp<int>;
    using Constant = Core::Constant;
    using Exp = Core::Exp<int>;
};

Core::Var<> strip_level(const Levels::Var&);
std::vector<Core::Var<>> strip_levels(const std::vector<Levels::Var>& xs);
Core::Pattern<> strip_levels_from_pattern(const Levels::Pattern&);
Levels::Binds set_level_for_module(FreshVarState& fresh_var_state, const Core::Binds<>& binds);

#endif
