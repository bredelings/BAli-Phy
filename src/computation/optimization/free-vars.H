#ifndef FREE_VARS_H
#define FREE_VARS_H

#include <vector>
#include "object.H"       // for Box
#include "immer/set.hpp"  // for immer::set
#include "computation/expression/var.H"
#include "computation/core/ast.H"

typedef Box<immer::set<var>> FreeVarSet;

template<typename T>
struct annot_expression_ref: public Object
{
    annot_expression_ref<T>* clone() const {return new annot_expression_ref<T>(*this);}

    T note;
    expression_ref exp;

    std::string print() const {return exp.print();}

    annot_expression_ref() = default;
    annot_expression_ref(const T& t, const expression_ref& e):note(t),exp(e) {}
    annot_expression_ref(const T&& t, const expression_ref& e):note(std::move(t)),exp(e) {}
};

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
    using WildcardPat = Core2::WildcardPat;
    using ConPat = Core2::ConPat<>;
    using Pattern = Core2::Pattern<>;
    using Alt = Core2::Alt<std::monostate, FreeVars>;
    using Alts = Core2::Alts<std::monostate, FreeVars>;
    using ConApp = Core2::ConApp<>;
    using BuiltinOp = Core2::BuiltinOp<>;
    using Constant = Core2::Constant;
    using Exp = Core2::Exp<std::monostate, FreeVars>;
};


template <>
std::string annot_expression_ref<FreeVarSet>::print() const;

annot_expression_ref<FreeVarSet>
add_free_variable_annotations(const expression_ref& E);

FV::Exp add_free_variable_annotations(const Core2::Exp<>& E);

FreeVarSet get_union(const FreeVarSet& s1, const FreeVarSet& s2);

FreeVars get_union(const FreeVars& s1, const FreeVars& s2);

[[nodiscard]] FreeVarSet erase(const FreeVarSet& s, const std::vector<var>& xs);

std::vector<var> get_vars(const expression_ref& pattern);

const FreeVarSet& get_free_vars(const annot_expression_ref<FreeVarSet>& e);

const FreeVarSet& get_free_vars(const expression_ref& E);

const FreeVars& get_free_vars(const FV::Exp& E);
FreeVars& get_free_vars(FV::Exp& E);
#endif
