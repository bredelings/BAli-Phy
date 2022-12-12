#ifndef UNIFY_H
#define UNIFY_H

#include <optional>
#include "computation/haskell/haskell.H"
#include "computation/haskell/coretype.H"
#include "immer/map.hpp" // for immer::map


typedef immer::map<TypeVar, Type> substitution_t;
typedef immer::map<MetaTypeVar, Type> usubstitution_t;

std::string print(const substitution_t& s);
// TODO
// 1. Merge unification / substitution code for types & kinds?

std::optional<Context> check_apply_subst(const substitution_t& s, Context C);
std::optional<Type> check_apply_subst(const substitution_t& s, const Type& t);
Type apply_subst(const substitution_t& s, const Type& t);
std::vector<Type> apply_subst(const substitution_t& s, const std::vector<Type>& t);
//Context apply_subst(const substitution_t& s, const Context& C);

std::optional<Context> check_apply_subst(const usubstitution_t& s, Context C);
std::optional<Type> check_apply_subst(const usubstitution_t& s, const Type& t);
Type apply_subst(const usubstitution_t& s, const Type& t);

#endif 
