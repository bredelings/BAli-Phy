#ifndef UNIFY_H
#define UNIFY_H

#include <optional>
#include "computation/haskell/haskell.H"
#include "immer/map.hpp" // for immer::map


typedef immer::map<Hs::TypeVar, Hs::Type> substitution_t;
typedef immer::map<Hs::MetaTypeVar, Hs::Type> usubstitution_t;

std::string print(const substitution_t& s);
// TODO
// 1. Merge unification / substitution code for types & kinds?

std::optional<Hs::Context> check_apply_subst(const substitution_t& s, Hs::Context C);
std::optional<Hs::Type> check_apply_subst(const substitution_t& s, const Hs::Type& t);
Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t);
std::vector<Hs::Type> apply_subst(const substitution_t& s, const std::vector<Hs::Type>& t);
//Hs::Context apply_subst(const substitution_t& s, const Hs::Context& C);

std::optional<Hs::Context> check_apply_subst(const usubstitution_t& s, Hs::Context C);
std::optional<Hs::Type> check_apply_subst(const usubstitution_t& s, const Hs::Type& t);
Hs::Type apply_subst(const usubstitution_t& s, const Hs::Type& t);

Hs::ConstructorDecl apply_subst(const substitution_t& s, Hs::ConstructorDecl con);
Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d);

#endif 
