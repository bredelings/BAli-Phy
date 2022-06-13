#ifndef TYPES_H
#define TYPES_H

#include <utility>
#include <vector>

#include "haskell/haskell.H"

std::set<Hs::TypeVar> free_type_variables(const Hs::Context& context);
std::set<Hs::TypeVar> free_type_variables(const Hs::Type& type);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Context& context);
std::set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Type& type);

std::set<std::string> free_type_vars(const Hs::Type& type);
std::set<std::string> free_type_vars(const Hs::Context& context);
#endif
