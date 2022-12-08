#ifndef TYPES_H
#define TYPES_H

#include <utility>
#include <vector>

#include "haskell/haskell.H"

std::set<Hs::TypeVar> free_type_variables(const Hs::Context& context);
std::set<Hs::TypeVar> free_type_variables(const Hs::Type& type);

std::set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Context& context);
std::set<Hs::MetaTypeVar> free_meta_type_variables(const Hs::Type& type);

bool affected_by_mtv(const std::vector<Hs::Type>& context, const Hs::MetaTypeVar& mtv);
bool affected_by_mtv(const Hs::Type& type, const Hs::MetaTypeVar& mtv);

bool contains_tv(const std::vector<Hs::Type>& types, const Hs::TypeVar& tv);
bool contains_tv(const Hs::Type& type, const Hs::TypeVar& tv);

bool contains_mtv(const std::vector<Hs::Type>& types, const Hs::MetaTypeVar& tv);
bool contains_mtv(const Hs::Type& type, const Hs::MetaTypeVar& tv);

std::set<std::string> free_type_vars(const Hs::Type& type);
std::set<std::string> free_type_vars(const Hs::Context& context);
#endif
