#ifndef TYPES_H
#define TYPES_H

#include <utility>
#include <vector>

#include "computation/haskell/coretype.H"
#include "computation/haskell/type.H"

std::set<TypeVar> free_type_variables(const std::vector<Type>& types);
std::set<TypeVar> free_type_variables(const Type& type);

std::set<MetaTypeVar> free_meta_type_variables(const std::vector<Type>& types);
std::set<MetaTypeVar> free_meta_type_variables(const Type& type);

bool affected_by_mtv(const std::vector<Type>& context, const MetaTypeVar& mtv);
bool affected_by_mtv(const Type& type, const MetaTypeVar& mtv);

bool contains_tv(const std::vector<Type>& types, const TypeVar& tv);
bool contains_tv(const Type& type, const TypeVar& tv);

bool contains_mtv(const std::vector<Type>& types, const MetaTypeVar& tv);
bool contains_mtv(const Type& type, const MetaTypeVar& tv);

std::set<std::string> free_type_vars(const Type& type);
std::set<std::string> free_type_vars(const std::vector<Type>& context);
#endif
