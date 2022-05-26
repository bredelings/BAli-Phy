#ifndef TYPES_H
#define TYPES_H

#include <utility>
#include <vector>

#include "haskell/haskell.H"

Hs::Type make_arrow_type(const Hs::Type& t1, const Hs::Type& t2);

std::pair<Hs::Type,std::vector<Hs::Type>> decompose_type_apps(Hs::Type t);

std::optional<std::pair<Hs::Type,Hs::Type>> is_function_type(const Hs::Type& t);

std::set<Hs::TypeVar> free_type_variables(const Hs::Context& context);
std::set<Hs::TypeVar> free_type_variables(const Hs::Type& type);

std::set<std::string> free_type_vars(const Hs::Type& type);
std::set<std::string> free_type_vars(const Hs::Context& context);

Hs::Type remove_top_gen(Hs::Type type);
#endif
