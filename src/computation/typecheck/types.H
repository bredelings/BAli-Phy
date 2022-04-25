#ifndef TYPES_H
#define TYPES_H

#include <utility>
#include <vector>

#include "parser/haskell.H"

Hs::Type make_arrow_type(const Hs::Type& t1, const Hs::Type& t2);

std::pair<Hs::Type,std::vector<Hs::Type>> decompose_type_apps(Hs::Type t);

std::optional<std::pair<Hs::Type,Hs::Type>> is_function_type(const Hs::Type& t);

#endif
