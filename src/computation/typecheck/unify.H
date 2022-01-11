#ifndef UNIFY_H
#define UNIFY_H

#include <optional>
#include "computation/parser/haskell.H"
#include "immer/map.hpp" // for immer::map


typedef immer::map<Hs::TypeVar, Hs::Type> substitution_t;

std::string print(const substitution_t& s);
// TODO
// 1. Merge unification / substitution code for types & kinds?

namespace std
{
    template <>
    class hash < Hs::Var >{
    public :
        size_t operator()(const Hs::Var &x) const
        {
            return std::hash<std::string>()(unloc(x.name));
        }
    };

    template <>
    class hash < Hs::TypeVar >{
    public :
        size_t operator()(const Hs::TypeVar &x) const
        {
            return std::hash<std::string>()(unloc(x.name));
        }
    };
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t);
Hs::Context apply_subst(const substitution_t& s, Hs::Context C);
Hs::Constructor apply_subst(const substitution_t& s, Hs::Constructor con);
Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d);

substitution_t compose(substitution_t s2, substitution_t s1);
std::optional<substitution_t> combine(substitution_t s1, substitution_t s2);
substitution_t unify(const Hs::Type& t1, const Hs::Type& t2);

std::optional<substitution_t> match(const Hs::Type& t1, const Hs::Type& t2);
bool same_type(const Hs::Type& t1, const Hs::Type& t2);

#endif 
