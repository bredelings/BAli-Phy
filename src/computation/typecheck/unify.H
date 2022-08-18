#ifndef UNIFY_H
#define UNIFY_H

#include <optional>
#include "computation/haskell/haskell.H"
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

    template <>
    class hash < Hs::MetaTypeVar >{
    public :
        size_t operator()(const Hs::MetaTypeVar &x) const
        {
            return std::hash<std::string>()(unloc(x.name));
        }
    };
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t);
Hs::Context apply_subst(const substitution_t& s, Hs::Context C);
Hs::Constructor apply_subst(const substitution_t& s, Hs::Constructor con);
Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d);

bool try_insert(const Hs::MetaTypeVar& tv, Hs::Type type);
bool occurs_check(const Haskell::MetaTypeVar& tv, const Hs::Type& t);

bool maybe_tunify(const Hs::Type& t1, const Hs::Type& t2);
bool maybe_tmatch(const Hs::Type& t1, const Hs::Type& t2);

bool same_type(const Hs::Type& t1, const Hs::Type& t2);

Hs::Type canonicalize_type(const Hs::TupleType& type1);
Hs::Type canonicalize_type(const Hs::ListType& type1);

#endif 
