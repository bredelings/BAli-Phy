#ifndef UNIFY_H
#define UNIFY_H

#include "computation/parser/haskell.H"
#include "immer/map.hpp" // for immer::map


typedef immer::map<Hs::TypeVar, Hs::Type> substitution_t;

// TODO
// 1. Merge unification / substitution code for types & kinds?

namespace std
{
    template <>
    class hash < Hs::Var >{
    public :
        size_t operator()(const Hs::Var &x) const
        {
            size_t h = std::hash<std::string>()(unloc(x.name));
            if (x.index)
                h = std::hash<int>()(*x.index) ^ h;
            return  h ;
        }
    };

    template <>
    class hash < Hs::TypeVar >{
    public :
        size_t operator()(const Hs::TypeVar &x) const
        {
            size_t h = std::hash<std::string>()(unloc(x.name));
            if (x.index)
                h = std::hash<int>()(*x.index) ^ h;
            return  h ;
        }
    };
}

Hs::Type apply_subst(const substitution_t& s, const Hs::Type& t);
Hs::Context apply_subst(const substitution_t& s, Hs::Context C);
Hs::Constructor apply_subst(const substitution_t& s, Hs::Constructor con);
Hs::DataOrNewtypeDecl apply_subst(const substitution_t& s, Hs::DataOrNewtypeDecl d);

substitution_t compose(substitution_t s2, substitution_t s1);
substitution_t unify(const Hs::Type& t1, const Hs::Type& t2);


#endif 
