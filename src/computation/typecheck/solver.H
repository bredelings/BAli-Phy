#ifndef SOLVER_H
#define SOLVER_H

#include "computation/haskell/haskell.H"

#include <vector>
#include <variant>

enum ConstraintFlavor { Wanted, Given };

struct NonCanonicalPred
{
    Core::Var dvar;
    Hs::Type constraint;
    std::string print() const;
    NonCanonicalPred(const Core::Var& v, const Hs::Type& c):dvar(v), constraint(c) {}
};

struct CanonicalDictPred
{
    Core::Var dvar;
    Hs::TypeCon klass;
    std::vector<Hs::Type> args;
    std::string print() const;
};

struct CanonicalEqualityPred
{
    Core::Var co;
    Hs::Type a;
    Hs::Type b;
    std::string print() const;
    CanonicalEqualityPred(const Core::Var& v, const Hs::Type& t1, const Hs::Type& t2):co(v),a(t1),b(t2) {}
};

struct Pred: public std::variant<NonCanonicalPred, CanonicalDictPred, CanonicalEqualityPred>
{
    std::string print() const;

    using variant::variant;
};

struct Predicate
{
    ConstraintFlavor flavor;
    Pred pred;

    std::string print() const;
    Predicate(ConstraintFlavor f, Pred p): flavor(f), pred(p) {}
};

std::vector<Predicate> make_predicates(ConstraintFlavor, const std::vector<std::pair<Core::Var, Hs::Type>>&);

struct ReactFail {};

struct ReactSuccess
{
    Core::Decls decls;
    std::vector<Predicate> predicates;
    ReactSuccess(const Core::Decls& d, const std::vector<Predicate>& ps):decls(d), predicates(ps) {}
};

typedef std::variant<ReactFail, ReactSuccess> Reaction;

#endif
