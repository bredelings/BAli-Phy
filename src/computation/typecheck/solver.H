#ifndef SOLVER_H
#define SOLVER_H

#include "computation/haskell/haskell.H"
#include "typecheck.H"

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

struct ReactSuccess {};

typedef std::variant<ReactFail, ReactSuccess> Reaction;

struct Changed
{
    Predicate P;
};

struct Unchanged
{
};

struct Solved
{
};

struct NonCanon
{
};

typedef std::variant<Changed, Unchanged, Solved, NonCanon> Change;

struct Solver: public TypeChecker
{
    Core::Decls decls;

    std::vector<Predicate> work_list;
    std::vector<Predicate> inerts;
    std::vector<Predicate> failed;

    std::optional<Predicate> canonicalize_equality(Core::Var& co_var, ConstraintFlavor flavor, Hs::Type t1, Hs::Type t2);
    bool canonicalize(Predicate& P);

    Change interact(const Predicate& P1, const Predicate& P2);

    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);
    Solver(const TypeChecker& tc);
};

#endif
