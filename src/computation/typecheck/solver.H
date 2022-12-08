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
    Hs::Type t1;
    Hs::Type t2;
    CanonicalEqualityPred flip() const;
    std::string print() const;
    CanonicalEqualityPred(const Core::Var& v, const Hs::Type& ta, const Hs::Type& tb):co(v),t1(ta),t2(tb) {}
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

struct InertSet
{
    std::optional<int> given_eq_level;

    std::vector<Predicate> tv_eqs;
    std::vector<Predicate> mtv_eqs;
    std::vector<Predicate> tyfam_eqs;
    std::vector<Predicate> dicts;
    std::vector<Predicate> irreducible;
    std::vector<Predicate> failed;
};


struct Solver: public TypeChecker
{
    Core::Decls decls;

    std::vector<Predicate> work_list;

    InertSet inerts;

    void add_inert(const Predicate& P);
    void add_to_work_list(const std::vector<Predicate>& ps);

    bool can_rewrite(const Predicate& P1, const Predicate& P2) const;

    bool is_touchable(const Hs::MetaTypeVar&, const Hs::Type&) const;

    void kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps);
    void kickout_after_unification(const Hs::MetaTypeVar&);

    std::optional<Predicate> canonicalize_equality(ConstraintFlavor flavor, CanonicalEqualityPred P);
    std::optional<Predicate> canonicalize_dict(ConstraintFlavor flavor, CanonicalDictPred P);
    std::optional<Predicate> canonicalize(Predicate& P);

    Hs::Type rewrite(ConstraintFlavor, Hs::Type) const;
    Change interact(const Predicate& P1, const Predicate& P2);

    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);
    Solver(const TypeChecker& tc);
};

#endif
