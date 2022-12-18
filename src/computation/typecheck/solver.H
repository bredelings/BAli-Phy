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
    Type constraint;
    std::string print() const;
    NonCanonicalPred(const Core::Var& v, const Type& c):dvar(v), constraint(c) {}
};

struct CanonicalDictPred
{
    Core::Var dvar;
    TypeCon klass;
    std::vector<Type> args;
    std::string print() const;
};

struct CanonicalEqualityPred
{
    Core::Var co;
    Type t1;
    Type t2;
    CanonicalEqualityPred flip() const;
    std::string print() const;
    CanonicalEqualityPred(const Core::Var& v, const Type& ta, const Type& tb):co(v),t1(ta),t2(tb) {}
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

std::vector<Predicate> make_predicates(ConstraintFlavor, const std::vector<std::pair<Core::Var, Type>>&);

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

    bool is_touchable(const MetaTypeVar&, const Type&) const;

    void kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps);
    void kickout_after_unification(const MetaTypeVar&);

    bool is_rewritable_lhs(Type t) const;

    std::optional<Predicate>
    canonicalize_equality_lhs2(ConstraintFlavor flavor, const CanonicalEqualityPred& P);

    std::optional<Predicate>
    canonicalize_equality_lhs1(ConstraintFlavor flavor, const CanonicalEqualityPred& P);

    std::optional<Predicate>
    canonicalize_equality_lhs(ConstraintFlavor flavor, const CanonicalEqualityPred& P);

    std::optional<Predicate>
    canonicalize_equality_type_apps(ConstraintFlavor flavor,
                               const Type&, const Type&, const Type&, const Type&);
    std::optional<Predicate>
    canonicalize_equality_type_cons(ConstraintFlavor flavor, const CanonicalEqualityPred& P,
                                    const TypeCon&, const std::vector<Type>&, const TypeCon&, const std::vector<Type>&);
    std::optional<Predicate> canonicalize_equality(ConstraintFlavor flavor, CanonicalEqualityPred P);
    std::optional<Predicate> canonicalize_dict(ConstraintFlavor flavor, CanonicalDictPred P);
    std::optional<Predicate> canonicalize(Predicate& P);

    Type rewrite(ConstraintFlavor, Type) const;
    Change interact(const Predicate& P1, const Predicate& P2);

    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);
    Solver(const TypeChecker& tc);
};

#endif
