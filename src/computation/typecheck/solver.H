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

std::vector<Predicate> make_predicates(ConstraintFlavor, const std::vector<Constraint>&);

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

    std::vector<std::tuple<MetaTypeVar,Type>> cycle_breakers;

    std::string print() const;
};

constexpr int occurs_definitely_bit = 0;
constexpr int occurs_maybe_bit = 1;
constexpr int impredicative_bit = 2;
constexpr int type_family_bit = 3;
constexpr std::bitset<8> ok_result;
constexpr std::bitset<8> occurs_definitely_result(1);
constexpr std::bitset<8> occurs_maybe_result(2);
constexpr std::bitset<8> impredicative_result(4);
constexpr std::bitset<8> type_family_result(8);

struct Solver: public TypeChecker
{
    Core::Decls decls;

    std::vector<Predicate> work_list;

    InertSet inerts;

    std::bitset<8> check_type_equality(const Type& lhs, const Type& rhs) const;

    void add_inert(const Predicate& P);
    void add_to_work_list(const std::vector<Predicate>& ps);

    bool can_rewrite(const Predicate& P1, const Predicate& P2) const;

    bool is_touchable(const MetaTypeVar&, const Type&) const;

    void kickout_rewritten(const Predicate& p, std::vector<Predicate>& ps);
    void kickout_after_unification(const MetaTypeVar&);

    bool contains_type(const Type&, const Type&) const;
    bool contains_type(const std::vector<Type>&, const Type&) const;

    bool is_rewritable_lhs(Type t) const;
    Type rewrite(ConstraintFlavor, Type) const;
    std::vector<Type> rewrite(ConstraintFlavor, std::vector<Type>) const;
    Type rewrite_mtv(ConstraintFlavor flavor, const MetaTypeVar&) const;
    Type rewrite_tv(ConstraintFlavor flavor, const TypeVar&) const;
    Type rewrite_constrained_type(ConstraintFlavor flavor, const ConstrainedType&) const;
    Type rewrite_forall(ConstraintFlavor flavor, const ForallType&) const;
    Type rewrite_type_con_app(ConstraintFlavor flavor, const TypeCon&, const std::vector<Type>&) const;
    Type rewrite_app(ConstraintFlavor flavor, const Type&, const Type&) const;
    
    std::optional<Type> maybe_break_type_equality_cycle(ConstraintFlavor, const CanonicalEqualityPred&, std::bitset<8>);

    Type break_type_equality_cycle(ConstraintFlavor, const Type&);
    void unbreak_type_equality_cycles();

    std::optional<Predicate>
    canonicalize_equality_var_tyfam(ConstraintFlavor flavor, CanonicalEqualityPred P);

    std::optional<Predicate>
    canonicalize_equality_lhs2(ConstraintFlavor flavor, CanonicalEqualityPred P);

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

    Change interact(const Predicate& P1, const Predicate& P2);

    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);
    Solver(const TypeChecker& tc);
};

#endif
