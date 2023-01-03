#ifndef SOLVER_H
#define SOLVER_H

#include "computation/haskell/haskell.H"
#include "typecheck.H"

#include <vector>
#include <variant>
#include <bitset>

struct NonCanonical
{
    Constraint constraint;

    const ConstraintOrigin& origin() const {return constraint.origin;}
    ConstraintFlavor flavor() const {return constraint.flavor;}
    int level() const {return constraint.level();}
    std::string print() const;
    NonCanonical(const Constraint& c):constraint(c) {}
};

struct CanonicalDict
{
    Constraint constraint;
    TypeCon klass;
    std::vector<Type> args;

    const ConstraintOrigin& origin() const {return constraint.origin;}
    ConstraintFlavor flavor() const {return constraint.flavor;}
    int level() const {return constraint.level();}
    std::string print() const;
};

struct CanonicalEquality
{
    Constraint constraint;
    Type t1;
    Type t2;

    CanonicalEquality flip() const;
    const ConstraintOrigin& origin() const {return constraint.origin;}
    ConstraintFlavor flavor() const {return constraint.flavor;}
    int level() const {return constraint.level();}
    std::string print() const;

    CanonicalEquality(const Constraint& c, const Type& ta, const Type& tb):constraint(c),t1(ta),t2(tb) {}
};

struct Predicate: public std::variant<NonCanonical, CanonicalDict, CanonicalEquality>
{
    std::string print() const;
    const ConstraintOrigin& origin() const;
    const Constraint& constraint() const;
    ConstraintFlavor flavor() const;
    int level() const;
    using variant::variant;
};

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
    Type rewrite_app(ConstraintFlavor, const Type&, const Type&) const;
    
    std::optional<Type> maybe_break_type_equality_cycle(const CanonicalEquality&, std::bitset<8>);

    Type break_type_equality_cycle(const Constraint&r, const Type&);
    void unbreak_type_equality_cycles();

    std::optional<Predicate>
    canonicalize_equality_var_tyfam(CanonicalEquality P);

    std::optional<Predicate>
    canonicalize_equality_lhs2(CanonicalEquality P);

    std::optional<Predicate>
    canonicalize_equality_lhs1(const CanonicalEquality& P);

    std::optional<Predicate>
    canonicalize_equality_lhs(const CanonicalEquality& P);

    std::optional<Predicate>
    canonicalize_equality_type_apps(const Constraint& C, const Type&, const Type&, const Type&, const Type&);
    std::optional<Predicate>
    canonicalize_equality_type_cons(const CanonicalEquality& P,
                                    const TypeCon&, const std::vector<Type>&, const TypeCon&, const std::vector<Type>&);
    std::optional<Predicate> canonicalize_equality(CanonicalEquality P);
    std::optional<Predicate> canonicalize_dict(CanonicalDict P);
    std::optional<Predicate> canonicalize(Predicate& P);

    Change interact(const Predicate& P1, const Predicate& P2);

    std::optional<Reaction> top_react(const Predicate& P);

    Core::Decls simplify(const LIE& givens, LIE& wanteds);
    Solver(const TypeChecker& tc);
};

#endif
