#ifndef HASKELL_CONSTRAINTS
#define HASKELL_CONSTRAINTS

#include <vector>
#include <utility>

#include "computation/haskell/coretype.H"
#include "computation/haskell/haskell.H" // for Hs::LExp
#include "computation/core/ast.H" // for Core2::Decls
#include "util/cow-ptr.H"

struct OccurrenceOrigin
{
    std::string name;
    OccurrenceOrigin(const std::string& s);
};

struct GivenOrigin
{
};

struct AppOrigin
{
    expression_ref app;
};

struct InstanceOrigin
{
};

struct LeftSectionOrigin
{
    Hs::LExp op;
};

struct PatOrigin
{
};

struct SuperclassOrigin
{
};

struct TypeConvertOrigin
{
};

struct CycleBreakerOrigin
{
    // Mention original origin
};

struct UnifyOrigin
{
    Type t1;
    Type t2;
};


typedef std::variant<std::monostate, OccurrenceOrigin, GivenOrigin, AppOrigin, InstanceOrigin, LeftSectionOrigin, PatOrigin, SuperclassOrigin, TypeConvertOrigin,CycleBreakerOrigin,UnifyOrigin> ConstraintOrigin;

enum ConstraintFlavor { Wanted, Given };

struct TypeCheckerContext;

struct Constraint
{
    ConstraintOrigin origin;
    cow_ptr<TypeCheckerContext> tc_state;

    ConstraintFlavor flavor;
    Core2::Var<> ev_var;
    Type pred;

    int level() const;
    std::string print() const;

    Constraint(const ConstraintOrigin&, ConstraintFlavor, Core2::Var<>, Type, const cow_ptr<TypeCheckerContext>& s);
};

typedef std::vector<Constraint> LIE;

LIE& operator+=(LIE& lie1, const LIE& lie2);

LIE dictionary_constraints(const LIE& lie);

LIE equality_constraints(const LIE& lie);

bool contains_equality_constraints(const LIE& constraints);

template <typename T>
std::vector<T>& operator+=(std::vector<T>& v1, const std::vector<T>& v2)
{
    v1.insert(v1.end(), v2.begin(), v2.end());
    return v1;
}

template <typename T>
std::vector<T> operator+(const std::vector<T>& v1, const std::vector<T>& v2)
{
    auto v3 = v1;
    v3 += v2;
    return v3;
}

struct Implication;

struct WantedConstraints
{
    LIE simple;
    std::vector<std::shared_ptr<Implication>> implications;

    WantedConstraints() = default;
    WantedConstraints(const LIE& l);

    WantedConstraints& operator+=(const WantedConstraints& wc2);
    WantedConstraints operator+(const WantedConstraints& wc2) const;

    bool empty() const;

    std::string print() const;

    LIE all_simple() const;
};

struct Implication
{
    // The givens  can have type vars with level <  l.
    // The wanteds can have type vars with level <= l.
    int level;

    // Pointer to a set of evidence bindings.
    // We can append new bindings to this object.
    std::shared_ptr<Core2::Decls<>> evidence_binds;

    std::vector<TypeVar> tvs;

    LIE givens;

    WantedConstraints wanteds;

    cow_ptr<TypeCheckerContext> tc_state;

    std::string print() const;

    Implication(int l, const std::vector<TypeVar>& tvs, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core2::Decls<>>& eb, const cow_ptr<TypeCheckerContext>& c);
};

std::vector<Type> preds_from_lie(const LIE& lie);

std::vector<Core2::Var<>> dict_vars_from_lie(const LIE& lie);

std::string print(const LIE& lie);

std::set<TypeVar> free_type_variables(const LIE& env);

std::set<MetaTypeVar> free_meta_type_variables(const LIE& env);

std::set<TypeVar> free_type_variables(const WantedConstraints& wanteds);

std::set<MetaTypeVar> free_meta_type_variables(const WantedConstraints& wanteds);
#endif
