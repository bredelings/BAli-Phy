#ifndef HASKELL_CONSTRAINTS
#define HASKELL_CONSTRAINTS

#include <vector>
#include <utility>

#include "haskell/coretype.H"
#include "haskell/core.H"
#include "context.H"

struct OccurrenceOrigin
{
    Located<std::string> name;
    OccurrenceOrigin(const std::string& s);
    OccurrenceOrigin(const Located<std::string>& s);
};

struct GivenOrigin
{
};

struct AppOrigin
{
};

struct InstanceOrigin
{
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
};

struct StringOrigin
{
    Type t1;
    Type t2;
    std::string msg;
};

struct UnifyOrigin
{
    Type t1;
    Type t2;
};


typedef std::variant<std::monostate, OccurrenceOrigin, GivenOrigin, AppOrigin, InstanceOrigin, PatOrigin, SuperclassOrigin, TypeConvertOrigin,CycleBreakerOrigin,UnifyOrigin,StringOrigin> ConstraintOrigin;

enum ConstraintFlavor { Wanted, Given };

struct Constraint
{
    ConstraintOrigin origin;
    ConstraintFlavor flavor;
    Core::Var ev_var;
    Type pred;
    int level;

    std::string print() const;

    Constraint(const ConstraintOrigin&, ConstraintFlavor, Core::Var, Type, int);
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
    std::shared_ptr<Core::Decls> evidence_binds;

    std::vector<TypeVar> tvs;

    LIE givens;

    WantedConstraints wanteds;

    TypeCheckerContext context;

    std::string print() const;

    Implication(int l, const std::vector<TypeVar>& tvs, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core::Decls>& eb, const TypeCheckerContext& c);
};

template <typename T>
std::vector<T> dict_vars_from_lie(const LIE& constraints)
{
    std::vector<T> dvars;
    for(auto& constraint: constraints)
        if (is_dictionary_pred(constraint.pred))
            dvars.push_back(constraint.ev_var);
    return dvars;
}

std::vector<Type> preds_from_lie(const LIE& lie);

std::vector<Core::Var> dict_vars_from_lie(const LIE& lie);

std::string print(const LIE& lie);

#endif
