#ifndef HASKELL_CONSTRAINTS
#define HASKELL_CONSTRAINTS

#include <vector>
#include <utility>

#include "haskell/coretype.H"
#include "haskell/core.H"

struct Constraint
{
    Core::Var ev_var;
    Type pred;

    std::string print() const;
};

typedef std::vector<Constraint> LIE;

LIE& operator+=(LIE& lie1, const LIE& lie2);

LIE dictionary_constraints(const LIE& lie);

LIE equality_constraints(const LIE& lie);

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

    std::string print() const;

    Implication(int l, const std::vector<TypeVar>& tvs, const LIE& g, const WantedConstraints& w, const std::shared_ptr<Core::Decls>& eb);
};

template <typename T>
std::vector<T> vars_from_lie(const LIE& lie)
{
    std::vector<T> dvars;
    for(auto& [dvar, _]: lie)
        dvars.push_back(dvar);
    return dvars;
}

std::vector<Type> constraints_from_lie(const LIE& lie);

std::vector<Core::Var> vars_from_lie(const LIE& lie);

std::string print(const LIE& lie);

#endif
