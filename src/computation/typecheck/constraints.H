#ifndef HASKELL_CONSTRAINTS
#define HASKELL_CONSTRAINTS

#include <vector>
#include <utility>

#include "haskell/type.H"
#include "haskell/core.H"

typedef std::vector<std::pair<Core::Var, Hs::Type>> LIE;

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
#endif
