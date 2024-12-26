#ifndef SIMPLIFIER_SUBSTITUTION_H
#define SIMPLIFIER_SUBSTITUTION_H

#include <map>
#include <optional>
#include "computation/optimization/occurrence.H"
#include "immer/map.hpp"

namespace simplifier
{

struct substitution_range;

typedef immer::map<Occ::Var, substitution_range> substitution;

struct substitution_range
{
    Occ::Exp E;
    std::optional<substitution> S;
    substitution_range(const Occ::Exp& e):E(e) {}
    substitution_range(const Occ::Exp& e, const substitution& s):E(e),S(s) {}
};
}

#endif /* SIMPLIFIER_H */
