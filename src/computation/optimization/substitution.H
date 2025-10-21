#ifndef SIMPLIFIER_SUBSTITUTION_H
#define SIMPLIFIER_SUBSTITUTION_H

#include <map>
#include <optional>
#include "computation/optimization/occurrence.H"
#include "immer/map.hpp"

namespace simplifier
{

struct substitution_range;

using substitution = immer::map<Occ::Var, substitution_range>;

struct substitution_range
{
    Occ::Exp E;
    std::shared_ptr<const substitution> S;
    substitution_range(const Occ::Exp& e):E(e) {}
    substitution_range(const Occ::Exp& e, const substitution& s):E(e),S(std::make_shared<const substitution>(s)) {}
};
}

#endif /* SIMPLIFIER_H */
