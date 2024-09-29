#ifndef SIMPLIFIER_SUBSTITUTION_H
#define SIMPLIFIER_SUBSTITUTION_H

#include <map>
#include "computation/expression/expression_ref.H"
#include "computation/expression/convert.H"

namespace simplifier
{

struct substitution_range;

struct substitution: public std::map<Occ::Var, substitution_range>
{
    using map::map;
};

struct substitution_range
{
    Occ::Exp E;
    const substitution* S = nullptr;
    substitution_range(const Occ::Exp& e):E(e) {}
    substitution_range(const Occ::Exp& e, const substitution& s):E(e),S(&s) {}
};
}

#endif /* SIMPLIFIER_H */
