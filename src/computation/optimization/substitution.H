#ifndef SIMPLIFIER_SUBSTITUTION_H
#define SIMPLIFIER_SUBSTITUTION_H

#include <map>
#include "computation/expression/expression_ref.H"

namespace simplifier
{

struct substitution_range;
struct substitution: public std::map<var, substitution_range>
{
    using map::map;
};

struct substitution_range
{
    expression_ref E;
    const substitution* S = nullptr;
    substitution_range(const expression_ref& e):E(e) {}
    substitution_range(const expression_ref& e, const substitution& s):E(e),S(&s) {}
};
}

#endif /* SIMPLIFIER_H */
