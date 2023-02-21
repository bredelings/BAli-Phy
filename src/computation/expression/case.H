#ifndef EXPRESSION_CASE_H
#define EXPRESSION_CASE_H

#include <vector>
#include <string>
#include "expression_ref.H"

namespace Core
{

// This isn't really the form that is run, because its not indexified yet!
// It also isn't the form that is type-checked, because the body is simple, not a MultiGuardedRHS.

// See let.H, which defines both
// * a desugared form (let_exp)
// * an indexified and runnable form (Let, which is an Operation)

struct Alt
{
    std::string print() const;

    expression_ref pattern;
    expression_ref body;

    bool operator==(const Alt& a) const;
    bool operator!=(const Alt& a) const;
};

struct Alts: public Object, public std::vector<Alt>
{
    std::string print() const;
    Alts* clone() const {return new Alts(*this);}

    bool operator==(const Object& o) const;
    bool operator==(const Alts& a) const;
    bool operator!=(const Alts& a) const;

    using std::vector<Alt>::vector;
};

}

Core::Alts make_alts(const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& object, const Core::Alts& alts);

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch);

void parse_alts(const Core::Alts& alts, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

bool parse_case_expression(const expression_ref& E, expression_ref& T, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

bool parse_case_expression(const expression_ref& E, expression_ref& T, Core::Alts& alts);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

bool is_case(const expression_ref& E);

#endif
