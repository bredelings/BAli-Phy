#ifndef EXPRESSION_CASE_H
#define EXPRESSION_CASE_H

#include <vector>
#include <string>
#include <optional>
#include <tuple>
#include "expression_ref.H"

namespace Expression
{

// This isn't really the form that is run, because its not indexified yet!
// It also isn't the form that is type-checked, because the body is simple, not a MultiGuardedRHS.

// See let.H, which defines both
// * a desugared form (let_exp)
// * an indexified and runnable form (Let, which is an Operation)

struct CaseAlt
{
    std::string print() const;

    expression_ref pattern;
    expression_ref body;

    bool operator==(const CaseAlt& a) const;
};

struct CaseAlts: public Object, public std::vector<CaseAlt>
{
    std::string print() const;
    CaseAlts* clone() const {return new CaseAlts(*this);}

    bool operator==(const Object& o) const;
    bool operator==(const CaseAlts& a) const;

    using std::vector<CaseAlt>::vector;
};

}

Expression::CaseAlts make_alts(const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& object, const Expression::CaseAlts& alts);

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch);

std::optional<std::tuple<expression_ref, Expression::CaseAlts>> parse_case_expression(const expression_ref& E);

bool is_case(const expression_ref& E);

#endif
