#ifndef EXPRESSION_CASE_H
#define EXPRESSION_CASE_H

#include <vector>
#include "expression_ref.H"

struct alt
{
    expression_ref pattern;
    expression_ref body;
};

struct case_expression
{
    expression_ref object;
    std::vector<alt> alts;
};

expression_ref make_alts(const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& object, const expression_ref& alts);

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch);

bool parse_case_expression(const expression_ref& E, expression_ref& T, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

std::optional<case_expression> parse_case_expression(const expression_ref& E);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

bool is_case(const expression_ref& E);

#endif
