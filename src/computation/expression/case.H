#ifndef EXPRESSION_CASE_H
#define EXPRESSION_CASE_H

#include <vector>
#include "expression_ref.H"

expression_ref make_alts(const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& object, const expression_ref& alts);

expression_ref make_if_expression(const expression_ref& condition, const expression_ref& true_branch, const expression_ref& false_branch);

void parse_alts(const expression_ref& alts, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

bool parse_case_expression(const expression_ref& E, expression_ref& T, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

bool is_case(const expression_ref& E);

#endif
