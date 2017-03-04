#ifndef EXPRESSION_CASE_H
#define EXPRESSION_CASE_H

#include <vector>
#include "expression_ref.H"

expression_ref case_expression(const expression_ref& T, const expression_ref& pattern, const expression_ref& body, const expression_ref& otherwise = expression_ref());

expression_ref case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

expression_ref make_case_expression(const expression_ref& T, const std::vector<expression_ref>& patterns, const std::vector<expression_ref>& bodies);

bool parse_case_expression(const expression_ref& E, expression_ref& T, std::vector<expression_ref>& patterns, std::vector<expression_ref>& bodies);

void parse_alternatives(const expression_ref& E, std::vector<expression_ref>& cases, std::vector<expression_ref>& results);

expression_ref def_function(const std::vector< std::vector<expression_ref> >& patterns, const std::vector<expression_ref>& bodies);

bool is_irrefutable_pattern(const expression_ref& E);

#endif
