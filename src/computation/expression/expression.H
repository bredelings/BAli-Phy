#ifndef EXPRESSION_H
#define EXPRESSION_H

#include <string>
#include "expression_ref.H"

bool is_reglike(const expression_ref& E);

expression_ref launchbury_normalize(const expression_ref& E);
expression_ref launchbury_unnormalize(const expression_ref& E);

bool is_WHNF(const expression_ref& E);

expression_ref parse_object(const std::string& s);

#endif
