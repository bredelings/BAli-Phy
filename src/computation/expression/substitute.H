#ifndef SUBSTITUTE_H
#define SUBSTITUTE_H

#include <vector>
#include "expression_ref.H"
#include "util/object.H"

expression_ref substitute(const expression_ref& E1, int var, const expression_ref& E2);

expression_ref substitute(const expression_ref& E1, const expression_ref& D, const expression_ref& E2);

/// Substitute for all matches found
expression_ref substitute(const expression_ref& E, const std::vector<expression_ref>& replace);

void alpha_rename(object_ptr<expression>& E, const expression_ref& x, const expression_ref& y);

#endif
