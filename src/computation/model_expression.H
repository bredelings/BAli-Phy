#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include <string>
#include "expression.H"
#include <iostream>
#include "computation/loader.H"

class Module;

expression_ref perform_exp(const expression_ref&);

expression_ref perform_exp(const expression_ref&, const std::string& prefix);

#endif
