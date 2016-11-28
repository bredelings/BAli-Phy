#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include <string>
#include "expression.H"
#include <iostream>
#include "computation/loader.H"

class Module;

expression_ref perform_exp(const expression_ref&);

expression_ref perform_exp(const expression_ref&, const std::string& prefix);

expression_ref result(const expression_ref&, const module_loader& L);

expression_ref result(const expression_ref&, const module_loader& L, const std::vector<Module>&);

expression_ref result(const expression_ref&, const module_loader& L, const std::vector<std::string>&);

expression_ref model_expression(const std::vector<expression_ref>& es);

expression_ref prefix(const expression_ref& s, const expression_ref& E);

expression_ref add_logger(const std::string& s, const expression_ref& E);

expression_ref translate_model(const expression_ref&);

#endif
