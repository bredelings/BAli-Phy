#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include "expression.H"
#include <iostream>
#include "computation/loader.H"

class Module;

expression_ref perform_exp(const expression_ref&);

expression_ref perform_exp(const expression_ref&, const std::string& prefix);

expression_ref result(const expression_ref&, const module_loader& L);

expression_ref result(const expression_ref&, const module_loader& L, const std::vector<Module>&);

expression_ref result(const expression_ref&, const module_loader& L, const std::vector<std::string>&);

template <typename T>
object_ptr<const T> result_as(const expression_ref& E,const module_loader& L) {return boost::dynamic_pointer_cast<const T>(result(E,L));}

template <typename T> struct Bounds;

expression_ref model_expression(const std::vector<expression_ref>& es);
expression_ref translate_model(const expression_ref&);
#endif
