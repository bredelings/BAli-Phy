#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include "expression.H"
#include <iostream>
#include "model_notes.H"
#include "computation/loader.H"

class Module;

class formula_expression_ref: public Model_Notes
{
  expression_ref E;

public:

  void set_exp(const expression_ref& E2) {E = E2;}

  expression_ref exp() const {return E;}

  expression_ref perform_exp() const;

  int add_expression(const formula_expression_ref&);

  object_ptr<const Object> result(const module_loader& L) const;

  object_ptr<const Object> result(const module_loader& L, const std::vector<Module>&) const;

  object_ptr<const Object> result(const module_loader& L, const std::vector<std::string>&) const;

  template <typename T>
  object_ptr<const T> result_as(const module_loader& L) const {return boost::dynamic_pointer_cast<const T>(result(L));}

  formula_expression_ref();
  formula_expression_ref(const expression_ref&);
  formula_expression_ref(const Model_Notes&, int);
  formula_expression_ref(const Model_Notes&, const expression_ref&);
};

formula_expression_ref apply(const formula_expression_ref& F1, const expression_ref& E2);
formula_expression_ref apply(const formula_expression_ref& F1, const formula_expression_ref& F2);

formula_expression_ref prefix_formula(const std::string&,const formula_expression_ref&);

std::ostream& operator<<(std::ostream&, const formula_expression_ref& F);

template <typename T> struct Bounds;

formula_expression_ref let_expression(const expression_ref& var, const expression_ref& body, const formula_expression_ref& T);
formula_expression_ref let_expression(const expression_ref& var, const formula_expression_ref& body, const expression_ref& T);
formula_expression_ref let_expression(const expression_ref& var, const formula_expression_ref& body, const formula_expression_ref& T);

formula_expression_ref lambda_quantify(const expression_ref&, const formula_expression_ref&);
formula_expression_ref operator^(const expression_ref&, const formula_expression_ref&);

formula_expression_ref operator,(const expression_ref&, const formula_expression_ref&);
formula_expression_ref operator,(const formula_expression_ref&, const expression_ref&);
formula_expression_ref operator,(const formula_expression_ref&, const formula_expression_ref&);

formula_expression_ref operator&(const expression_ref&, const formula_expression_ref&);
formula_expression_ref operator&(const formula_expression_ref&, const expression_ref&);
formula_expression_ref operator&(const formula_expression_ref&, const formula_expression_ref&);

formula_expression_ref operator*(const expression_ref&, const formula_expression_ref&);
formula_expression_ref operator*(const formula_expression_ref&, const expression_ref&);
formula_expression_ref operator*(const formula_expression_ref&, const formula_expression_ref&);

formula_expression_ref get_list(const std::vector<formula_expression_ref>&);

expression_ref model_expression(const std::vector<expression_ref>& es);
expression_ref translate_model(const expression_ref&);
#endif
