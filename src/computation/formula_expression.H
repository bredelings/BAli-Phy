#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include "expression.H"
#include <iostream>
#include "model_notes.H"

class Program;

class formula_expression_ref: public Model_Notes
{
  expression_ref E;

  // I should probably make a note_collection class that is shared
  // between formula_expression_ref and class context.
public:

  void set_exp(const expression_ref& E2) {E = E2;}

  expression_ref exp() const {return E;}

  int add_expression(const formula_expression_ref&);

  object_ptr<const Object> result() const;

  object_ptr<const Object> result(const Program&) const;

  template <typename T>
  object_ptr<const T> result_as() const {return boost::dynamic_pointer_cast<const T>(result());}

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

formula_expression_ref def_parameter(const std::string& name);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&, const expression_ref&);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&, const expression_ref&);

expression_ref def_parameter(Model_Notes&, const std::string& name);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&);
expression_ref def_parameter(Model_Notes&, const std::string& name, const expression_ref& def_value, std::nullptr_t, const expression_ref&, const expression_ref&);

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

formula_expression_ref get_list(const std::vector<formula_expression_ref>&);

std::set<std::string> find_declared_parameters(const std::vector<expression_ref>&);
std::set<std::string> find_declared_parameters(const Model_Notes&);

#endif
