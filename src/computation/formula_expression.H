#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include <boost/shared_ptr.hpp>
#include "expression.H"
#include <iostream>

class formula_expression_ref
{
  int I;

  // I should probably make a note_collection class that is shared
  // between formula_expression_ref and class context.
  std::vector<expression_ref> notes;
public:

  int add_note(const expression_ref&);

  expression_ref get_note(int i) const {return notes[i];}

  const std::vector<expression_ref>& get_notes() const {return notes;}

  int n_notes() const {return notes.size();}

  void set_note(int i, const expression_ref&);

  formula_expression_ref operator()(const formula_expression_ref&) const;

  formula_expression_ref operator()(const expression_ref&) const;

  int index() const {return I;}

  expression_ref exp() const {return notes[I];}

  int add_expression(const formula_expression_ref&);

  boost::shared_ptr<const Object> result() const;

  template <typename T>
  boost::shared_ptr<const T> result_as() const {return boost::dynamic_pointer_cast<const T>(result());}

  formula_expression_ref();
  formula_expression_ref(const expression_ref&);
  formula_expression_ref(const std::vector<expression_ref>&, int);
  formula_expression_ref(const std::vector<expression_ref>&, const expression_ref&);
};

formula_expression_ref prefix_formula(const std::string&,const formula_expression_ref&);

std::ostream& operator<<(std::ostream&, const formula_expression_ref& F);

template <typename T> struct Bounds;

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&);
formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref&, const expression_ref&);

formula_expression_ref lambda_quantify(const expression_ref&, const formula_expression_ref&);

formula_expression_ref operator,(const expression_ref, const formula_expression_ref);
formula_expression_ref operator,(const formula_expression_ref, const expression_ref);
formula_expression_ref operator,(const formula_expression_ref, const formula_expression_ref);

formula_expression_ref operator&(const expression_ref, const formula_expression_ref);
formula_expression_ref operator&(const formula_expression_ref, const expression_ref);
formula_expression_ref operator&(const formula_expression_ref, const formula_expression_ref);

formula_expression_ref Tuple(const formula_expression_ref&, const formula_expression_ref&);
formula_expression_ref Tuple(const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&);
formula_expression_ref Tuple(const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&);
formula_expression_ref Tuple(const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&, const formula_expression_ref&);
#endif
