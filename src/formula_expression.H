#ifndef FORMULA_EXPRESSION_H
#define FORMULA_EXPRESSION_H

#include <boost/shared_ptr.hpp>
#include "formula.H"


struct formula_expression_ref
{
  boost::shared_ptr<const Formula> F;
  int index;

  formula_expression_ref operator()(const formula_expression_ref&) const;

  expression_ref exp() const {return F->sub_exp(index);}

  int add_expression(const expression_ref&);

  formula_expression_ref();
  formula_expression_ref(const expression_ref&);
  formula_expression_ref(const boost::shared_ptr<const Formula>& F, int);
  formula_expression_ref(const boost::shared_ptr<const Formula>& F, const expression_ref&);
};

formula_expression_ref prefix_formula(const std::string&,const formula_expression_ref&);

#endif
