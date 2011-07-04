#include "formula_expression.H"

using boost::shared_ptr;

formula_expression_ref formula_expression_ref::operator()(const formula_expression_ref& R) const
{
  return formula_expression_ref(combine(F, R.F), exp()(R.exp()));
}

formula_expression_ref expression_ref::operator()(const formula_expression_ref& arg) const
{
  return formula_expression_ref(*this)(arg);
}

formula_expression_ref::formula_expression_ref()
  :index(-1)
{ }

formula_expression_ref::formula_expression_ref(const expression_ref& R)
{
  Formula* F2 = new Formula();
  index = F2->add_expression(R);
  F = shared_ptr<const Formula>(F2);
}

formula_expression_ref::formula_expression_ref(const shared_ptr<const Formula>& F1, int i)
  : F(F1),index(i)
{ }

formula_expression_ref::formula_expression_ref(const shared_ptr<const Formula>& F1, const expression_ref& R)
{
  Formula* F2 = F1->clone();
  index = F2->add_expression(R);
  F = shared_ptr<const Formula>(F2);
}

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  return formula_expression_ref(prefix_formula(prefix,R.F),R.index);
}

int formula_expression_ref::add_expression(const expression_ref& R)
{
  Formula* F2 = F->clone();
  int new_index = F2->add_expression(R);
  F = shared_ptr<const Formula>(F2);
  return new_index;
}
