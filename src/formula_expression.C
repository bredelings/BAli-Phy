#include "formula_expression.H"
#include "context.H"

using boost::shared_ptr;

formula_expression_ref formula_expression_ref::operator()(const formula_expression_ref& R) const
{
  return formula_expression_ref(combine(F, R.F), exp()(R.exp()));
}

formula_expression_ref expression_ref::operator()(const formula_expression_ref& arg1) const
{
  return formula_expression_ref(*this)(arg1);
}

formula_expression_ref expression_ref::operator()(const formula_expression_ref& arg1,
						  const formula_expression_ref& arg2) const
{
  return (*this)(arg1)(arg2);
}

formula_expression_ref expression_ref::operator()(const formula_expression_ref& arg1,
						  const formula_expression_ref& arg2,
						  const formula_expression_ref& arg3) const						  
{
  return (*this)(arg1)(arg2)(arg3);
}

formula_expression_ref expression_ref::operator()(const formula_expression_ref& arg1,
						  const formula_expression_ref& arg2,
						  const formula_expression_ref& arg3,
						  const formula_expression_ref& arg4) const						  
{
  return (*this)(arg1)(arg2)(arg3)(arg4);
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

boost::shared_ptr<const Object> formula_expression_ref::result() const
{
  Context C(F);
  return C.evaluate(index);
}
