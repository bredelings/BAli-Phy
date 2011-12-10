#include "formula_expression.H"
#include "context.H"
#include "bounds.H"

using boost::shared_ptr;

formula_expression_ref formula_expression_ref::operator()(const formula_expression_ref& R) const
{
  return formula_expression_ref(combine(notes, R.notes), exp()(R.exp()));
}

formula_expression_ref formula_expression_ref::operator()(const expression_ref& R) const
{
  return formula_expression_ref(notes, exp()(R));
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
{ }

formula_expression_ref::formula_expression_ref(const expression_ref& r)
  :R(r), notes(new Formula)
{ }

formula_expression_ref::formula_expression_ref(const shared_ptr<const Formula>& F1, const expression_ref& r)
  :R(r),
   notes(F1)
{ }

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  return formula_expression_ref(prefix_formula(prefix, R.notes), add_prefix(prefix, R.exp()));
}

int formula_expression_ref::add_expression(const formula_expression_ref& R)
{
  // The indices of the first argument are (currently) not altered, so 'index' is unchanged.
  notes = combine(notes, R.notes);
  return notes->add_expression(R.exp());
}

boost::shared_ptr<const Object> formula_expression_ref::result() const
{
  polymorphic_cow_ptr<Formula> notes2 = notes;
  int index = notes2->add_expression(exp());
  Context C(notes2);
  return C.evaluate(index);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b)
{
  expression_ref var = parameter(name);
  formula_expression_ref Var (var);
  Var.add_expression( default_value(var, def_value) );
  Var.add_expression( bounds(var, b) );
  return Var;
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& D)
{
  formula_expression_ref Var = def_parameter(name,def_value,b);
  Var.add_expression(distributed(Var.exp(),D));
  return Var;
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& F, const expression_ref& A)
{
  expression_ref D = Tuple(2)(F,A);
  return def_parameter(name,def_value,b,D);
}

std::ostream& operator<<(std::ostream& o, const formula_expression_ref& F)
{
  o<<F.exp();
  return o;
}
