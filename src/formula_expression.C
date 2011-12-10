#include "formula_expression.H"
#include "context.H"
#include "bounds.H"
#include "graph_register.H"

using boost::shared_ptr;
using std::vector;

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

formula_expression_ref::formula_expression_ref(const expression_ref& R)
  :I(0),notes(1,R)
{ }

formula_expression_ref::formula_expression_ref(const vector<expression_ref>& N, int i)
  :I(i),notes(N)
{ }

formula_expression_ref::formula_expression_ref(const vector<expression_ref>& N, const expression_ref& R)
  :notes(N)
{ 
  I = N.size();
  notes.push_back(R);
}

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  return formula_expression_ref(prefix_formula(prefix, R.notes), R.index());
}

int formula_expression_ref::add_expression(const formula_expression_ref& R)
{
  // The indices of the first argument are (currently) not altered, so 'index' is unchanged.
  int i = notes.size() + R.index();
  notes.insert(notes.end(), R.notes.begin(), R.notes.end());
  return i;
}

boost::shared_ptr<const Object> formula_expression_ref::result() const
{
  context C(notes);
  return C.evaluate_expression(exp());
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
