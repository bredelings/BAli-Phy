#include "formula_expression.H"
#include "bounds.H"
#include "context.H"

/* 1. So a formula_expression_ref is basically a model.
     It is a single evaluatable expression E with annotations (*notes*) N about its parameters.
     Neither the notes, nor the expression should be changed.

   2. Is E a note?
     Well, we need it to be a note for the following reasons.
     - A parameter might be "declared" in E.  Thus if E is removed, the number of parameters might change.
     - When we add prefixes to variables, we need to modify both E and N.
     - 

   3. What kinds of notes might we need?
      - default value [computable!]
      - bounds [computable!]
      - prior [computable!]
      - loggers
      - mcmc steps
      - clamps?
      Question: suppose we JUST have priors.  Is that still a model?  It has no compute expression.

   4. We could alternatively decide that a model 

   5. How about saying that a model includes ONLY those parameters mentioned in E?

   6. Which parameters does a model contain?
     - Currently, it contains any parameters mentioned anywhere.
     - If the expression is reduced a parameters might be eliminated.
       For example (@,\x->2,y) reduces to 2, which then no longer contains 'y'.
 */

using boost::shared_ptr;
using std::vector;

void formula_expression_ref::set_note(int i, const expression_ref& E)
{
  // how would we process changes to notes, when those notes have effects?
  notes[i] = E;
}

formula_expression_ref formula_expression_ref::operator()(const formula_expression_ref& R2) const
{
  // Perhaps I should take out the expression that is the argument... we perhaps not.
  formula_expression_ref R3(combine(notes, R2.get_notes()), I);
  R3.set_note(I, R3.get_note(I)(R2.exp()) );
  return R3;
}

formula_expression_ref formula_expression_ref::operator()(const expression_ref& R2) const
{
  formula_expression_ref R3(*this);
  R3.set_note(I, R3.get_note(I)(R2) );
  return R3;
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
  :I(-1)
{ }

formula_expression_ref::formula_expression_ref(const expression_ref& R)
  :I(0)
{ 
  add_note(R);
}

formula_expression_ref::formula_expression_ref(const Model_Notes& N, int i)
  :Model_Notes(N),I(i)
{ }

formula_expression_ref::formula_expression_ref(const Model_Notes& N, const expression_ref& R)
  :Model_Notes(N)
{ 
  I = add_note(R);
}

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  return formula_expression_ref(add_prefix(prefix, R.get_notes()), R.index());
}

int formula_expression_ref::add_expression(const formula_expression_ref& R)
{
  // The indices of the first argument are (currently) not altered, so 'index' is unchanged.
  int i = notes.size() + R.index();
  notes.insert(notes.end(), R.get_notes().begin(), R.get_notes().end());
  return i;
}

boost::shared_ptr<const Object> formula_expression_ref::result() const
{
  context C(get_notes());
  return C.evaluate_expression(exp());
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value)
{
  expression_ref var = parameter(name);
  N.add_note( default_value(var, def_value) );
  return var;
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value)
{
  Model_Notes N;
  expression_ref E = def_parameter(N, name, def_value);
  return formula_expression_ref(N,E);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b)
{
  formula_expression_ref Var = def_parameter(name, def_value);
  Var.add_expression( var_bounds(Var.exp(), b) );
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

formula_expression_ref lambda_quantify(const expression_ref& d, const formula_expression_ref& F)
{
  // Perhaps I should take out the expression that is the argument... we perhaps not.
  formula_expression_ref F2 = F;
  F2.set_note(F2.index(), lambda_quantify(d,F.exp()) );
  return F2;
}

formula_expression_ref operator,(const expression_ref E1, const formula_expression_ref F2)
{
  return E1(F2);
}

formula_expression_ref operator,(const formula_expression_ref F1, const expression_ref E2)
{
  return F1(E2);
}

formula_expression_ref operator,(const formula_expression_ref F1, const formula_expression_ref F2)
{
  return F1(F2);
}

formula_expression_ref operator&(const expression_ref& E1, const formula_expression_ref& F2)
{
  return (Cons,E1,F2);
}

formula_expression_ref operator&(const formula_expression_ref& F1, const expression_ref& E2)
{
  return (Cons,F1,E2);
}

formula_expression_ref operator&(const formula_expression_ref& F1, const formula_expression_ref& F2)
{
  return (Cons,F1,F2);
}

formula_expression_ref Tuple(const formula_expression_ref& R1,const formula_expression_ref& R2)
{
  return Tuple(2)(R1)(R2);
}

formula_expression_ref Tuple(const formula_expression_ref& R1,const formula_expression_ref& R2,const formula_expression_ref& R3)
{
  return Tuple(3)(R1)(R2)(R3);
}

formula_expression_ref Tuple(const formula_expression_ref& R1,const formula_expression_ref& R2,const formula_expression_ref& R3,const formula_expression_ref& R4)
{
  return Tuple(4)(R1)(R2)(R3)(R4);
}

formula_expression_ref Tuple(const formula_expression_ref& R1,const formula_expression_ref& R2,const formula_expression_ref& R3,const formula_expression_ref& R4,const formula_expression_ref& R5)
{
  return Tuple(5)(R1)(R2)(R3)(R4)(R5);
}

