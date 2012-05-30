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

   7. Q1: Do I want to separate the expression from the notes, again?
      Q2: Can I combine subsets of notes incrementally without 
          expression syntax for formula_expression_ref?
 */

using std::vector;

std::vector<expression_ref> formula_expression_ref::get_notes_plus_exp() const
{
  Model_Notes M = *this;
  M.add_note( exp() );
  return M.get_notes();
}

formula_expression_ref::formula_expression_ref()
{ }

formula_expression_ref::formula_expression_ref(const expression_ref& R)
  :E(R)
{ }

formula_expression_ref::formula_expression_ref(const Model_Notes& N, int i)
  :Model_Notes(N),E(N.get_note(i))
{ }

formula_expression_ref::formula_expression_ref(const Model_Notes& N, const expression_ref& R)
  :Model_Notes(N),E(R)
{  }

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  return formula_expression_ref( add_prefix(prefix, R), add_prefix(prefix, R.exp() ) );
}

int formula_expression_ref::add_expression(const formula_expression_ref& R)
{
  add_notes(R.get_notes());
  return add_note(R.exp());
}

object_ptr<const Object> formula_expression_ref::result() const
{
  Program P;
  return result(P);
}

object_ptr<const Object> formula_expression_ref::result(const Program& P) const
{
  context C(get_notes_plus_exp());
  C += P;
  return C.evaluate_expression(exp());
}

formula_expression_ref apply(const formula_expression_ref& F1, const expression_ref& E2)
{
  formula_expression_ref F3(F1);
  F3.set_exp(apply(F3.exp(), E2));
  return F3;
}

formula_expression_ref apply(const formula_expression_ref& F1, const formula_expression_ref& F2)
{
  formula_expression_ref F3(F1);
  F3.add_notes(F2.get_notes());
  F3.set_exp(apply(F1.exp(), F2.exp()));
  return F3;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value)
{
  expression_ref var = parameter(name);
  N.add_note( (default_value, var, def_value) );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, const Bounds<double>& b)
{
  expression_ref var = def_parameter(N, name, def_value);
  N.add_note( (var_bounds, var, b) );
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& D)
{
  expression_ref var = def_parameter(N, name, def_value, b);
  N.add_note( (distributed, var, D));
  return var;
}

expression_ref def_parameter(Model_Notes& N, const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& F, const expression_ref& A)
{
  expression_ref D = Tuple(F,A);
  return def_parameter(N, name, def_value, b, D);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value)
{
  Model_Notes N;
  expression_ref E = def_parameter(N, name, def_value);
  return formula_expression_ref(N,E);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b)
{
  Model_Notes N;
  expression_ref E = def_parameter(N, name, def_value, b);
  return formula_expression_ref(N,E);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& D)
{
  Model_Notes N;
  expression_ref E = def_parameter(N, name, def_value, b, D);
  return formula_expression_ref(N,E);
}

formula_expression_ref def_parameter(const std::string& name, const expression_ref& def_value, const Bounds<double>& b, const expression_ref& F, const expression_ref& A)
{
  Model_Notes N;
  expression_ref E = def_parameter(N, name, def_value, b, F, A);
  return formula_expression_ref(N,E);
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
  F2.set_exp(lambda_quantify(d,F.exp()) );
  return F2;
}

formula_expression_ref operator^(const expression_ref& d, const formula_expression_ref& F)
{
  return lambda_quantify(d, F);
}

formula_expression_ref operator,(const expression_ref& E1, const formula_expression_ref& F2)
{
  return apply(E1,F2);
}

formula_expression_ref operator,(const formula_expression_ref& F1, const expression_ref& E2)
{
  return apply(F1,E2);
}

formula_expression_ref operator,(const formula_expression_ref& F1, const formula_expression_ref& F2)
{
  return apply(F1,F2);
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
