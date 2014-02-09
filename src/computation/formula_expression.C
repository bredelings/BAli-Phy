#include "formula_expression.H"
#include "bounds.H"
#include "context.H"
#include "computation/module.H"
#include "parser/AST.H"

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
using std::set;
using std::string;

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

formula_expression_ref substitute(const formula_expression_ref& R, const expression_ref& E1, const expression_ref& E2)
{
  const Model_Notes& N = R;
  return {substitute(N,E1,E2), substitute(R.exp(), E1, E2)};
}

formula_expression_ref rename_module(const formula_expression_ref F, const string& modid1, const string& modid2)
{
  formula_expression_ref F2 = F;
  for(auto& note: F2.get_notes())
    note = rename_module(note, modid1, modid2);
  F2.set_exp(rename_module(F2.exp(), modid1, modid2));
  return F2;
}

formula_expression_ref prefix_formula(const std::string& prefix,const formula_expression_ref& R)
{
  set<string> declared_parameter_names = find_declared_parameters(R.get_notes());
  formula_expression_ref R2 = R;
  for(const auto& name: declared_parameter_names)
    R2 = substitute(R2, parameter(name), parameter(prefix+"."+name));

  return R2;
}

int formula_expression_ref::add_expression(const formula_expression_ref& R)
{
  add_notes(R.get_notes());
  return add_note(R.exp());
}

expression_ref perform_exp(const expression_ref& F)
{
  expression_ref E = F;
  if (is_AST(E,"model"))
  {
    E = translate_model(E);
    E = (identifier("gen_model"),E);
    E = (identifier("unsafePerformIO'"),E);
    E = (identifier("evaluate"),-1,E);
  }
  return E;
}

object_ptr<const Object> result(const expression_ref& E, const module_loader& L)
{
  return result(E, L,vector<Module>{});
}

object_ptr<const Object> result(const expression_ref& E, const module_loader& L, const vector<Module>& Ps)
{
  context C(L, {}, Ps);
  return C.evaluate_expression(perform_exp(E));
}

object_ptr<const Object> result(const expression_ref& E, const module_loader& L, const vector<string>& module_names)
{
  context C(L, {}, module_names);
  return C.evaluate_expression(perform_exp(E));
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

std::ostream& operator<<(std::ostream& o, const formula_expression_ref& F)
{
  o<<F.exp();
  return o;
}

formula_expression_ref let_expression(const expression_ref& var, const expression_ref& body, const formula_expression_ref& T)
{
  formula_expression_ref F3(T);
  F3.set_exp(let_expression(var, body, T.exp() ));
  return F3;
}

formula_expression_ref let_expression(const expression_ref& var, const formula_expression_ref& body, const expression_ref& T)
{
  formula_expression_ref F3(body);
  F3.set_exp(let_expression(var, body.exp(), T ));
  return F3;
}

formula_expression_ref let_expression(const expression_ref& var, const formula_expression_ref& body, const formula_expression_ref& T)
{
  formula_expression_ref F3(body);
  F3.add_notes(T.get_notes());
  F3.set_exp(let_expression(var, body.exp(), T.exp() ));
  return F3;
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
  return Cons*E1*F2;
}

formula_expression_ref operator&(const formula_expression_ref& F1, const expression_ref& E2)
{
  return Cons*F1*E2;
}

formula_expression_ref operator&(const formula_expression_ref& F1, const formula_expression_ref& F2)
{
  return Cons*F1*F2;
}

formula_expression_ref operator*(const formula_expression_ref& F1, const formula_expression_ref& F2)
{
  formula_expression_ref F3(F1);
  F3.add_notes(F2.get_notes());
  F3.set_exp(F1.exp() * F2.exp());
  return F3;
}

formula_expression_ref operator*(const expression_ref& E1, const formula_expression_ref& F2)
{
  return formula_expression_ref{E1}*F2;
}

formula_expression_ref operator*(const formula_expression_ref& F1, const expression_ref& E2)
{
  return F1*formula_expression_ref{E2};
}

formula_expression_ref get_list(const vector<formula_expression_ref>& v)
{
  formula_expression_ref F = ListEnd;

  for(int i=v.size()-1;i>=0;i--)
    F = v[i]&F;

  return F;
}

expression_ref formula_expression_ref::perform_exp() const
{
  return ::perform_exp(E);
}

expression_ref model_expression(const vector<expression_ref>& es)
{
  return expression_ref(AST_node("model"),es);
}

expression_ref translate_model(const expression_ref& E)
{
  if (is_AST(E,"model"))
  {
    int index = 0;
    for(const auto& F: E->sub)
      index = std::max(index,get_safe_binder_index(F));

    expression_ref E2 = E->sub[0];
    int A = E->sub.size()-1;
    for(int i=0;i<A;i++)
      E2 = (E2,dummy(index+i));
    for(int i=A-1;i>=0;i--)
    {
      expression_ref arg = translate_model(E->sub[i+1]);
      E2 = (identifier(">>="),arg,dummy(index+i)^E2);
    }
    return E2;
  }
  else
    return ((identifier("return"),E));
}
