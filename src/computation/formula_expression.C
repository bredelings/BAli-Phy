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

expression_ref perform_exp(const expression_ref& F, const string& prefix)
{
  expression_ref E = F;
  if (is_AST(E,"model"))
  {
    E = translate_model(E);
    E = (identifier("add_prefix"),prefix,E);
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
