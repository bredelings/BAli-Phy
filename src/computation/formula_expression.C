#include "formula_expression.H"
#include "context.H"
#include "computation/module.H"
#include "parser/AST.H"

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
  context C(L, Ps);
  return C.evaluate_expression(perform_exp(E));
}

object_ptr<const Object> result(const expression_ref& E, const module_loader& L, const vector<string>& module_names)
{
  context C(L, module_names);
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
    for(const auto& F: E.sub())
      index = std::max(index,get_safe_binder_index(F));

    expression_ref E2 = E.sub()[0];
    int A = E.size()-1;
    for(int i=0;i<A;i++)
      E2 = (E2,dummy(index+i));
    for(int i=A-1;i>=0;i--)
    {
      expression_ref arg = translate_model(E.sub()[i+1]);
      E2 = (identifier(">>="),arg,dummy(index+i)^E2);
    }
    return E2;
  }
  else
    return ((identifier("return"),E));
}
