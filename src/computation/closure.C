#include "object.H"
#include "computation/operation.H"
#include "computation/expression.H"

using std::vector;
using std::string;

void closure::clear()
{
  exp.clear();
  Env.clear();
}

string closure::print() const
{
  string result = exp.print();
  if (Env.size())
    result += " {" + join(Env,", ") + "}";
  return result;
}

closure get_trimmed(const closure& C)
{
  closure C2 = C;
  return get_trimmed(std::move(C2));
}

closure get_trimmed(closure&& C)
{
  if (C.exp.head().type() == trim_type)
  {
    expression_ref old = C.exp;
    const vector<int>& keep = as_<Vector<int>>(old.sub()[0]);

    C.exp = C.exp.sub()[1];

    // Since environments are indexed backwards
    for(int i=0;i<keep.size();i++)
    {
      int k = keep[keep.size()-1-i];
      C.Env[i] = C.lookup_in_env(k);
    }
    C.Env.resize(keep.size());

    // Should this ever happen?
    assert(not is_a<Trim>(C.exp));
  }
  
  return std::move(C);
}

expression_ref deindexify(const closure& C)
{
  vector<expression_ref> variables;
  for(int R: C.Env)
    variables.push_back(reg_var(R));
  
  return deindexify(C.exp, variables);
}

closure trim_unnormalize(closure C)
{
  C.exp = trim_unnormalize(C.exp);
  return C;
}

