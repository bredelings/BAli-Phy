#include "object.H"
#include "computation/operation.H"
#include "computation/expression.H"

using std::vector;
using std::string;

void closure::clear()
{
  exp.reset();
  Env.clear();
}

string closure::print() const
{
  string result = exp->print();
  if (Env.size())
    result += " {" + join(Env,", ") + "}";
  return result;
}

closure get_trimmed(const closure& C)
{
  closure C2 = C;

  if (is_a<Trim>(C.exp))
  {
    C2.exp = C.exp->sub[1];
    
    const vector<int>& keep = is_a<Vector<int>>(C.exp->sub[0])->t;
    
    // Since environments are indexed backwards
    C2.Env.resize(keep.size());
    for(int i=0;i<keep.size();i++)
      C2.Env[i] = C.lookup_in_env(keep[keep.size() - 1 - i]);

    // Should this ever happen?
    assert(not is_a<Trim>(C2.exp));

    // For safety... 
    C2 = get_trimmed(C2);
  }
  
  return C2;
}
