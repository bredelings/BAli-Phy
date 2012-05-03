#include "object.H"
#include "computation/operation.H"
#include "computation/expression.H"

using std::vector;
using boost::dynamic_pointer_cast;

void closure::clear()
{
  exp.reset();
  Env.clear();
}

std::string closure::print() const
{
  return exp->print()+ "{" + join(Env,",")+"}";
}

closure get_trimmed(const closure& C)
{
  closure C2 = C;

  if (object_ptr<const expression> E = is_a(C.exp ,Trim()))
  {
    C2.exp = E->sub[2];
    
    const vector<int>& keep = dynamic_pointer_cast<const Vector<int>>(E->sub[1])->t;
    
    // Since environments are indexed backwards
    C2.Env.resize(keep.size());
    for(int i=0;i<keep.size();i++)
      C2.Env[i] = C.lookup_in_env(keep[keep.size() - 1 - i]);

    // Should this ever happen?
    assert(not is_a(C2.exp, Trim()));

    // For safety... 
    C2 = get_trimmed(C2);
  }
  
  return C2;
}
