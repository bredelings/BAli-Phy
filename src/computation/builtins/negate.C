#include "computation/computation.H"
#include "myexception.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
    return -(*xd);
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
    return -(*xi);
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
    return -(*xld);
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
    return -(*xc);
  else
    throw myexception()<<"Negate: object '"<<x->print()<<"' is not Double, Int, Log_Double, or Char'";
}
