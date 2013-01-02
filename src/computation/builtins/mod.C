#include "computation/operations.H"
#include "computation/graph_register.H"
#include "math/exponential.H"
#include "myexception.H"

extern "C" closure builtin_function(OperationArgs& Args)
{
  using boost::dynamic_pointer_cast;

  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return Int( *xi % *yi );
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return Char( *xc % *yc );
  }
  else
    throw myexception()<<"Mod: object '"<<x->print()<<"' is not Int, or Char'";
}

