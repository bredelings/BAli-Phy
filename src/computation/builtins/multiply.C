#include "computation/computation.H"
#include "myexception.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_multiply(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return (*xd) * (*yd);
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return (*xi) * (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return (*xld) * (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return (*xc) * (*yc);
  }
  else
    throw myexception()<<"Multiply: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}
