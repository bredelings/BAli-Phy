#include "computation/computation.H"
#include "myexception.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_show(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  
  object_ptr<String> v (new String);

  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
    v->t = convertToString<double>(*xd);
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
    v->t = convertToString<int>(*xi);
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    log_double_t ld = *xld;
    v->t = "LD"+convertToString<double>(ld.log());
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
    v->t = (char)*xc;
  else if (object_ptr<const String> xs = dynamic_pointer_cast<const String>(x))
    v->t = xs->t;
  else
    throw myexception()<<"Add: object '"<<x->print()<<"' is not Double, Int, Log_Double, Char, or String'";

  return v;
}
