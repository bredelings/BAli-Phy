#include "computation/computation.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_log(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);

  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    double xx = *xd;
    assert(xx > 0.0);
    return new Double(log(xx));
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    double xx = (int)*xi;
    assert(xx > 0.0);
    return new Double(log(xx));
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    log_double_t xx = *xld;
    return new Double(log(xx));
  }

  throw myexception()<<"log: object '"<<x->print()<<"' is not Double, Int, or Log_Double";
}

extern "C" closure builtin_function_pow(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);

  double yy = 0;
  if (object_ptr<const Double> yd = dynamic_pointer_cast<const Double>(y))
    yy = *yd;
  else if (object_ptr<const Int> yi = dynamic_pointer_cast<const Int>(y))
    yy = (int)*yi;
  else
    throw myexception()<<"pow: exponent '"<<x->print()<<"' is not Double or Int";
    
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    double xx = *xd;
    assert(xx > 0.0);
    return new Double(pow(xx,yy));
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    double xx = (int)*xi;
    assert(xx > 0.0);
    return new Double(pow(xx,yy));
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    log_double_t xx = *xld;
    return new Log_Double(pow(xx,yy));
  }

  throw myexception()<<"pow: object '"<<x->print()<<"' is not Double, Int, or Log_Double";
}

extern "C" closure builtin_function_sqrt(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(sqrt(*x));
}

extern "C" closure builtin_function_truncate(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);

  return new Double(trunc(*x));
}

extern "C" closure builtin_function_ceiling(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);

  return new Double(ceil(*x));
}

extern "C" closure builtin_function_floor(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(floor(*x));
}


extern "C" closure builtin_function_round(OperationArgs& Args)
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  assert(*x > 0.0);

  return new Double(round(*x));
}

extern "C" closure builtin_function_doubleToInt(OperationArgs& Args)
{
  double x = *Args.evaluate_as<Double>(0);
  int xi = (int)x;
  return new Int(xi);
}

extern "C" closure builtin_function_vector_from_list(OperationArgs& Args)
{
  object_ptr<OVector> v (new OVector);

  const closure* top = &Args.evaluate_slot_to_closure(0);
  while(top->exp->size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp->size() == 2);

    int element_index = assert_is_a<index_var>(top->exp->sub[0])->index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = assert_is_a<index_var>(top->exp->sub[1])->index;
    int next_reg = top->lookup_in_env( next_index );

    // Add the element to the list.
    v->push_back( Args.evaluate_reg_to_object(element_reg) );
    // Move to the next element or end
    top = &Args.evaluate_reg_to_closure(next_reg);
  }
  assert(is_exactly(top->exp,"[]"));

  return v;
}

extern "C" closure builtin_function_add(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert_and_check<const Double>(y);
    return (*xd) + (*yd);
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert_and_check<const Int>(y);
    return (*xi) + (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert_and_check<const Log_Double>(y);
    return (*xld) + (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert_and_check<const Char>(y);
    return (*xc) + (*yc);
  }
  else
    throw myexception()<<"Add: object '"<<x->print()<<"' is not Double, Int, Log_Double, or Char'";
}

extern "C" closure builtin_function_multiply(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert_and_check<const Double>(y);
    return (*xd) * (*yd);
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert_and_check<const Int>(y);
    return (*xi) * (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert_and_check<const Log_Double>(y);
    return (*xld) * (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert_and_check<const Char>(y);
    return (*xc) * (*yc);
  }
  else
    throw myexception()<<"Multiply: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_divide(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert_and_check<const Double>(y);
    return (*xd) / (*yd);
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert_and_check<const Int>(y);
    return (*xi) / (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert_and_check<const Log_Double>(y);
    return (*xld) / (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert_and_check<const Char>(y);
    return (*xc) / (*yc);
  }
  else
    throw myexception()<<"Divide: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_subtract(OperationArgs& Args)
{
  object_ref x = Args.evaluate(0);
  object_ref y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert_and_check<const Double>(y);
    return (*xd) - (*yd);
  }
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert_and_check<const Int>(y);
    return (*xi) - (*yi);
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert_and_check<const Log_Double>(y);
    return (*xld) - (*yld);
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert_and_check<const Char>(y);
    return (*xc) - (*yc);
  }
  else
    throw myexception()<<"Minus: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}
