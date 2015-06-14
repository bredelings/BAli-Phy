#include "computation/computation.H"

using boost::dynamic_pointer_cast;

extern "C" closure builtin_function_exp(OperationArgs& Args)
{
  double x = *Args.evaluate_as<Double>(0);

  return new Double(exp(x));
}

extern "C" closure builtin_function_log(OperationArgs& Args)
{
  auto x = Args.evaluate(0);

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
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);

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
  assert(*x >= 0.0);

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
  while(top->exp.size())
  {
    assert(is_exactly(top->exp,":"));
    assert(top->exp.size() == 2);

    int element_index = as_<index_var>(top->exp.sub()[0]).index;
    int element_reg = top->lookup_in_env( element_index );

    int next_index = as_<index_var>(top->exp.sub()[1]).index;
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
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
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
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
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
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
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

extern "C" closure builtin_function_mod(OperationArgs& Args)
{
  using boost::dynamic_pointer_cast;

  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
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

extern "C" closure builtin_function_subtract(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
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

extern "C" closure builtin_function_negate(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  
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

extern "C" closure builtin_function_equals(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) == (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) == (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) == (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) == (*yc)};
  }
  else
    throw myexception()<<"==: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_notequals(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) != (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) != (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) != (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) != (*yc)};
  }
  else
    throw myexception()<<"/=: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_greaterthan(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) > (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) > (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) > (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) > (*yc)};
  }
  else
    throw myexception()<<">: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_greaterthanorequal(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) >= (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) >= (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) >= (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) >= (*yc)};
  }
  else
    throw myexception()<<">=: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_lessthan(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) < (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) < (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) < (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) < (*yc)};
  }
  else
    throw myexception()<<"<: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

extern "C" closure builtin_function_lessthanorequal(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  auto y = Args.evaluate(1);
  
  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
  {
    object_ptr<const Double> yd = convert<const Double>(y);
    return {(*xd) <= (*yd)};
  }

  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
  {
    object_ptr<const Int> yi = convert<const Int>(y);
    return {(*xi) <= (*yi)};
  }
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    object_ptr<const Log_Double> yld = convert<const Log_Double>(y);
    return {(*xld) <= (*yld)};
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    object_ptr<const Char> yc = convert<const Char>(y);
    return {(*xc) <= (*yc)};
  }
  else
    throw myexception()<<"<=: object '"<<x->print()<<"' is not Double, Int, Log_double, or Char'";
}

#include "conversion.H"

extern "C" closure builtin_function_doubleToLogDouble(OperationArgs& Args)
{
  return numeric_conversion_function<double,log_double_t>(Args);
}

extern "C" closure builtin_function_intToDouble(OperationArgs& Args)
{
  return numeric_conversion_function<int,double>(Args);
}

#include "iota.H"

extern "C" closure builtin_function_iotaUnsigned(OperationArgs& Args)
{
  return iota_function<unsigned>(Args);
}

extern "C" closure builtin_function_join(OperationArgs& Args)
{
  Args.evaluate_slot_to_reg(0);
  int R = Args.evaluate_slot_to_reg(1);

  return {index_var(0),{R}};
}

extern "C" closure builtin_function_seq(OperationArgs& Args)
{
  Args.evaluate_slot_no_record(0);

  int index = as_<index_var>(Args.reference(1)).index;
  int R = Args.current_closure().lookup_in_env( index);

  return {index_var(0),{R}};
}

extern "C" closure builtin_function_show(OperationArgs& Args)
{
  auto x = Args.evaluate(0);
  
  object_ptr<String> v (new String);

  if (object_ptr<const Double> xd = dynamic_pointer_cast<const Double>(x))
    *v = convertToString<double>(*xd);
  else if (object_ptr<const Int> xi = dynamic_pointer_cast<const Int>(x))
    *v = convertToString<int>(*xi);
  else if (object_ptr<const Log_Double> xld = dynamic_pointer_cast<const Log_Double>(x))
  {
    log_double_t ld = *xld;
    *v = "LD"+convertToString<double>(ld.log());
  }
  else if (object_ptr<const Char> xc = dynamic_pointer_cast<const Char>(x))
  {
    std::string s;
    s = *xc;
    *v = s;
  }
  else if (object_ptr<const String> xs = dynamic_pointer_cast<const String>(x))
    *v = *xs;
  else
    throw myexception()<<"Add: object '"<<x->print()<<"' is not Double, Int, Log_Double, Char, or String'";

  return v;
}

extern "C" closure builtin_function_builtinError(OperationArgs& Args)
{
  std::string message = *Args.evaluate_as<String>(0);
  
  throw myexception()<<message;
}

extern "C" closure builtin_function_reapply(OperationArgs& Args)
{
  int index1 = as_<index_var>(Args.reference(0)).index;
  int R1 = Args.current_closure().lookup_in_env( index1 );

  int index2 = as_<index_var>(Args.reference(1)).index;
  int R2 = Args.current_closure().lookup_in_env( index2 );

  expression_ref apply_E;
  {
    expression_ref fE = index_var(1);
    expression_ref argE = index_var(0);
    apply_E = (fE, argE);
  }

  // %1 %0 {R1,R2}
  int apply_reg = Args.allocate({apply_E,{R1, R2}});

  // FIXME - aren't we trying to eliminate general evaluation of regs that aren't children?  See below:

  // Evaluate the newly create application reg - and depend upon it!
  if (Args.evaluate_changeables())
    Args.evaluate_reg_to_object(apply_reg);

  return {index_var(0),{apply_reg}};
}
