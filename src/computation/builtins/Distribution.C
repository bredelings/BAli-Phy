#include <vector>
#include <valarray>
#include <string>
#include "computation/operation.H"
#include "computation/computation.H"
#include "computation/module.H"
#include "probability/probability.H"
#include "bounds.H"

using std::vector;
using std::string;
using std::valarray;

extern "C" closure builtin_function_exponential_density(OperationArgs& Args)
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(x,mu);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_gamma_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_gamma_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = gamma_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_beta_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = beta_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_beta_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);
  
  Double result = beta_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_normal_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}
 
extern "C" closure builtin_function_normal_quantile(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = normal_quantile(p, a1 ,a2);
  return object_ptr<const Object>(result.clone());
}

extern "C" closure builtin_function_cauchy_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  Log_Double result = cauchy_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

// First convert N from tuple to list...
// Second convert this builtin routine to just take two Vector<double> arguments.
// Third convert all the expression_ref's here to "var" and use Distribution_Functions()
extern "C" closure builtin_function_dirichlet_density(OperationArgs& Args)
{
  object_ptr<const Vector<double>> n = Args.evaluate_as<Vector<double>>(0);
  object_ptr<const Vector<double>> x = Args.evaluate_as<Vector<double>>(1);
  
  object_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(*x,*n) ) );
  
  return R;
}

extern "C" closure builtin_function_laplace_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x, a1, a2) ) );
}

extern "C" closure builtin_function_uniform_density(OperationArgs& Args)
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a1,a2) ) );
}
