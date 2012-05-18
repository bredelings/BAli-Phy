#include "distribution-operations.H"
#include "distribution.H"

using std::vector;
using std::valarray;

closure exponential_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  object_ptr<const Double> mu = Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(*x,*mu);
  return object_ptr<const Object>(result.clone());
}

closure log_exponential_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  double mu = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(log(x),mu)/x;
  return object_ptr<const Object>(result.clone());
}

closure gamma_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);
  
  std::valarray<double> A = get_varray<double,Double>(a);
  Log_Double result = gamma_pdf(*x, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure gamma_quantile_op::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> p = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);

  std::valarray<double> A = get_varray<double,Double>(a);
  Double result = gamma_quantile(*p, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure beta_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);
  
  std::valarray<double> A = get_varray<double,Double>(a);
  Log_Double result = beta_pdf(*x, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure normal_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);
  
  std::valarray<double> A = get_varray<double,Double>(a);
  Log_Double result = normal_pdf(*x, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure log_normal_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);
  
  std::valarray<double> A = get_varray<double,Double>(a);
  Log_Double result = log_normal_pdf(*x, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure log_normal_quantile_op::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> p = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);

  std::valarray<double> A = get_varray<double,Double>(a);
  Double result = log_normal_quantile(*p, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure cauchy_density::operator()(OperationArgs& Args) const
{
  object_ptr<const Double> x = Args.evaluate_as<Double>(0);
  expression_ref a = Args.evaluate_structure(1);
  
  std::valarray<double> A = get_varray<double,Double>(a);
  Log_Double result = cauchy_pdf(*x, A[0], A[1]);
  return object_ptr<const Object>(result.clone());
}

closure dirichlet_density::operator()(OperationArgs& Args) const
{
  expression_ref X = Args.evaluate_structure(0);
  expression_ref N = Args.evaluate_structure(1);
  
  std::vector<double> x = get_vector_from_list<double,Double>(X);
  std::vector<double> n = get_vector<double,Double>(N);
  
  object_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(x,n) ) );
  
  return R;
}

closure laplace_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  expression_ref A = Args.evaluate_structure(1);

  vector<double> a = get_vector<double,Double>(A);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x,a[0],a[1]) ) );
}

closure log_laplace_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  expression_ref A = Args.evaluate_structure(1);

  vector<double> a = get_vector<double,Double>(A);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(log(x),a[0],a[1])/x ) );
}

closure uniform_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  expression_ref A = Args.evaluate_structure(1);

  vector<double> a = get_vector<double,Double>(A);
  
  return object_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a[0],a[1]) ) );
}

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( constructor("prob_density",3) );

expression_ref exponential_dist = (prob_density, "Exponential", lambda_expression(exponential_density()), 0);

expression_ref log_exponential_dist = (prob_density, "LogExponential", lambda_expression(log_exponential_density()), 0);

expression_ref gamma_dist       = (prob_density, "Gamma", lambda_expression(gamma_density()), lambda_expression(gamma_quantile_op()));

expression_ref beta_dist        = (prob_density, "Beta", lambda_expression(beta_density()), 0);

expression_ref normal_dist      = (prob_density, "Normal", lambda_expression(normal_density()), 0);

expression_ref log_normal_dist  = (prob_density, "LogNormal", lambda_expression(log_normal_density()), lambda_expression(log_normal_quantile_op()));

expression_ref cauchy_dist      = (prob_density, "Cauchy", lambda_expression(cauchy_density()), 0);

expression_ref dirichlet_dist   = (prob_density, "Dirichlet", lambda_expression(dirichlet_density()), 0);

expression_ref laplace_dist     = (prob_density, "Laplace", lambda_expression(laplace_density()), 0);

expression_ref log_laplace_dist = (prob_density, "LogLaplace", lambda_expression(log_laplace_density()), 0);

expression_ref uniform_dist     = (prob_density, "Uniform", lambda_expression(uniform_density()), 0);

