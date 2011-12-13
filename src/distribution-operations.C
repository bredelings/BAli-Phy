#include "distribution-operations.H"

using boost::shared_ptr;

shared_ptr<const Object> laplace_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  shared_ptr<const expression> a_E = Args.evaluate_as<expression>(1);
  
  // Idea: we could define this conversion INSIDE the machine...
  std::vector<double> a(a_E->size() - 1);
  for(int i=0;i<a.size();i++)
    a[i] = *convert<const Double>(Args.evaluate_expression(a_E->sub[i+1]));
  
  return shared_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x,a[0],a[1]) ) );
}

shared_ptr<const Object> log_laplace_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  shared_ptr<const expression> a_E = Args.evaluate_as<expression>(1);
  
  // Idea: we could define this conversion INSIDE the machine...
  std::vector<double> a(a_E->size() - 1);
  for(int i=0;i<a.size();i++)
    a[i] = *convert<const Double>(Args.evaluate_expression(a_E->sub[i+1]));
  
  return shared_ptr<Log_Double> (new Log_Double( ::laplace_pdf(log(x),a[0],a[1])/x ) );
}

shared_ptr<const Object> uniform_density::operator()(OperationArgs& Args) const
{
  double x = *Args.evaluate_as<Double>(0);
  shared_ptr<const expression> a_E = Args.evaluate_as<expression>(1);
  
  // Idea: we could define this conversion INSIDE the machine...
  std::vector<double> a(a_E->size() - 1);
  for(int i=0;i<a.size();i++)
    a[i] = *convert<const Double>(Args.evaluate_expression(a_E->sub[i+1]));
  
  return shared_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a[0],a[1]) ) );
}

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( data_function("prob_density",2) );

expression_ref exponential_dist = prob_density("Exponential", exponential_density());

expression_ref gamma_dist       = prob_density("Gamma", gamma_density());

expression_ref beta_dist        = prob_density("Beta", beta_density());

expression_ref normal_dist      = prob_density("Normal", normal_density());

expression_ref log_normal_dist  = prob_density("LogNormal", log_normal_density());

expression_ref cauchy_dist      = prob_density("Cauchy", cauchy_density());

expression_ref dirichlet_dist   = prob_density("Dirichlet", dirichlet_density());

expression_ref laplace_dist     = prob_density("Laplace", laplace_density());

expression_ref log_laplace_dist = prob_density("LogLaplace", log_laplace_density());

expression_ref uniform_dist     = prob_density("Uniform", uniform_density());

