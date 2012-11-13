#include "probability/distribution-operations.H"
#include "computation/prelude.H"

using std::vector;
using std::valarray;

closure exponential_density::operator()(OperationArgs& Args) const
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(x,mu);
  return object_ptr<const Object>(result.clone());
}

closure log_exponential_density::operator()(OperationArgs& Args) const
{
  double mu = *Args.evaluate_as<Double>(0);
  double x = *Args.evaluate_as<Double>(1);
  
  Log_Double result = exponential_pdf(log(x),mu)/x;
  return object_ptr<const Object>(result.clone());
}

closure gamma_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure gamma_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = gamma_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure log_gamma_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = gamma_pdf(log(x), a1, a2)/x;
  return object_ptr<const Object>(result.clone());
}

closure beta_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = beta_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure beta_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);
  
  Double result = beta_quantile(p, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure normal_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  Log_Double result = normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure log_normal_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  Log_Double result = log_normal_pdf(x, a1, a2);
  return object_ptr<const Object>(result.clone());
}

closure log_normal_quantile_op::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double p  = *Args.evaluate_as<Double>(2);

  Double result = log_normal_quantile(p, a1 ,a2);
  return object_ptr<const Object>(result.clone());
}

closure cauchy_density::operator()(OperationArgs& Args) const
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
closure dirichlet_density::operator()(OperationArgs& Args) const
{
  const vector<double>& n = Args.evaluate_as<Vector<double>>(0)->t;
  const vector<double>& x = Args.evaluate_as<Vector<double>>(1)->t;
  
  object_ptr<Log_Double> R (new Log_Double( ::dirichlet_pdf(x,n) ) );
  
  return R;
}

closure laplace_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(x, a1, a2) ) );
}

closure log_laplace_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);

  return object_ptr<Log_Double> (new Log_Double( ::laplace_pdf(log(x),a1,a2)/x ) );
}

closure epsilon_density::operator()(OperationArgs& Args) const
{
  double E_length_mean = *Args.evaluate_as<Double>(0);
  double log_epsilon  = *Args.evaluate_as<Double>(1);
  double E_length = log_epsilon - logdiff(0,log_epsilon);

  return object_ptr<Log_Double> (new Log_Double( exp_exponential_pdf(E_length,E_length_mean) ) );
}

closure uniform_density::operator()(OperationArgs& Args) const
{
  double a1 = *Args.evaluate_as<Double>(0);
  double a2 = *Args.evaluate_as<Double>(1);
  double x  = *Args.evaluate_as<Double>(2);
  
  return object_ptr<Log_Double> (new Log_Double( ::uniform_pdf(x,a1,a2) ) );
}

closure bernoulli_prob::operator()(OperationArgs& Args) const
{
  double p = *Args.evaluate_as<Double>(0);
  bool b = *Args.evaluate_as<Bool>(1);

  if (not b)
    p = 1.0 - p;

  return object_ptr<Log_Double> (new Log_Double( p ) );
}

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( constructor("ProbDensity",3) );

expression_ref exponentialDensity = var("exponentialDensity");
expression_ref exponential_dist = (prob_density, "Exponential", exponentialDensity, 0);

expression_ref logExponentialDensity = var("logExponentialDensity");
expression_ref log_exponential_dist = (prob_density, "LogExponential", logExponentialDensity, 0);

expression_ref gammaDensity  = var("gammaDensity");
expression_ref gammaQuantile = var("gammaQuantile");
expression_ref gamma_dist       = (prob_density, "Gamma", gammaDensity, gammaQuantile);

expression_ref logGammaDensity = var("logGammaDensity");
expression_ref log_gamma_dist   = (prob_density, "LogGamma", logGammaDensity, 0);

expression_ref betaDensity     = var("betaDensity");
expression_ref betaQuantile    = var("betaQuantile");
expression_ref beta_dist        = (prob_density, "Beta", betaDensity, betaQuantile);

expression_ref normalDensity     = var("normalDensity");
expression_ref normal_dist      = (prob_density, "Normal", normalDensity, 0);

expression_ref logNormalDensity     = var("logNormalDensity");
expression_ref logNormalQuantile    = var("logNormalQuantile");
expression_ref log_normal_dist  = (prob_density, "LogNormal", logNormalDensity, logNormalQuantile);

expression_ref cauchyDensity    = var("cauchyDensity");
expression_ref cauchy_dist      = (prob_density, "Cauchy", cauchyDensity, 0);

expression_ref dirichletDensity   = var("dirichletDensity");
expression_ref dirichlet_dist   = (prob_density, "Dirichlet", dirichletDensity, 0);

expression_ref laplaceDensity   = var("laplaceDensity");
expression_ref laplace_dist     = (prob_density, "Laplace", laplaceDensity, 0);

expression_ref logLaplaceDensity = var("logLaplaceDensity");
expression_ref log_laplace_dist = (prob_density, "LogLaplace", logLaplaceDensity, 0);

expression_ref uniformDensity   = var("uniformDensity");
expression_ref uniform_dist     = (prob_density, "Uniform", uniformDensity, 0);

expression_ref bernoulliProb    = var("bernoulliProb");
expression_ref bernoulli_dist   = (prob_density, "Bernoulli", bernoulliProb, 0);


Program Distribution_Functions()
{
  Program P("Distributions");

  // Note: we separate the "builtin" versions (which don't do case analysis on their arguments)
  //       from the from the real versions (which do).

  P.def_function("exponentialDensity", 2, lambda_expression( exponential_density() ) );
  P.def_function("logExponentialDensity", 2, lambda_expression( log_exponential_density() ) );

  P.def_function("builtinGammaDensity", 3, lambda_expression( gamma_density() ) );
  P += Def( (gammaDensity, Tuple(v1,v2), v3), (var("builtinGammaDensity"),v1,v2,v3));
  P.def_function("builtinGammaQuantile", 3, lambda_expression( gamma_quantile_op() ) );
  P += Def( (gammaQuantile, Tuple(v1,v2), v3), (var("builtinGammaQuantile"),v1,v2,v3));

  P.def_function("builtinLogGammaDensity", 3, lambda_expression( log_gamma_density() ) );
  P += Def( (logGammaDensity, Tuple(v1,v2), v3), (var("builtinLogGammaDensity"),v1,v2,v3));

  P.def_function("builtinBetaDensity", 3, lambda_expression( beta_density() ) );
  P += Def( (betaDensity, Tuple(v1,v2), v3), (var("builtinBetaDensity"),v1,v2,v3)); 
  P.def_function("builtinBetaQuantile", 3, lambda_expression( beta_quantile_op() ) );
  P += Def( (betaQuantile, Tuple(v1,v2), v3), (var("builtinBetaQuantile"),v1,v2,v3));

  P.def_function("builtinNormalDensity", 3, lambda_expression( normal_density() ) );
  P += Def( (normalDensity, Tuple(v1,v2), v3), (var("builtinNormalDensity"),v1,v2,v3)); 

  P.def_function("builtinLogNormalDensity", 3, lambda_expression( log_normal_density() ) );
  P += Def( (logNormalDensity, Tuple(v1,v2), v3), (var("builtinLogNormalDensity"),v1,v2,v3)); 
  P.def_function("builtinLogNormalQuantile", 3, lambda_expression( log_normal_quantile_op() ) );
  P += Def( (logNormalQuantile, Tuple(v1,v2), v3), (var("builtinLogNormalQuantile"),v1,v2,v3));

  P.def_function("builtinCauchyDensity", 3, lambda_expression( cauchy_density() ) );
  P += Def( (cauchyDensity, Tuple(v1,v2), v3), (var("builtinCauchyDensity"),v1,v2,v3)); 

  P.def_function("builtinLaplaceDensity", 3, lambda_expression( laplace_density() ) );
  P += Def( (laplaceDensity, Tuple(v1,v2), v3), (var("builtinLaplaceDensity"),v1,v2,v3)); 

  P.def_function("builtinDirichletDensity", 3, lambda_expression( dirichlet_density() ) );
  P += Def( (dirichletDensity, v1, v2), (var("builtinDirichletDensity"),(var("listToVectorDouble"),v1),(var("listToVectorDouble"),v2))); 

  P.def_function("builtinLogLaplaceDensity", 3, lambda_expression( log_laplace_density() ) );
  P += Def( (logLaplaceDensity, Tuple(v1,v2), v3), (var("builtinLogLaplaceDensity"),v1,v2,v3)); 

  P.def_function("builtinUniformDensity", 3, lambda_expression( uniform_density() ) );
  P += Def( (uniformDensity, Tuple(v1,v2), v3), (var("builtinUniformDensity"),v1,v2,v3)); 

  P.def_function("epsilonDensity", 2, lambda_expression( epsilon_density() ) );
  P.def_function("epsilonDist", 0, (prob_density,"epsilonDist", var("epsilonDensity"),0));

  P.def_function("bernoulliProb", 2, lambda_expression( bernoulli_prob() ) );

  return P;
}
