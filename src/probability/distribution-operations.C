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

// Fields: n_random, n_parameters, string, density op
expression_ref prob_density = lambda_expression( constructor("Distributions.ProbDensity",3) );

Program Distribution_Functions()
{
  Program P("Distributions");
  P.import_module(get_Prelude(),"Prelude",false);

  // Note: we separate the "builtin" versions (which don't do case analysis on their arguments)
  //       from the from the real versions (which do).

  P.def_constructor("ProbDensity",3);

  P.def_function("exponentialDensity", 2, lambda_expression( exponential_density() ) );
  P.def_function("logExponentialDensity", 2, lambda_expression( log_exponential_density() ) );

  P.def_function("builtinGammaDensity", 3, lambda_expression( gamma_density() ) );
  P.def_function("builtinGammaQuantile", 3, lambda_expression( gamma_quantile_op() ) );
  P += "{gammaDensity (a,b) x = builtinGammaDensity a b x}";
  P += "{gammaQuantile (a,b) p = builtinGammaQuantile a b p}";

  P.def_function("builtinLogGammaDensity", 3, lambda_expression( log_gamma_density() ) );
  P += "{logGammaDensity (a,b) x = builtinLogGammaDensity (a,b) x}";

  P.def_function("builtinBetaDensity", 3, lambda_expression( beta_density() ) );
  P.def_function("builtinBetaQuantile", 3, lambda_expression( beta_quantile_op() ) );
  P += "{betaDensity (a,b) x = builtinBetaDensity a b x}";
  P += "{betaQuantile (a,b) p = builtinBetaQuantile a b p}";

  P.def_function("builtinNormalDensity", 3, lambda_expression( normal_density() ) );
  P += "{normalDensity (mu,sigma) x =  builtinNormalDensity mu sigma x}";

  P.def_function("builtinLogNormalDensity", 3, lambda_expression( log_normal_density() ) );
  P.def_function("builtinLogNormalQuantile", 3, lambda_expression( log_normal_quantile_op() ) );
  P += "{logNormalDensity (mu,sigma) x = builtinLogNormalDensity mu sigma x}";
  P += "{logNormalQuantile (mu,sigma) x = builtinLogNormalQuantile mu sigma x}";

  P.def_function("builtinCauchyDensity", 3, lambda_expression( cauchy_density() ) );
  P += "{cauchyDensity (m,s) x = builtinCauchyDensity m s x}";

  P.def_function("builtinLaplaceDensity", 3, lambda_expression( laplace_density() ) );
  P += "{laplaceDensity (m,s) x = builtinLaplaceDensity m s x}";

  P.def_function("builtinDirichletDensity", 3, lambda_expression( dirichlet_density() ) );
  P += "{dirichletDensity ps xs = builtinDirichletDensity (listToVectorDouble ps) (listToVectorDouble xs)}";

  P.def_function("builtinLogLaplaceDensity", 3, lambda_expression( log_laplace_density() ) );
  P += "{logLaplaceDensity (m,s) x = builtinLogLaplaceDensity m s x}";

  P.def_function("builtinUniformDensity", 3, lambda_expression( uniform_density() ) );
  P += "{uniformDensity (min,max) x = builtinUniformDensity min max x}";

  P += "{exponentialQuantile mu p = gammaQuantile (1.0,mu) p}";

  P += "{mixtureDensity ((p1,((ProbDensity _ density1 _),args1)):l) x = (doubleToLogDouble p1)*(density1 args1 x)+(mixtureDensity l x);\
         mixtureDensity [] _ = (doubleToLogDouble 0.0)}";

  //------------ Define distribution objects --------------------//
  P += "{betaDist  =       (ProbDensity \"Beta\"        betaDensity        betaQuantile)}";

  P += "{bernoulliDensity p b = if b then (doubleToLogDouble p) else (doubleToLogDouble (1.0-p))}";
  P += "{bernoulli args = (ProbDensity \"Bernoulli\" bernoulliDensity (error \"Bernoulli has no quantile\"), args)}";
  P += "{normal args = (ProbDensity \"Normal\" normalDensity 0, args)}";
  P += "{exponential args = (ProbDensity \"Exponential\" exponentialDensity exponentialQuantile, args)}";
  P += "{gamma args = (ProbDensity \"Gamma\" gammaDensity gammaQuantile, args)}";
  P += "{betaD args = (ProbDensity \"Beta\"        betaDensity        betaQuantile, args)}";
  P += "{mixture args = (ProbDensity \"Mixture\" mixtureDensity 0, args)}";
  P += "{dirichlet args = (ProbDensity \"Dirichlet\" dirichletDensity (error \"Dirichlet has no quantiles\"), args)}";
  P += "{laplace args = (ProbDensity \"Laplace\" laplaceDensity 0, args)}";
  P += "{logLaplace args = (ProbDensity \"LogLaplace\" logLaplaceDensity 0, args)}";
  P += "{logExponential args = (ProbDensity \"LogExponential\" logExponentialDensity 0, args)}";
  P += "{logNormal args = (ProbDensity \"LogNormal\" logNormalDensity logNormalQuantile, args)}";
  P += "{logGamma args = (ProbDensity \"LogGamma\" logGammaDensity 0, args)}";
  P += "{uniform args = (ProbDensity \"Uniform\" uniformDensity 0, args)}";
  P += "{cauchy args = (ProbDensity \"Cauchy\" cauchyDensity 0, args)}";

  P += "{iidDensity (n,((ProbDensity _ density _),args)) xs = let {densities = (map (density args) xs) ; pr = foldl' (*) (doubleToLogDouble 1.0) densities} in if (length xs == n) then pr else (doubleToLogDouble 0.0)}";
  P += "{iid args = (ProbDensity \"i.i.d.\" iidDensity 0, args )}";

  P += "{\
plateDensity (n,f) xs = let {xs' = zip [1..] xs;\
                             densities = map (\\(i,x) -> case (f i) of {(ProbDensity _ d _, a) -> d a x}) xs';\
                             pr = foldl' (*) (doubleToLogDouble 1.0) densities}\
                        in if (length xs == n) then pr else (doubleToLogDouble 0.0)}";
  P += "{plate args = (ProbDensity \"Plate\" plateDensity 0, args )}";

  return P;
}
