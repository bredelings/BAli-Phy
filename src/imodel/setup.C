#include "setup.H"
#include "imodel/imodel.H"
#include "probability/distribution-operations.H"
#include "computation/operations.H"

using std::string;
using std::vector;

// FIXME - change to return a (model,standardized name) pair.

/// Return an owned_ptr (possibly NULL) to an IndelModel or type \a name.
formula_expression_ref get_imodel(string name, const SequenceTree& T) 
{
  // FIXME: How to change the formula_ref expression to a parseable haskell string?
  //        

  //-------------Choose an indel model--------------//
  formula_expression_ref imodel;

  // Default
  if (name == "") 
    throw myexception()<<"Indel model name is empty! (A default should have been automatically set.)";

  if (name == "none")
    { }
  else if (name == "RS05")
    ;
  else if (name == "RS07-no-T")
    ;
  else if (name == "RS07")
  {
    expression_ref RS07BranchHMM = lambda_expression( RS07_branch_HMM() );
    expression_ref lengthp = lambda_expression( RS07_lengthp() );

    expression_ref log_lambda = def_parameter(imodel, "logLambda", -4.0, nullptr, laplace_dist, Tuple(-4.0, 1.0));
    expression_ref meanIndelLengthMinus1 = def_parameter(imodel, "meanIndelLengthMinus1", 1.0, lower_bound(0),exponential_dist, 10.0);

    expression_ref epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
    expression_ref lambda = (var("exp"), log_lambda);
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("IModels.training");

    imodel.set_exp( Tuple(v1^(v2^(RS07BranchHMM, epsilon, lambda*(var("!"),v1,v2), heat, training)), 
			  v1^(lengthp,epsilon,v1)) );
  }
  else if (name == "fg_branch_scale[RS07]")
  {
    expression_ref RS07BranchHMM = lambda_expression( RS07_branch_HMM() );
    expression_ref lengthp = lambda_expression( RS07_lengthp() );

    expression_ref log_lambda = def_parameter(imodel, "logLambda", -4.0, nullptr, laplace_dist, Tuple(-4.0, 1.0));
    expression_ref meanIndelLengthMinus1 = def_parameter(imodel, "meanIndelLengthMinus1", 1.0, lower_bound(0),exponential_dist, 10.0);

    expression_ref log_lambda_scale = def_parameter(imodel, "logLambdaScale", 0.0, nullptr, laplace_dist, Tuple(0.0, 1.0));
    expression_ref lambda_scale_on = def_parameter(imodel, "lambdaScaleOn", false, nullptr, (var("bernoulli"), 0.5));
    // FIXME!  We need a discrete uniform
    expression_ref lambda_scale_branch = def_parameter(imodel, "lambdaScaleBranch", -1, between(0,T.n_branches()));

    expression_ref epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
    expression_ref lambda = (var("exp"), log_lambda);
    expression_ref lambda_scale = (var("exp"), log_lambda_scale);
    expression_ref rate = (var("If"), (var("=="),v2,lambda_scale_branch),lambda*lambda_scale,lambda);
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("IModels.training");

    imodel.set_exp( Tuple(v1^(v2^(RS07BranchHMM, epsilon, rate*(var("!"),v1,v2), heat, training)), 
			  v1^(lengthp,epsilon,v1)) );
  }
  else if (name == "relaxed_rates1[RS07]")
  {
    /*
     * DefineBuiltin RS07BranchHMM
     * DefineBuiltin lengthp
     * 
     * meanIndelLenthMinus1 ~ Exponential(10.0)
     * lambdaSigmaOverMu ~ LogLaplace(-3.0, 1.0)
     * lambdaScale{$i} ~ Gamma(1.0/(LambdaSigmaOverMu*LambdaSigmaOverMu), LambdaSigmaOverMu*LambdaSigmaOverMu)
     * logLambdaMean ~ Laplace(-4.0, 1.0)
     * epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1)
     * main = let {lambdaMean = exp logLambdaMean, lambdaScales = listArray' lambdasList} in 
     *            (\a b -> RS07BranchMHH epsilon (lambdaMean*(lambdaScales!b))*(a!b) Heat.beta IModels.training, 
                   \a -> lengthp epsilon a)
     */

    expression_ref RS07BranchHMM = lambda_expression( RS07_branch_HMM() );
    expression_ref lengthp = lambda_expression( RS07_lengthp() );
    expression_ref meanIndelLengthMinus1 = def_parameter(imodel, "meanIndelLengthMinus1", 1.0, lower_bound(0),exponential_dist, 10.0);
    expression_ref epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);

    expression_ref lambda_sigma_over_mu = def_parameter(imodel,"lambdaSigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    expression_ref B = lambda_sigma_over_mu*lambda_sigma_over_mu;
    expression_ref A = 1.0/B;

    vector<expression_ref> branch_lambdas;
    for(int b=0;b<T.n_branches();b++)
      branch_lambdas.push_back(def_parameter(imodel, "lambdaScale"+convertToString(b), 1.0, nullptr, gamma_dist, Tuple(A, B)));
    expression_ref lambdas_list = get_list(branch_lambdas);

    expression_ref log_lambda_mean = def_parameter(imodel, "logLambdaMean", -4.0, nullptr, laplace_dist, Tuple(-4.0, 1.0));

    expression_ref lambda_mean = (var("exp"), log_lambda_mean);
    
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("IModels.training");

    // let lambdaMean = exp logLambdaMean, lambdaScales = arrayFromList lambdasList .... lambdaMean*lambdasList!b

    imodel.set_exp( let_expression(v3, (var("exp"), log_lambda_mean),
				   let_expression(v4, (var("listArray'"), lambdas_list),
						  Tuple(v1^(v2^(RS07BranchHMM, epsilon, v3*(var("!"),v4,v2)*(var("!"),v1,v2), heat, training)), 
							v1^(lengthp,epsilon,v1))
						  )
				   )
		    );
  }
  else if (name == "relaxed_rates[RS07]")
  {
    /*
     * DefineBuiltin RS07BranchHMM
     * DefineBuiltin lengthp
     * 
     * meanIndelLenthMinus1 ~ Exponential(10.0)
     * lambdaSigmaOverMu ~ LogLaplace(-3.0, 1.0)
     * lambdaScale{$i} ~ Gamma(1.0/(LambdaSigmaOverMu*LambdaSigmaOverMu), LambdaSigmaOverMu*LambdaSigmaOverMu)
     * logLambdaMean ~ Laplace(-4.0, 1.0)
     * epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1)
     * main = let {lambdaMean = exp logLambdaMean, lambdaScales = listArray' lambdasList} in 
     *            (\a b -> RS07BranchMHH epsilon (lambdaMean*(lambdaScales!b))*(a!b) Heat.beta IModels.training, 
                   \a -> lengthp epsilon a)

     */

    /*
     * iidDensity ((probDensity _ density _),args) l = foldl' (*) 1.0 (fmap (density args) l)
     *
     * iid (dist,args) = ((probDensity "i.i.d." (iidDensity dist args) (error "idd has no quantile")), () )
     *
     * logLambdaMeans = [logLambdaMean1, logLambdaMean2, logLambdaMean3, logLambdaMean4]
     *
     * logLambdaMean ~ Laplace(-4.0, 1.0)
     * logLambdaMeans ~ iid ( normal (logLambdaMean, sigmaBetweenGroup))
     *
     * a = 10.0
     * q1 ~ Beta(a, 1.0)
     * q2 ~ Beta(a, 1.0)
     * q3 ~ Beta(a, 1.0)
     *
     * p1 = q1
     * p2 = (1.0-q1)*q2
     * p3 = (1.0-q1)*(1.0-q2)*q3
     * p4 = (1.0-q1)*(1.0-q2)*(1.0-q3)
     *
     * dists = fmap (\x -> Normal(x, sigmaWithinGroup)) [logLambdaMean1, logLambdaMean2, logLambdaMean3, logLambdaMean4]
     * weights = [p1, p2, p3, p4]
     *
     * logLambdasList = logLambda1:logLambda2:logLambda3:logLambda4 .... logLambda{$B}
     *
     * logLambdasList ~ iid Mixture( zip weights dists )
     *
     * main = let {lambdaMean = exp logLambdaMean, logLambdaScales = listArray' logLambdasList} in 
     *            (\a b -> RS07BranchMHH epsilon (exp logLambdaScales!b))*(a!b) Heat.beta IModels.training, 
                   \a -> lengthp epsilon a)
     *
     */
    expression_ref RS07BranchHMM = lambda_expression( RS07_branch_HMM() );
    expression_ref lengthp = lambda_expression( RS07_lengthp() );
    expression_ref meanIndelLengthMinus1 = def_parameter(imodel, "meanIndelLengthMinus1", 1.0, lower_bound(0),exponential_dist, 10.0);
    expression_ref epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);

    expression_ref lambda_sigma_over_mu = def_parameter(imodel,"lambdaSigmaOverMu", 0.1, lower_bound(0), log_laplace_dist, Tuple(-3.0, 1.0) );
    expression_ref B = lambda_sigma_over_mu*lambda_sigma_over_mu;
    expression_ref A = 1.0/B;

    vector<expression_ref> branch_lambdas;
    for(int b=0;b<T.n_branches();b++)
      branch_lambdas.push_back(def_parameter(imodel, "lambdaScale"+convertToString(b), 1.0, nullptr, gamma_dist, Tuple(A, B)));
    expression_ref lambdas_list = get_list(branch_lambdas);

    expression_ref log_lambda_mean = def_parameter(imodel, "logLambdaMean", -4.0, nullptr, laplace_dist, Tuple(-4.0, 1.0));

    expression_ref lambda_mean = (var("exp"), log_lambda_mean);
    
    expression_ref heat = parameter("Heat.beta");
    expression_ref training = parameter("IModels.training");

    // let lambdaMean = exp logLambdaMean, lambdaScales = arrayFromList lambdasList .... lambdaMean*lambdasList!b

    imodel.set_exp( let_expression(v3, (var("exp"), log_lambda_mean),
				   let_expression(v4, (var("listArray'"), lambdas_list),
						  Tuple(v1^(v2^(RS07BranchHMM, epsilon, v3*(var("!"),v4,v2)*(var("!"),v1,v2), heat, training)), 
							v1^(lengthp,epsilon,v1))
						  )
				   )
		    );
  }
  else
    throw myexception()<<"Unrecognized indel model '"<<name<<"'";

  // Up the sampling rate for imodel parameters...
  expression_ref sampling_rate = lambda_expression(constructor("SamplingRate",2));

  std::set<string> declared_parameter_names = find_declared_parameters(imodel);
  for(const auto& parameter_name: declared_parameter_names)
    imodel.add_note( (sampling_rate, parameter(parameter_name), 10.0) );

  return imodel;
}

