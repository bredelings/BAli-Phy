#include "setup.H"
#include "imodel.H"
#include "distribution-operations.H"
#include "computation/operations.H"

using std::string;

// FIXME - change to return a (model,standardized name) pair.

/// Return an owned_ptr (possibly NULL) to an IndelModel or type \a name.
formula_expression_ref get_imodel(string name, const SequenceTree& T) 
{
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
  else
    throw myexception()<<"Unrecognized indel model '"<<name<<"'";

  // Up the sampling rate for imodel parameters...
  expression_ref sampling_rate = lambda_expression(constructor("SamplingRate",2));

  std::set<string> declared_parameter_names = find_declared_parameters(imodel);
  for(const auto& parameter_name: declared_parameter_names)
    imodel.add_note( (sampling_rate, parameter(parameter_name), 10.0) );

  return imodel;
}

