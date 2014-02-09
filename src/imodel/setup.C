#include "imodel/setup.H"
#include "imodel/imodel.H"
#include "computation/module.H"

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
    return model_expression({identifier("rs07_model"),0});
  else if (name == "relaxed_rates_RS07")
    return model_expression({identifier("rs07_relaxed_rates_model"),0});
  else
    throw myexception()<<"Unrecognized indel model '"<<name<<"'";

  // Up the sampling rate for imodel parameters...
  expression_ref sampling_rate = lambda_expression(constructor("SamplingRate",2));

  std::set<string> declared_parameter_names = find_declared_parameters(imodel);
  for(const auto& parameter_name: declared_parameter_names)
    imodel.add_note( (sampling_rate * parameter(parameter_name) * 10.0) );

  return imodel;
}

