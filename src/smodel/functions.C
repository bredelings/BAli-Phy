#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "smodel/functions.H"
#include "computation/graph_register.H"
#include "smodel/operations.H"

using std::vector;
using std::string;

Program SModel_Functions(const vector<string>& module_root_paths)
{
  Program P("SModel");

  P.def_function("plusGWF", lambda_expression(substitution::Plus_gwF_Op()));
  P.def_function("lExp", lambda_expression(LExp_Op()));
  P.def_function("getQ", lambda_expression(substitution::Q_Op()));
  P.def_function("getEigensystem", lambda_expression(substitution::Get_Eigensystem_Op()));
  P.def_function("getEquilibriumRate", lambda_expression(substitution::Get_Equilibrium_Rate_Op()));

  P.def_constructor("ReversibleMarkov",7);
  P.def_constructor("ReversibleFrequency",4);
  P.def_constructor("F81",4);
  P.def_constructor("MixtureModel",1);
  P.def_constructor("MixtureModels",1);

  // scale x (F81 a s a' pi)= (F81 a s a'*x pi) ??
  // scale x (MixtureModel (DiscreteDistribution l)) s= (MixtureModel (DiscreteDistribution (fmap2,scale(s),l))) ??
  return P;
}
