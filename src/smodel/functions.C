#include "computation/module.H"
#include "computation/operations.H"
#include "smodel/functions.H"
#include "computation/graph_register.H"
#include "smodel/operations.H"

using std::vector;
using std::string;

void SModel_Functions(Module& P)
{
  P.def_function("plusGWF", lambda_expression(substitution::Plus_gwF_Op()));
  P.def_function("lExp", lambda_expression(LExp_Op()));
  P.def_function("getQ", lambda_expression(substitution::Q_Op()));
  P.def_function("getEigensystem", lambda_expression(substitution::Get_Eigensystem_Op()));
  P.def_function("getEquilibriumRate", lambda_expression(substitution::Get_Equilibrium_Rate_Op()));
  P.def_function("hky", lambda_expression(substitution::HKY_Op()));
  P.def_function("tn", lambda_expression(substitution::TN_Op()));
  P.def_function("gtr", lambda_expression(substitution::GTR_Op()));
  P.def_function("m0", lambda_expression(substitution::M0_Op()));
  P.def_function("singletToTripletExchange", lambda_expression( substitution::Singlet_to_Triplet_Exchange_Op()));

  P.def_constructor("ReversibleMarkov",7);
  P.def_constructor("ReversibleFrequency",4);
  P.def_constructor("F81",4);
  P.def_constructor("MixtureModel",1);
  P.def_constructor("MixtureModels",1);

  // scale x (F81 a s a' pi)= (F81 a s a'*x pi) ??
  // scale x (MixtureModel (DiscreteDistribution l)) s= (MixtureModel (DiscreteDistribution (fmap2,scale(s),l))) ??
}
