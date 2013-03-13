#include "computation/module.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include "smodel/operations.H"

using std::vector;
using std::string;

void SModel_Functions(Module& P)
{
  P.def_function("getEquilibriumRate", lambda_expression(substitution::Get_Equilibrium_Rate_Op()));
  P.def_function("singletToTripletExchange", lambda_expression( substitution::Singlet_to_Triplet_Exchange_Op()));

  // scale x (F81 a s a' pi)= (F81 a s a'*x pi) ??
  // scale x (MixtureModel (DiscreteDistribution l)) s= (MixtureModel (DiscreteDistribution (fmap2,scale(s),l))) ??
}
