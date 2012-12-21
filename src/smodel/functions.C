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
  P.import_module(module_root_paths,"Prelude",false);

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
  P += "{scale x (ReversibleMarkov a s q pi l t r) = ReversibleMarkov a s q pi l (x*t) (x*r)}";

  P += "{multiParameter f (DiscreteDistribution d) = MixtureModel (DiscreteDistribution (fmap2 f d))}";

  P += "{multiRate m d = multiParameter (\\x->(scale x m)) d}";

  P += "{rate (ReversibleMarkov a s q pi l t r) = r;\
         rate (MixtureModel d) = average (fmap2 rate d)}";
     
  P += "{qExp (ReversibleMarkov a s q pi l t r) = lExp l pi t}";

  P += "{branchTransitionP (MixtureModel (DiscreteDistribution l)) t = let {r = rate (MixtureModel (DiscreteDistribution l))} in map (\\x -> qExp (scale (t/r) (snd x))) l}";

  P += "{qFromSandR s (ReversibleFrequency a smap pi r) = let {q = getQ s r} in ReversibleMarkov a smap q pi (getEigensystem q pi) 1.0 (getEquilibriumRate a smap q pi)}";

  P += "{nBaseModels (MixtureModel (DiscreteDistribution l)) = length l;\
         nBaseModels (MixtureModels (m:ms)) = nBaseModels m}";

  P += "{baseModel (MixtureModel (DiscreteDistribution l)) i = snd (l !! i)}";

  P += "{stateLetters (ReversibleMarkov _ smap _ _ _ _ _) = smap;\
         stateLetters (F81 _ smap _ _ ) = smap;\
         stateLetters (MixtureModel l) = stateLetters (baseModel (MixtureModel l) 0);\
         stateLetters (MixtureModels (m:ms)) = stateLetters m}";

  P += "{nStates m = sizeOfVectorUnsigned (stateLetters m)}";
  
  P += "{getAlphabet (ReversibleMarkov a _ _ _ _ _ _) = a;\
         getAlphabet (F81 a _ _ _) = a;\
         getAlphabet (MixtureModel l) = getAlphabet (baseModel (MixtureModel l) 0);\
         getAlphabet (MixtureModels (m:ms)) = getAlphabet m}";

  P += "{frequencies (ReversibleMarkov _ _ _ pi _ _ _) = pi;\
         frequencies (F81 _ _ _ pi) = pi}";

  P += "{componentFrequencies (MixtureModel d)       i = frequencies (baseModel (MixtureModel d) i);\
         componentFrequencies (MixtureModels (m:ms)) i = componentFrequencies m i}";

  P += "{distribution (MixtureModel (DiscreteDistribution l)) = map fst l;\
         distribution (MixtureModels (m:ms))                  = distribution m}";

  P += "{getNthMixture (MixtureModels l) i = l !! i}";

  P += "{unwrapMM (MixtureModel dd) = dd}";

  P += "{mixMixtureModels l dd = MixtureModel (mixDiscreteDistributions l (map unwrapMM dd))}";

  return P;
}
