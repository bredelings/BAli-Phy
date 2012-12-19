#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "smodel/functions.H"
#include "computation/graph_register.H"
#include "smodel/operations.H"

const expression_ref MultiParameter = var("multiParameter");
const expression_ref rate = var("rate");
const expression_ref scale = var("scale");
const expression_ref QExp = var("qExp");
const expression_ref Q_from_S_and_R = var("qFromSandR");
const expression_ref branch_transition_p = var("branchTransitionP");

const expression_ref n_base_models = var("nBaseModels");
const expression_ref state_letters = var("stateLetters");
const expression_ref n_states = var("nStates");
const expression_ref get_alphabet = var("getAlphabet");
const expression_ref get_frequencies = var("frequencies");
const expression_ref get_component_frequencies = var("componentFrequencies");
const expression_ref base_model = var("baseModel");
const expression_ref distribution = var("distribution");
const expression_ref MultiRate = var("multiRate");
const expression_ref get_nth_mixture = var("getNthMixture");
const expression_ref UnwrapMM = var("unwrapMM");
const expression_ref MixMixtureModels = var("mixMixtureModels");

// (ReversibleMarkov alpha state_letters q pi l t)
const expression_ref ReversibleMarkov = lambda_expression( constructor("SModel.ReversibleMarkov", 7) );

// (ReversibleFrequency alpha state_letters pi R)
const expression_ref ReversibleFrequency = lambda_expression( constructor("SModel.ReversibleFrequency", 4) );

// (F81 alpha state_letters a pi)
const expression_ref F81M = lambda_expression( constructor("SModel.F81", 4) );

// (MixtureModel (DiscreteDistribution [(Double,RMM|F81)]))
const expression_ref MixtureModel = lambda_expression( constructor("SModel.MixtureModel", 1) );

// (MixtureModels [MixtureModel])
const expression_ref MixtureModels = lambda_expression( constructor("SModel.MixtureModels", 1) );

// TODO: transition_p
// After we get transition_p right, then it SHOULD be fast.  Benchmark!
//
const expression_ref DiscreteDistribution = lambda_expression(constructor("Prelude.DiscreteDistribution",1));

Program SModel_Functions()
{
  Program P("SModel");
  P.import_module(get_Prelude(),"Prelude",false);

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

  // distribution (MixtureModel alpha s (DiscreteDistribution l)) = fmap fst l
  // distribution (MixtureModels h:t) = distribution h
  P += Def( (distribution, (MixtureModel,(DiscreteDistribution,v1))), (var("fmap"),var("fst"),v1))
          ( (distribution, (MixtureModels,v1&v2)), (distribution, v1) );

  // get_nth_mixture (MixtureModels l) b = l !! b
  P += Def( (get_nth_mixture, (MixtureModels, v1), v2), (var("!!"),v1,v2) );

  // UnwrapMM (MixtureModel dd) = dd
  P += Def( (UnwrapMM, (MixtureModel, v0)), v0 );

  // MixMixtureModels l dd = MixtureModel (MixDiscreteDistributions l (fmap UnwrapMM dd))
  P += Def( (var("mixMixtureModels"), v0, v1), (var("MixtureModel"), (var("mixDiscreteDistributions"), v0, (var("fmap"), var("unwrapMM"), v1) ) ) );

  return P;
}
