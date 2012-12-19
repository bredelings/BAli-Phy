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

  // branch_transition_p m@(MixtureModel (DiscreteDistribution l) ) t = list_to_vector (fmap \p->(QExp (scale (t/(rate m)) (snd p) ) ) l)
  P += Def( (branch_transition_p, (MixtureModel, (DiscreteDistribution, v3) ), v1 ),
	    (var("fmap"),v2^(QExp, (var("scale"), (v1/(rate, (MixtureModel, (DiscreteDistribution, v3) ) ) ), (var("snd"), v2) ) ), v3 ) );

  // Q_from_S_and_R s (ReversibleFrequency a smap pi R) = ReversibleMarkov a smap (Q S R) pi 0 1.0 (Get_Equilibrium_Rate a smap Q pi)
  P += Def( (Q_from_S_and_R, v1, (ReversibleFrequency, v2, v3, v4, v5) ), 
	      let_expression(v6,(substitution::Q,v1,v5),
			   (ReversibleMarkov, v2, v3, v6, v4, (substitution::Get_Eigensystem,v6,v4), 1.0,
			    (substitution::Get_Equilibrium_Rate, v2, v3, v6, v4)
			   )
	      )
	  );

  // n_base_models (MixtureModel a state_letters (DiscreteDistribution l)) = length l
  // n_base_models (MixtureModels h:t) = n_base_models h
  P += Def( (var("nBaseModels"), (MixtureModel,(DiscreteDistribution,v1))), (var("length"),v1))
          ( (var("nBaseModels"), (MixtureModels,v1&v2)), (var("nBaseModels"),v1) );

  // state_letters (ReversibleMarkov alpha smap q pi l t r) = smap
  // state_letters (F81 alpha s a pi) = s
  // state_letters (MixtureModel alpha s d) = state_letters (base_model (MixtureModel alpha s d) 0)
  // state_letters (MixtureModels h:t) = state_letters h
  P += Def( (state_letters, (ReversibleMarkov,v1,v2,v3,v4,v5,v6,v7)), v2)
          ( (state_letters, (F81M,v1,v2,v3,v4)), v2)
          ( (state_letters, (MixtureModel,v1)), (state_letters,(base_model,(MixtureModel,v1),0)))
          ( (state_letters, (MixtureModels,v1&v2)), (state_letters,v1) );

  // n_states m = vector_size (state_letters m)
  P += Def( (n_states,v1), (VectorSize<unsigned>(),(state_letters,v1)));
  
  // get_alphabet (ReversibleMarkov a smap q pi l t r) = alpha
  // get_alphabet (F81 alpha s a pi) = alpha
  // get_alphabet (MixtureModel alpha s d) = alpha
  // get_alphabet (MixtureModels h:t) = get_alphabet h
  P += Def( (get_alphabet, (ReversibleMarkov,v1,v2,v3,v4,v5,v6,v7)), v1)
          ( (get_alphabet, (F81M,v1,v2,v3,v4)), v1)
          ( (get_alphabet, (MixtureModel,v1)), (get_alphabet,(base_model,(MixtureModel,v1),0)))
          ( (get_alphabet, (MixtureModels,v1&v2)), (get_alphabet,v1) );

  // get_frequencies (ReversibleMarkov alpha s q) = get_frequencies q
  // get_frequencies (F81 alpha s a pi) = pi
  P += Def( (get_frequencies, (ReversibleMarkov,v1,v2,v3,v4,v5,v6,v7)), v4)
          ( (get_frequencies, (F81M,v1,v2,v3,v4)), v4);

  // get_component_frequencies (MixtureModel alpha s d)  i = get_frequencies (base_model (MixtureModel alpha s d) i)
  // get_component_frequencies (MixtureModels h:t)       i = get_component_frequencies h i
  P += Def( (get_component_frequencies, (MixtureModel,v1), v4), (get_frequencies,(base_model,(MixtureModel,v1),v4)))
          ( (get_component_frequencies, (MixtureModels,v1&v2), v4), (get_component_frequencies,v1,v4));

  // base_model (MixtureModel alpha s (DiscreteDistribution l)) i = get_list_index l i
  P += Def( (base_model, (MixtureModel,(DiscreteDistribution,v1)),v2), (var("snd"),(var("!!"),v1,v2)));

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
