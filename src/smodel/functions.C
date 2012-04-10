#include "computation/prelude.H"
#include "computation/operations.H"
#include "smodel/functions.H"
#include "computation/graph_register.H"
#include "smodel/operations.H"

const expression_ref MultiParameter = var("MultiParameter");
const expression_ref rate = var("rate");
const expression_ref scale = var("scale");
const expression_ref QExp = var("QExp");
const expression_ref Q_from_S_and_R = var("Q_from_S_and_R");

const expression_ref n_base_models = var("n_base_models");
const expression_ref state_letters = var("state_letters");
const expression_ref n_states = var("n_states");
const expression_ref get_alphabet = var("get_alphabet");
const expression_ref get_frequencies = var("frequencies");
const expression_ref get_component_frequencies = var("component_frequencies");
const expression_ref base_model = var("base_model");
const expression_ref distribution = var("distribution");
const expression_ref MultiRate = var("MultiRate");

// (ReversibleMarkov alpha state_letters q pi l t)
const expression_ref ReversibleMarkov = lambda_expression( constructor("ReversibleMarkov", 6) );

// (ReversibleFrequency alpha state_letters pi R)
const expression_ref ReversibleFrequency = lambda_expression( constructor("ReversibleMarkov", 4) );

// (F81 alpha state_letters a pi)
const expression_ref F81M = lambda_expression( constructor("F81", 4) );

// (MixtureModel (DiscreteDistribution [(Double,RMM|F81)]))
const expression_ref MixtureModel = lambda_expression( constructor("MixtureModel", 1) );

// TODO: transition_p
// After we get transition_p right, then it SHOULD be fast.  Benchmark!
// TODO: convert ReversibleFrequencyModel and AlphabetExchangeModel to H.
//       This will allow expressing the MarkovModel as an object with a thunk to get_eigensystem.

Program SModel_Functions()
{
  Program P;

  expression_ref times = lambda_expression(Multiply<Double>());
  expression_ref plus = lambda_expression( Add<Double>() );
  expression_ref plus_i = lambda_expression( Add<Int>() );

  typed_expression_ref<Double> x1 ( v1 );

  // MultiParameter f (DiscreteDistribution d) = MixtureModel(DiscreteDistribution (fmap2 f d))
  P += Def( (MultiParameter,v1,(DiscreteDistribution,v2)), (MixtureModel,(DiscreteDistribution,(fmap2,v1,v2))));

  // MultiRate m D = MultiParameter \x.(scale x m) D
  P += Def( (MultiRate,v1,v2), (MultiParameter,v3^(scale,v3,v1), v2) );

  // scale x (ReversibleMarkov a s q pi l t) = (ReversibleMarkov a s q p l (x * t))
  // scale x (F81 a s a' pi)= (F81 a s a'*x pi) ??
  // scale x (MixtureModel (DiscreteDistribution l)) s= (MixtureModel (DiscreteDistribution (fmap2,scale(s),l))) ??
  P += Def( (scale,v0,(ReversibleMarkov,v1,v2,v3,v4,v5,v6)),(ReversibleMarkov,v1,v2,v3,v4,v5,(times,v0,v6)) );

  // rate (ReversibleMarkovModel a smap q pi l t) = t*(get_equilibrium_rate a smap q pi)
  // rate (MixtureModel (DiscreteDistribution l) ) = average (fmap2 rate l)
  P += Def( (rate,(ReversibleMarkov,v1,v2,v3,v4,v5,v6)),(times,v6,(substitution::Get_Equilibrium_Rate,v1,v2,v3,v4) ) )
          ( (rate,(MixtureModel, v1) ), (average,(fmap2, rate, v1) ) );
     
  // QExp (ReversibleMarkov a smap q pi l t) = (LExp l pi t)
  P += Def( (QExp, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), (LExp,v5,v4,v6));

  // Q_from_S_and_R s (ReversibleFrequency a smap pi R) = ReversibleMarkov a smap (Q S R) pi 0 1.0
  P += Def( (Q_from_S_and_R, v1, (ReversibleFrequency, v2, v3, v4, v5) ), 
	    let_expression(v6,(substitution::Q,v1,v5),(ReversibleMarkov, v2, v3, v6, v4, (substitution::Get_Eigensystem,v6,v4), 1.0) ) );

  // n_base_models (MixtureModel a state_letters (DiscreteDistribution l)) = length l
  P += Def( (n_base_models, (MixtureModel,(DiscreteDistribution,v1))), (length,v1));
  
  // state_letters (ReversibleMarkov alpha smap q pi l t) = smap
  // state_letters (F81 alpha s a pi) = s
  // state_letters (MixtureModel alpha s d) = state_letters (base_model (MixtureModel alpha s d) 0)
  P += Def( (state_letters, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), v2)
          ( (state_letters, (F81M,v1,v2,v3,v4)), v2)
          ( (state_letters, (MixtureModel,v1)), (state_letters,(base_model,(MixtureModel,v1),0)));

  // n_states m = vector_size (state_letters m)
  P += Def( (n_states,v1), (VectorSize<unsigned>(),(state_letters,v1)));
  
  // get_alphabet (ReversibleMarkov a smap q pi l t) = alpha
  // get_alphabet (F81 alpha s a pi) = alpha
  // get_alphabet (MixtureModel alpha s d) = alpha
  P += Def( (get_alphabet, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), v1)
          ( (get_alphabet, (F81M,v1,v2,v3,v4)), v1)
          ( (get_alphabet, (MixtureModel,v1)), (get_alphabet,(base_model,(MixtureModel,v1),0)));

  // get_frequencies (ReversibleMarkov alpha s q) = get_frequencies q
  // get_frequencies (F81 alpha s a pi) = pi
  P += Def( (get_frequencies, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), v4)
          ( (get_frequencies, (F81M,v1,v2,v3,v4)), v4);

  // get_component_frequencies (MixtureModel alpha s d)  i = get_frequencies (base_model (MixtureModel alpha s d) i)
  P += Def( (get_component_frequencies, (MixtureModel,v1), v4), (get_frequencies,(base_model,(MixtureModel,v1),v4)));

  // base_model (MixtureModel alpha s (DiscreteDistribution l)) i = get_list_index l i
  P += Def( (base_model, (MixtureModel,(DiscreteDistribution,v1)),v2), (snd,(get_list_index,v1,v2)));

  // distribution (MixtureModel alpha s (DiscreteDistribution l)) = fmap fst l
  P += Def( (distribution, (MixtureModel,(DiscreteDistribution,v1))), (fmap,fst,v1));

  return P;
}


/*

 *
 * OK, so Q_from_R_and_S should take an "S" and a (ReversibleFrequencyModelObject a smap R pi) and return:
 *
 *     let q = (Q,S,R) in ReversibleMarkovModelObject a smap Q pi (get_eigensystem Q pi) 1.0
 *
 * rate (ReversibleMarkov a smap Q pi lambda t) = (Get_Equilibrium_Rate a smap Q pi)
 *
 * QExp (ReversibleMarkov a smap Q pi lambda t) = (LExp lambda pi t)
 *
 * rate (MixtureModel (DiscreteDistribution (p,m):t) ) = p*(rate m)+(rate MixtureModel (DiscreteDistribution t) )
 */
