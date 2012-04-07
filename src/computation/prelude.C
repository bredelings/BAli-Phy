#include "computation/prelude.H"
#include "computation/operations.H"
#include "computation/graph_register.H"

const expression_ref foldr = var("foldr");
const expression_ref foldl = var("foldl");
const expression_ref foldl_ = var("foldl'");
const expression_ref fmap = var("fmap");
const expression_ref fmap1 = var("fmap1");
const expression_ref fmap2 = var("fmap2");
const expression_ref take = var("take");
const expression_ref iterate = var("iterate");
const expression_ref sum_i = var("sum_i");
const expression_ref If = var("If");
const expression_ref ExtendDiscreteDistribution = var("ExtendDiscreteDistribution");
const expression_ref MultiParameter = var("MultiParameter");
const expression_ref fst = var("fst");
const expression_ref snd = var("snd");
const expression_ref get_list_index = var("!!");
const expression_ref listArray = var("listArray");
const expression_ref length = var("length");
const expression_ref scale = var("scale");
const expression_ref rate = var("rate");
const expression_ref QExp = var("QExp");

const expression_ref n_base_models = var("n_base_models");
const expression_ref state_letters = var("state_letters");
const expression_ref n_states = var("n_states");
const expression_ref get_alphabet = var("get_alphabet");
const expression_ref get_frequencies = var("frequencies");
const expression_ref get_component_frequencies = var("component_frequencies");
const expression_ref base_model = var("base_model");
const expression_ref distribution = var("distribution");
const expression_ref MultiRate = var("MultiRate");

// (ReversibleMarkovModel alpha state_letters q pi l t)
const expression_ref ReversibleMarkov = lambda_expression( constructor("ReversibleMarkov", 6) );

// (F81 alpha state_letters a pi)
const expression_ref F81M = lambda_expression( constructor("F81", 4) );

// (MixtureModel (DiscreteDistribution [(Double,RMM|F81)]))
const expression_ref MixtureModel = lambda_expression( constructor("MixtureModel", 1) );


const expression_ref v0 = dummy(0);
const expression_ref v1 = dummy(1);
const expression_ref v2 = dummy(2);
const expression_ref v3 = dummy(3);
const expression_ref v4 = dummy(4);
const expression_ref v5 = dummy(5);
const expression_ref v6 = dummy(6);

expression_ref operator^(const expression_ref& x, const expression_ref& T)
{
  return lambda_quantify(x,T);
}

Program get_Prelude()
{
  Program P;

  expression_ref DiscreteDistribution = lambda_expression(constructor("DiscreteDistribution",1));

  // foldr f z []  = z
  // foldr f z x:xs = (f x (foldr f z xs))
  P += Def( (foldr, v1, v2, ListEnd)    , v2)
          ( (foldr, v1, v2, v3&v4), (v1,v3,(foldr,v1,v2,v4) ) );

  // foldl f z []  = z
  // foldl f z x:xs = foldl f (f z x) xs
  P += Def( (foldl, v1, v2, ListEnd)    , v2)
    ( (foldl, v1, v2, v3&v4), (foldl, v1, (v1, v2, v3), v4) );

  // foldl' f z []  = z
  // foldl' f z x:xs = let z' = (f z x) in seq z' $ foldl f z' xs
  P += Def( (foldl_, v1, v2, ListEnd)    , v2)
          ( (foldl_, v1, v2, v3&v4), let_expression(v5,(v1,v2,v3),(seq,v5,(foldl_, v1, v5, v4) ) ) );

  // take 0 x   = []
  // take n []  = []
  // take n h:t = h:(take (n-1) t)
  {
    P += Def( (take, 0, v1), ListEnd )
            ( (take, v1, ListEnd), ListEnd)
            ( (take, v1, v2&v3), v2&(take,(v1 - 1),v3) );
  }

  // iterate f x = x:iterate f (f x)
  P += Def( (iterate, v1, v2), v2&(iterate, v1, (v1,v2)) );
  
  // fmap f []  = []
  // fmap f h:t = (f h):(fmap f t)
  P += Def( (fmap, v1, ListEnd)    , ListEnd)
          ( (fmap, v1, v2&v3), (v1,v2) & (fmap, v1, v3) );

  // fmap1 f []  = []
  // fmap1 f (p,x):t = (f p,x):(fmap1 f t)
  P += Def( (fmap1, v1, ListEnd)    , ListEnd)
          ( (fmap1, v1, Tuple(v2,v3)&v4), Tuple((v1,v2),v3) & (fmap1, v1, v4) )
          ( (fmap1, v1, (DiscreteDistribution,v2)), (DiscreteDistribution,(fmap1,v1,v2)));

  // fmap2 f []  = []
  // fmap2 f (p,x):t = (p,f x):(fmap2 f t)
  // fmap2 f (DiscreteDistribution d) = (DiscreteDistribution (fmap2 f d))
  P += Def( (fmap2, v1, ListEnd)    , ListEnd)
          ( (fmap2, v1, Tuple(v2,v3)&v4), Tuple(v2,(v1,v3)) & (fmap2, v1, v4) )
          ( (fmap2, v1, (DiscreteDistribution,v2)), (DiscreteDistribution,(fmap2,v1,v2)));

  // sum [] = 0
  // sum h:t = h+(sum t)
  expression_ref plus_i = lambda_expression( Add<Int>() );
  P += Def( (sum_i, ListEnd), 0)
          ( (sum_i, v1&v2), (plus_i, v1, (sum_i, v2)) );

  expression_ref times = lambda_expression(Multiply<Double>());

  // ExtendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution (p,x):(fmap1 \q -> q*(1.0-p) d)
  P += Def( ExtendDiscreteDistribution(DiscreteDistribution(v0),v1,v2), DiscreteDistribution(Tuple(v1,v2)&(fmap1, v4^v4*(1.0-v1), v0)) );

  // If True  y z = y
  // If False y z = z
  P += Def( (If, true , v1, v2), v1)
          ( (If, v3, v1, v2), v2);

  expression_ref MultiParameter = var("MultiParameter");
  // MultiParameter f (DiscreteDistribution d) = MixtureModel(DiscreteDistribution (fmap2 f d))
  P += Def( (MultiParameter,v1,(DiscreteDistribution,v2)), (MixtureModel,(DiscreteDistribution,(fmap2,v1,v2))));

  // MultiRate (MixtureModel (DiscreteDistribution l)) D = MixtureModel(DiscreteDistribution (fmap2 f d))
  P += Def( (MultiRate,v1,(DiscreteDistribution,v2)), (MixtureModel,(DiscreteDistribution,(fmap2,v1,v2))));

  // fst (x,y) = x
  P += Def( (fst,Tuple(v1,v2)), v1);

  // snd (x,y) = y
  P += Def( (snd,Tuple(v1,v2)), v2);

  // !! h:t 0 = h
  // !! h:t i = !! t (i-1)
  P += Def( (get_list_index,v1&v2,0), v1)
          ( (get_list_index,v1&v2,v3), (get_list_index,v2,(v3-1)) );

  // listArray b l = mkArray b \i -> l!!i
  P += Def( (listArray,v1,v2),(mkArray,v1,lambda_quantify(v3,(get_list_index,v2,v3))) );

  // length l = foldl_ (+) 0 l
  P += Def( (length, v1), (foldl_,lambda_quantify(v2,lambda_quantify(v3,v2+1)), 0, v1) );

  // scale x (ReversibleMarkov a s q pi l t) = (ReversibleMarkov a s q p l (x * t))
  // scale x (F81 a s a' pi)= (F81 a s a'*x pi) ??
  // scale x (MixtureModel (DiscreteDistribution l)) s= (MixtureModel (DiscreteDistribution (fmap2,scale(s),l))) ??
  P += Def( (scale,v0,(ReversibleMarkov,v1,v2,v3,v4,v5,v6)),(ReversibleMarkov,v1,v2,v3,v4,v5,(times,v0,v6)) );

  // rate (ReversibleMarkovModel a smap q pi l t) = t*(get_equilibrium_rate a smap q pi)
  // rate (MixtureModel (DiscreteDistribution (p,m):t) ) = p*(rate m)+(rate MixtureModel (DiscreteDistribution t) )
  //  P += Def( (rate,(ReversibleMarkov,v1,v2,v3,v4,v5)),(Get_Equilibrium_Rate,v1,v2,v3,v4) );

  // QExp (ReversibleMarkov a smap q pi l t) = (LExp l pi t)
  P += Def( (QExp, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), (LExp,v5,v4,v6));

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
  P += Def( (get_frequencies, (ReversibleMarkov,v1,v2,v3,v4,v5,v6)), v2)
          ( (get_frequencies, (F81M,v1,v2,v3,v4)), v4);

  // get_component_frequencies (MixtureModel alpha s d)  i = get_frequencies (base_model (MixtureModel alpha s d) i)
  P += Def( (get_component_frequencies, (MixtureModel,v1), v4), (get_frequencies(base_model,(MixtureModel,v1),v4)));

  // base_model (MixtureModel alpha s (DiscreteDistribution l)) i = get_list_index l i
  P += Def( (base_model, (MixtureModel,(DiscreteDistribution,v1)),v2), (snd,(get_list_index,v1,v2)));

  // distribution (MixtureModel alpha s (DiscreteDistribution l)) = fmap fst l
  P += Def( (distribution, (MixtureModel,(DiscreteDistribution,v1))), (fmap,fst,v1));

  return P;
}

const Program Prelude = get_Prelude();

/*
 * SModelObject a smap
 *
 * ExchangeModelObject S            // SymmetricMatrixObject ?
 *
 * AlphabetExchangeModelObject (SModelObject a smap) (ExchangeModelObject S)
 *
 * ReversibleFrequencyModelObject (SModelObject a smap) R pi
 *
 * ReversibleMarkovModelObject (SModelObject a smap) Q pi Lambda
 * 
 * 
 *
 *
 *
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
