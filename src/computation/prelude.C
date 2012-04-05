#include "computation/prelude.H"
#include "computation/operations.H"
#include "computation/graph_register.H"

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

  // (RateMatrix q pi l t)
const expression_ref RateMatrix = lambda_expression( constructor("RateMatrix", 4) );

// (ReversibleMarkovModel alpha state_letters (RateMatrix q pi l t))
const expression_ref ReversibleMarkovM = lambda_expression( constructor("ReversibleMarkovModel", 3) );

// (F81 alpha state_letters a pi)
const expression_ref F81M = lambda_expression( constructor("F81", 4) );

// (MixtureM alpha state_letters (DiscreteDistribution [(Double,RMM|F81)]))
const expression_ref MixtureM = lambda_expression( constructor("MixtureM", 3) );


const expression_ref v0 = dummy(0);
const expression_ref v1 = dummy(1);
const expression_ref v2 = dummy(2);
const expression_ref v3 = dummy(3);
const expression_ref v4 = dummy(4);
const expression_ref v5 = dummy(5);

expression_ref operator^(const expression_ref& x, const expression_ref& T)
{
  return lambda_quantify(x,T);
}

Program get_Prelude()
{
  Program P;

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
          ( (fmap, v1, v2&v3), Tuple(v1,v2) & (fmap, v1, v3) );

  // fmap1 f []  = []
  // fmap1 f (p,x):t = (f p,x):(fmap1 f t)
  P += Def( (fmap1, v1, ListEnd)    , ListEnd)
          ( (fmap1, v1, Tuple(v2,v3)&v4), Tuple((v1,v2),v3) & (fmap1, v1, v4) );

  // fmap2 f []  = []
  // fmap2 f (p,x):t = (p,f x):(fmap2 f t)
  P += Def( (fmap2, v1, ListEnd)    , ListEnd)
          ( (fmap2, v1, Tuple(v2,v3)&v4), Tuple(v2,(v1,v3)) & (fmap2, v1, v4) );

  // sum [] = 0
  // sum h:t = h+(sum t)
  expression_ref plus_i = lambda_expression( Add<Int>() );
  P += Def( (sum_i, ListEnd), 0)
          ( (sum_i, v1&v2), (plus_i, v1, (sum_i, v2)) );

  expression_ref times = lambda_expression(Multiply<Double>());

  // ExtendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution (p,x):(fmap1 \q -> q*(1.0-p) d)
  expression_ref DiscreteDistribution = lambda_expression(constructor("DiscreteDistribution",1));
  P += Def( ExtendDiscreteDistribution(DiscreteDistribution(v0),v1,v2), DiscreteDistribution(Tuple(v1,v2)&(fmap1, v4^v4*(1.0-v1), v0)) );

  // If True  y z = y
  // If False y z = z
  P += Def( (If, true , v1, v2), v1)
          ( (If, v3, v1, v2), v2);

  expression_ref MultiParameter = var("MultiParameter");
  // MultiParameter f (DiscreteDistribution d) = DiscreteDistribution (fmap2 f d)
  P += Def( (MultiParameter,v1,(DiscreteDistribution,v2)), (DiscreteDistribution,(fmap2,v1,v2)));

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

  // length [] = 0
  // length h:t = 1+length(t);
  P += Def( (length,ListEnd),0)
          ( (length,v1&v2),1+(length,v2));

  // scale (RateMatrix q pi l t) s = (RateMatrix q pi l (s*t))
  // scale (ReversibleMarkovM a s q) = (ReversibleMarkovM a s (scale q))
  // scale (F81 a s a' pi) s = (F81 a s a'*s pi) ??
  // scale (MixtureM a s (DiscreteDistribution l)) s= (MixtureM a s (DiscreteDistribution (fmap2,times(s),l))) ??
  P += Def( (scale,(RateMatrix,v1,v2,v3,v4),v5),(RateMatrix,v1,v2,v3,(times,v4,v5)));

  // rate (RateMatrix q pi l t) = t
  // rate (MixtureM ?)
  P += Def( (rate,(RateMatrix,v1,v2,v3,v4)),v4);

  // QExp (RateMatrix q pi l t) = (LExp l pi t)
  P += Def( (QExp, (RateMatrix,v1,v2,v3,v4)), (LExp,v3,v2,v4));

  // n_base_models (MixtureM a state_letters (DiscreteDistribution l)) = length l
  P += Def( (n_base_models, (MixtureM,v1,v2,(DiscreteDistribution,v3))), (length,v3));
  
  // state_letters (ReversibleMarkovM alpha s q) = s
  // state_letters (F81 alpha s a pi) = s
  // state_letters (MixtureM alpha s d) = state_letters (base_model (MixtureM alpha s d) 0)
  P += Def( (state_letters, (ReversibleMarkovM,v1,v2,v3)), v2)
          ( (state_letters, (F81M,v1,v2,v3,v4)), v2)
          ( (state_letters, (MixtureM,v1,v2,v3)), (state_letters,(base_model,(MixtureM,v1,v2,v3),0)));

  // n_states m = vector_size (state_letters m)
  P += Def( (n_states,v1), (VectorSize<unsigned>(),(state_letters,v1)));
  
  // get_alphabet (ReversibleMarkovM alpha s q) = alpha
  // get_alphabet (F81 alpha s a pi) = alpha
  // get_alphabet (MixtureM alpha s d) = alpha
  P += Def( (get_alphabet, (ReversibleMarkovM,v1,v2,v3)), v1)
          ( (get_alphabet, (F81M,v1,v2,v3,v4)), v1)
          ( (get_alphabet, (MixtureM,v1,v2,v3)), v1);

  // get_frequencies (RateMatrix q pi l t) = pi
  // get_frequencies (ReversibleMarkovM alpha s q) = get_frequencies q
  // get_frequencies (F81 alpha s a pi) = pi
  P += Def( (get_frequencies, (RateMatrix,v1,v2,v3,v4)), v4)
          ( (get_frequencies, (ReversibleMarkovM,v1,v2,v3)), (get_frequencies,v1))
        ( (get_frequencies, (F81M,v1,v2,v3,v4)), v4);

  // get_component_frequencies (MixtureM alpha s d)  i = get_frequencies (base_model (MixtureM alpha s d) i)
  P += Def( (get_component_frequencies, (MixtureM,v1,v2,v3), v4), (get_frequencies(base_model,(MixtureM,v1,v2,v3),v4)));

  // base_model (MixtureM alpha s (DiscreteDistribution l)) i = get_list_index l i
  P += Def( (base_model, (MixtureM,v1,v2,(DiscreteDistribution,v3)),v4), (get_list_index,v3,v4));

  // distribution (MixtureM alpha s (DiscreteDistribution l)) = fmap fst l
  P += Def( (distribution, (MixtureM,v1,v2,(DiscreteDistribution,v3))), (fmap,fst,v3));

  

  return P;
}

const Program Prelude = get_Prelude();
