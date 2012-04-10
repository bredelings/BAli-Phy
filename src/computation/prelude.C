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
const expression_ref fst = var("fst");
const expression_ref snd = var("snd");
const expression_ref get_list_index = var("!!");
const expression_ref listArray = var("listArray");
const expression_ref ExtendDiscreteDistribution = var("ExtendDiscreteDistribution");
const expression_ref length = var("length");
const expression_ref average = var("average");


const expression_ref DiscreteDistribution = lambda_expression(constructor("DiscreteDistribution",1));


Program get_Prelude()
{
  Program P;

  typed_expression_ref<Double> x1 (v1);

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
  expression_ref plus = lambda_expression( Add<Double>() );

  // ExtendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution (p,x):(fmap1 \q -> q*(1.0-p) d)
  P += Def( ExtendDiscreteDistribution(DiscreteDistribution(v0),v1,v2), DiscreteDistribution(Tuple(v1,v2)&(fmap1, v4^v4*(1.0-v1), v0)) );

  // average (DiscreteDistribution l) = foldl_ (\xy.(x+(fst y)*(snd y))) 0 l
  P += Def( (average, (DiscreteDistribution, v3) ), (foldl_, v1^(v2^(x1+(times,(fst,v2),(snd,v2)))), 0.0, v3) );

  // If True  y z = y
  // If False y z = z
  P += Def( (If, true , v1, v2), v1)
          ( (If, v3, v1, v2), v2);

  // fst (x,y) = x
  P += Def( (fst,Tuple(v1,v2)), v1);

  // snd (x,y) = y
  P += Def( (snd,Tuple(v1,v2)), v2);

  // !! h:t 0 = h
  // !! h:t i = !! t (i-1)
  P += Def( (get_list_index,v1&v2,0), v1)
          ( (get_list_index,v1&v2,v3), (get_list_index,v2,(v3-1)) );

  // listArray b l = mkArray b \i -> l!!i
  P += Def( (listArray,v1,v2),(mkArray, v1, v3^(get_list_index,v2,v3)) );

  // length l = foldl_ (+) 0 l
  P += Def( (length, v1), (foldl_,v2^(v3^(v2+1)), 0, v1) );

  return P;
}

const Program Prelude = get_Prelude();
