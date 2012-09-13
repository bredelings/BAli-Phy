#include "computation/prelude.H"
#include "computation/program.H"
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
const expression_ref sum_ = var("sum");
const expression_ref If = var("If");
const expression_ref fst = var("fst");
const expression_ref snd = var("snd");
const expression_ref get_list_index = var("!!");
const expression_ref listArray = var("listArray");
const expression_ref ExtendDiscreteDistribution = var("ExtendDiscreteDistribution");
const expression_ref MixDiscreteDistributions = var("MixDiscreteDistributions");
const expression_ref MixDiscreteDistributions_ = var("MixDiscreteDistributions'");
const expression_ref UniformDiscretize = var("UniformDiscretize");
const expression_ref length = var("length");
const expression_ref plusplus = var("++");
const expression_ref average = var("average");


const expression_ref DiscreteDistribution = lambda_expression(constructor("DiscreteDistribution",1));
const expression_ref UnwrapDD = var("UnwrapDD");


Program make_Prelude()
{
  Program P("Prelude");

  // foldr f z []  = z
  // foldr f z x:xs = (f x (foldr f z xs))
  P += Def( (foldr, v1, v2, ListEnd)    , v2)
          ( (foldr, v1, v2, v3&v4), (v1,v3,(foldr,v1,v2,v4) ) );

  // foldl f z []  = z
  // foldl f z x:xs = foldl f (f z x) xs
  P += Def( (foldl, v1, v2, ListEnd)    , v2)
          ( (foldl, v1, v2, v3&v4), (foldl, v1, (v1, v2, v3), v4) );

  // foldl' f z []  = z
  // foldl' f z x:xs = let z' = (f z x) in seq z' $ foldl' f z' xs
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
  expression_ref plus = lambda_expression( Add() );
  P += Def( (sum_, ListEnd), 0)
          ( (sum_, v1&v2), v1 + (sum_, v2) );

  expression_ref to_double = lambda_expression( Conversion<int,double>() );

  // ExtendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution (p,x):(fmap1 \q -> q*(1.0-p) d)
  P += Def( (ExtendDiscreteDistribution, (DiscreteDistribution, v0), v1, v2), (DiscreteDistribution, Tuple(v1,v2)&(fmap1, v4^v4*(1.0-v1), v0)) );

  // MixDiscreteDistributions_ h:t h2:t2 = DiscreteDistribution (fmap1 \q->q*h h2)++(MixDiscreteDistributions_ t t2)
  // MixDiscreteDistributions_ []  []    = []
  P += Def( (MixDiscreteDistributions_, v0&v1, v2&v3) , (plusplus,(fmap1, v4^v4*v0, v2),(MixDiscreteDistributions_,v1,v3)))
          ( (MixDiscreteDistributions_, ListEnd, ListEnd) , ListEnd );

  // MixDiscreteDistributions l1 l2 = DiscreteDistribution (MixDiscreteDistributions l1 (fmap UnwrapDD l2))
  P += Def( (MixDiscreteDistributions, v0, v1) , (DiscreteDistribution, (MixDiscreteDistributions_, v0, (fmap,UnwrapDD,v1))));

  // average (DiscreteDistribution l) = foldl_ (\xy.(x+(fst y)*(snd y))) 0 l
  P += Def( (average, (DiscreteDistribution, v3) ), (foldl_, v1^(v2^(v1+(fst,v2)*(snd,v2))), 0.0, v3) );

  // UniformDiscretize q n = fmap /\i.(1/n, q ((2*i+1)/n) ) (take n (iterate (+1) 0) )
  // [ We could do this as two nested fmaps, instead. ]
  // [ We could factor out to_double(v2), and 1.0/to_double(v2)
  P += Def( (UniformDiscretize, v1, v2), (DiscreteDistribution, (fmap, lambda_quantify(v3,let_expression(v4,(to_double,v2), Tuple(1.0/v4, (v1,((2.0*v3+1.0)/(2.0*v4)))))), (take, v2, (iterate, (plus,1.0), 0.0) ) ) ) );

  // If True  y z = y
  // If _     y z = z
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

  // length l = foldl' \x->\y->(x+1) 0 l
  P += Def( (length, v1), (foldl_,v2^(v3^(v2+1)), 0, v1) );

  // plusplus [] y = y
  // plusplus h:t y = h:(plusplus t y)
  P += Def( (plusplus, ListEnd, v0), v0)
          ( (plusplus, v0&v1, v2),v0&(plusplus,v1,v2));

  // UnwrapDD (DiscreteDistribution l) = l
  P += Def( (UnwrapDD, (DiscreteDistribution, v1)), v1 );


  P.declare_fixity("!!", 9, left_fix);
  /*
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
  */
  //  P.declare_fixity(".", 9, left_fix);

  //  P.declare_fixity("^", 8, right_fix);
  //  P.declare_fixity("^^", 8, right_fix);
  //  P.declare_fixity("**", 8, right_fix);

  P.def_function("*", 2, lambda_expression( Multiply() ) );
  P.declare_fixity("*", 7, left_fix);
  P.def_function("/", 2, lambda_expression( Divide() ) );
  P.declare_fixity("/", 7, left_fix);

  //  P.declare_fixity("div", 7, left_fix);
  //  P.declare_fixity("mod", 7, left_fix);
  //  P.declare_fixity("rem", 7, left_fix);
  //  P.declare_fixity("quot", 7, left_fix);

  P.def_function("+", 2, lambda_expression( Add() ) ); 
  P.declare_fixity("+", 6, left_fix);
  P.def_function("-", 2, lambda_expression( Minus() ) );
  P.declare_fixity("-", 6, left_fix);

  // this needs to be added as a constructor expression
  P.def_function(":", 2, lambda_expression( right_assoc_constructor(":",2) ) );
  P.declare_fixity(":", 5, right_fix);
  P.declare_fixity("++", 5, right_fix);

  P.def_function("==", 2, lambda_expression( Equals() ) );
  P.declare_fixity("==", 5, non_fix);
  //  P.declare_fixity("/=", 5, non_fix);
  P.def_function("<", 2, lambda_expression( LessThan() ) );
  P.declare_fixity("<", 5, non_fix);
  //  P.declare_fixity("<=", 5, non_fix);
  P.def_function(">", 2, lambda_expression( GreaterThan() ) );
  P.declare_fixity(">", 5, non_fix);
  //  P.declare_fixity(">=", 5, non_fix);
  
  //  P.declare_fixity("elem", 4, non_fix);
  //  P.declare_fixity("notElem", 4, non_fix);

  //  P.declare_fixity("&&", 3, right_fix);
  //  P.declare_fixity("||", 2, right_fix);

  //  P.declare_fixity(">>", 1, left_fix);
  //  P.declare_fixity(">>=", 1, left_fix);

  /*
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x
  */
  //  P.declare_fixity("$", 0, right_fix);
  //  P.declare_fixity("$!", 0, right_fix);
  P.def_function("seq", 2, lambda_expression( Seq() ) );
  P.declare_fixity("seq", 0, right_fix);

  return P;
}

const Program& get_Prelude()
{
  static const Program P = make_Prelude();
  return P;
}
