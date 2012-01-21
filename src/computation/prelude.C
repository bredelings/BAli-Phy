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

const expression_ref v0 = dummy(0);
const expression_ref v1 = dummy(1);
const expression_ref v2 = dummy(2);
const expression_ref v3 = dummy(3);
const expression_ref v4 = dummy(4);

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
  P += Def( ExtendDiscreteDistribution(DiscreteDistribution(v0),v1,v2), DiscreteDistribution(Tuple(v1,v2)&(fmap1, times(1.0-v1), v0)) );

  // If True  y z = y
  // If False y z = z
  P += Def( (If, true , v1, v2), v1)
          ( (If, false, v1, v2), v2);

  expression_ref MultiParameter = var("MultiParameter");
  // MultiParameter f (DiscreteDistribution d) = DiscreteDistribution (fmap f d)
  P += Def( (MultiParameter,v1,(DiscreteDistribution,v2)), (DiscreteDistribution,(fmap2,v1,v2)));

  return P;
}

const Program Prelude = get_Prelude();
