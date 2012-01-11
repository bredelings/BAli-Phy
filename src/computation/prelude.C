#include "computation/prelude.H"
#include "computation/operations.H"
#include "computation/graph_register.H"

const expression_ref fmap = var("fmap");
const expression_ref take = var("take");
const expression_ref iterate = var("iterate");
const expression_ref sum_i = var("sum_i");

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
    typed_expression_ref<Int> I1 = v1;
    P += Def( take(0, v1), ListEnd )
            ( take(v1, ListEnd), ListEnd)
            ( take(v1, Cons(v2,v3)), Cons(v2, take(I1 - 1)(v3)) );
  }

  // iterate f x = x:iterate f (f x)
  P += Def( iterate(v1,v2), Cons(v2, iterate(v1)(v1(v2))) );
  
  // fmap f []  = []
  // fmap f h:t = (f h):(fmap f t)
  P += Def( fmap(v1, ListEnd)    , ListEnd)
          ( fmap(v1, Cons(v2,v3)), Cons(v1(v2), fmap(v1, v3) ) );

  // sum [] = 0
  // sum h:t = h+(sum t)
  expression_ref plus_i = lambda_expression( Add<Int>() );
  P += Def( sum_i(ListEnd), 0)
          ( sum_i(Cons(v1,v2)), plus_i(v1,sum_i(v2)) );

  return P;
}

const Program Prelude = get_Prelude();
