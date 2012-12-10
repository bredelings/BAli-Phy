#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include "mytypes.H"

using std::vector;

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
const expression_ref listArray_ = var("listArray'");
const expression_ref ExtendDiscreteDistribution = var("ExtendDiscreteDistribution");
const expression_ref MixDiscreteDistributions = var("MixDiscreteDistributions");
const expression_ref MixDiscreteDistributions_ = var("MixDiscreteDistributions'");
const expression_ref UniformDiscretize = var("UniformDiscretize");
const expression_ref length = var("length");
const expression_ref plusplus = var("++");
const expression_ref average = var("average");
const expression_ref reapply = var("reapply");
const expression_ref unsafePerformIO_ = var("unsafePerformIO'");
const expression_ref unsafePerformIO = var("unsafePerformIO");
const expression_ref join_ = var("join");

const expression_ref DiscreteDistribution = lambda_expression(constructor("DiscreteDistribution",1));
const expression_ref UnwrapDD = var("UnwrapDD");

/*
 * 1. Remove true/false in favor of True/False.
 * 2. Convert strings to [Char]
 * 3. Convert Defs to use the machine.
 * 4. SYNTAX: replace a ~ b ( c ) with a ~ b
 * 5. SYNTAX: external a ~ b [To not declare all parameters]
 * 6. Allows defs or something in BUGS files.
 *    - Allow multiline commands in BUGS files.
 *    - What then separates things?
 * 7. Rationalize Model_Notes, formula_expression_ref, and program?
 *    - Make Model_Notes into a Program with notes added?
 *    - Could we parse a BUGS file in to a Model_Notes?
 * 8. Try to rewrite e.g. M8b into a BUGS module.
 */


Program make_Prelude()
{
  Program P("Prelude");

  P.def_function("true", 0, true );
  P.def_function("false", 0, false );
  P.def_function("error", 1, lambda_expression( Error() ) ); 
  P.def_function("intToDouble", 1, lambda_expression( Conversion<int,double>() ) ); 

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

  P += "{repeat x = x:(repeat x)}";

  // iterate f x = x:iterate f (f x)
  P += Def( (iterate, v1, v2), v2&(iterate, v1, (v1,v2)) );

  P += "{map f []  = [];\
         map f (h:t) = (f h):(map f t)}";
  
  P += "{fmap = map}";

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

  // (f . g) x = f (g x)
  P += Def( (var("."), v1, v2, v3)    , (v1, (v2, v3)) );

  

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

  // UniformDiscretize q n = map (\i->(1.0/n, q ((2*i+1)/n) )) (take n (iterate (+1) 0) )
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

  // snd (x,y) = y
  P += Def( (var("swap"),Tuple(v1,v2)), Tuple(v2,v1));

  // !! h:t 0 = h
  // !! h:t i = !! t (i-1)
  P += Def( (get_list_index,v1&v2,0), v1)
          ( (get_list_index,v1&v2,v3), (get_list_index,v2,(v3-1)) );

  // listArray b l = mkArray b \i -> l!!i
  P += Def( (listArray,v1,v2),(mkArray, v1, v3^(get_list_index,v2,v3)) );

  // listArray' l = listArray (length l) l
  P += Def( (listArray_,v1),(listArray,(length,v1),v1));

  // length l = foldl' (\x y->(x+1)) 0 l
  P += Def( (length, v1), (foldl_,v2^(v3^(v2+1)), 0, v1) );

  // plusplus [] y = y
  // plusplus h:t y = h:(plusplus t y)
  P += Def( (plusplus, ListEnd, v0), v0)
          ( (plusplus, v0&v1, v2),v0&(plusplus,v1,v2));

  // UnwrapDD (DiscreteDistribution l) = l
  P += Def( (UnwrapDD, (DiscreteDistribution, v1)), v1 );

  const expression_ref IOAction1 = lambda_expression(constructor("IOAction1",2));
  const expression_ref IOAction2 = lambda_expression(constructor("IOAction2",3));
  const expression_ref IOAction3 = lambda_expression(constructor("IOAction3",4));
  const expression_ref IOAction4 = lambda_expression(constructor("IOAction4",5));
  const expression_ref IOReturn = lambda_expression(constructor("IOReturn",1));
  const expression_ref IOAndPass = lambda_expression(constructor("IOAndPass",2));
  const expression_ref IOAnd = lambda_expression(constructor("IOAnd",2));

  P.def_constructor("IOAction1",2);
  P.def_constructor("IOAction2",3);
  P.def_constructor("IOAction3",4);
  P.def_constructor("IOAction4",5);

  P.def_constructor("IOReturn1",1);
  P.def_constructor("IOAndPass",2);
  P.def_constructor("IOAnd",2);

  // unsafePerformIO x = reapply unsafePerformIO' x
  P += Def( (unsafePerformIO, v1), (reapply, unsafePerformIO_, v1) );

  // FIXME? IOAction 0 doesn't work, because we don't get a separate cell for each application... to nothing.
  //        Current approach: supply dummy arguments to such a builtin that are not used.

  // unsafePerformIO' (IOAction1 x y) = x y
  // unsafePerformIO' (IOAction2 x y z) = x y z
  // unsafePerformIO' (IOAction3 x y z w ) = x y z w 
  // unsafePerformIO' (IOAction4 x y z w u) = x y z w u
  // unsafePerformIO' (IOReturn a) = a
  // unsafePerformIO' (IOAndPass f g) = let x = (unsafePerformIO' f) in x `join` (unsafePerformIO' (g x))
  // unsafePerformIO' (IOAnd f g) = (unsafePerformIO' f) `join` (unsafePerformIO' g)
  P += Def( (unsafePerformIO_, (IOAction1, v1, v2)), (v1,v2))
    ( (unsafePerformIO_, (IOAction2, v1, v2, v3)), (v1, v2, v3))
    ( (unsafePerformIO_, (IOAction3, v1, v2, v3, v4)), (v1, v2, v3, v4))
    ( (unsafePerformIO_, (IOAction4, v1, v2, v3, v4, v5)), (v1, v2, v3, v4, v5))
    ( (unsafePerformIO_, (IOReturn, v1)), v1)
    ( (unsafePerformIO_, (IOAndPass, v1, v2)), let_expression(v3,(unsafePerformIO_, v1), (join_, v3, (unsafePerformIO_, (v2, v3)))))
    ( (unsafePerformIO_, (IOAnd, v1, v2)), (join_, (unsafePerformIO_, v1), (unsafePerformIO_, v2)));

  //--------------------------------------- listFromVectorInt ----------------------------------------//
  P.def_function("getVectorIntElement", 2, lambda_expression( BuiltinGetVectorIndexOp<int,Int>() ) ); 
  P.def_function("sizeOfVectorInt", 1, lambda_expression( VectorSizeOp<int>() ) );


  // listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):(listFromVectorInt v s (i+1)) else []
  P += Def( (var("listFromVectorInt'"), v1, v2, v3), (If,(v3<v2),(var("getVectorIntElement"),v1,v3)&(var("listFromVectorInt'"),v1,v2,(v3+1)),ListEnd));

  // listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0
  P += Def( (var("listFromVectorInt"), v1), (var("listFromVectorInt'"),v1,(var("sizeOfVectorInt"),v1),0));


  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorVectorIntElement", 2, lambda_expression( BuiltinGetVectorIndexOp<Vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorVectorInt", 1, lambda_expression( VectorSizeOp<Vector<int>>() ) );

  // listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):(listFromVectorVectorInt v s (i+1)) else []
  P += Def( (var("listFromVectorVectorInt'"), v1, v2, v3), (If,(v3<v2),(var("getVectorVectorIntElement"),v1,v3)&(var("listFromVectorVectorInt'"),v1,v2,(v3+1)),ListEnd));

  // listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0
  P += Def( (var("listFromVectorVectorInt"), v1), (var("listFromVectorVectorInt'"),v1,(var("sizeOfVectorVectorInt"),v1),0));

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorvectorIntElement", 2, lambda_expression( BuiltinGetVectorIndexOp<vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorvectorInt", 1, lambda_expression( VectorSizeOp<vector<int>>() ) );

  // listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):(listFromVectorvectorInt v s (i+1)) else []
  P += Def( (var("listFromVectorvectorInt'"), v1, v2, v3), (If,(v3<v2),(var("getVectorvectorIntElement"),v1,v3)&(var("listFromVectorvectorInt'"),v1,v2,(v3+1)),ListEnd));

  // listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0
  P += Def( (var("listFromVectorvectorInt"), v1), (var("listFromVectorvectorInt'"),v1,(var("sizeOfVectorvectorInt"),v1),0));

  //--------------------------------------- listToVectorInt ---------------------------------------//

  P.def_function("builtinNewVectorInt", 1, lambda_expression( BuiltinNewVectorOp<int>() ) ); 
  P.def_function("builtinSetVectorIndexInt", 3, lambda_expression( BuiltinSetVectorIndexOp<int,Int>() ) ); 

  const expression_ref builtinNewVectorInt = var("builtinNewVectorInt");
  const expression_ref builtinSetVectorIndexInt = var("builtinSetVectorIndexInt");

  P += Def( (var("newVectorInt"), v1), (IOAction1, builtinNewVectorInt, v1));

  P += Def( (var("setVectorIndexInt"), v1, v2, v3), (IOAction3, builtinSetVectorIndexInt, v1, v2, v3));

  const expression_ref listToVectorInt = var("listToVectorInt");
  const expression_ref copyListToVectorInt = var("copyListToVectorInt");
  // listToVectorInt l = do { v <- newVectorInt (length l); copyListToVector l v 0 ; return v;}
  // listToVectorInt l = newVectorInt (length l) <<= (\v -> copyListToVector l v 0 << return v;)
  P += Def( (listToVectorInt, v1), (unsafePerformIO, (IOAndPass, (var("newVectorInt"), (length, v1)),
							 lambda_quantify(v2,(IOAnd,(copyListToVectorInt, v1, v2, 0),(IOReturn, v2))) ) ) );

  // copyListToVectorInt [] v i = return ()
  // copyListToVectorInt h:t v i = do { setVectorIndexInt v i h ; copyListToVectorInt t v (i+1) }
  // copyListToVectorInt h:t v i = setVectorIndexInt v i h << copyListToVectorInt t v (i+1)
  P += Def( (copyListToVectorInt, ListEnd, v3, v4), (IOReturn, constructor("()",0)))
    ( (copyListToVectorInt, v1&v2  , v3, v4), (IOAnd,(var("setVectorIndexInt"), v3, v4, v1),(copyListToVectorInt, v2,v3,(v4+1))));

  //--------------------------------------- listToVectorDouble ---------------------------------------//

  P.def_function("builtinNewVectorDouble", 1, lambda_expression( BuiltinNewVectorOp<double>() ) ); 
  P.def_function("builtinSetVectorIndexDouble", 3, lambda_expression( BuiltinSetVectorIndexOp<double,Double>() ) ); 

  const expression_ref builtinNewVectorDouble = var("builtinNewVectorDouble");
  const expression_ref builtinSetVectorIndexDouble = var("builtinSetVectorIndexDouble");

  const expression_ref newVectorDouble = var("newVectorDouble");
  P += Def( (newVectorDouble, v1), (IOAction1, builtinNewVectorDouble, v1));

  const expression_ref setVectorIndexDouble = var("setVectorIndexDouble");
  P += Def( (setVectorIndexDouble, v1, v2, v3), (IOAction3, builtinSetVectorIndexDouble, v1, v2, v3));

  const expression_ref listToVectorDouble = var("listToVectorDouble");
  const expression_ref copyListToVectorDouble = var("copyListToVectorDouble");
  // listToVectorDouble l = do { v <- newVectorDouble (length l); copyListToVector l v 0 ; return v;}
  // listToVectorDouble l = newVectorDouble (length l) <<= (\v -> copyListToVector l v 0 << return v;)
  P += Def( (listToVectorDouble, v1), (unsafePerformIO, (IOAndPass, (newVectorDouble, (length, v1)),
							 lambda_quantify(v2,(IOAnd,(copyListToVectorDouble, v1, v2, 0),(IOReturn, v2))) ) ) );

  // copyListToVectorDouble [] v i = return ()
  // copyListToVectorDouble h:t v i = do { setVectorIndexDouble v i h ; copyListToVectorDouble t v (i+1) }
  // copyListToVectorDouble h:t v i = setVectorIndexDouble v i h << copyListToVectorDouble t v (i+1)
  P += Def( (copyListToVectorDouble, ListEnd, v3, v4), (IOReturn, constructor("()",0)))
    ( (copyListToVectorDouble, v1&v2  , v3, v4), (IOAnd,(setVectorIndexDouble, v3, v4, v1),(copyListToVectorDouble, v2,v3,(v4+1))));

  //--------------------------------------- listToVectorMatrix ---------------------------------------//

  P.def_function("builtinNewVectorMatrix", 1, lambda_expression( BuiltinNewVectorOp<Matrix>() ) ); 
  P.def_function("builtinSetVectorIndexMatrix", 3, lambda_expression( BuiltinSetVectorIndexOp<Matrix,MatrixObject>() ) ); 

  const expression_ref builtinNewVectorMatrix = var("builtinNewVectorMatrix");
  const expression_ref builtinSetVectorIndexMatrix = var("builtinSetVectorIndexMatrix");

  const expression_ref newVectorMatrix = var("newVectorMatrix");
  P += Def( (newVectorMatrix, v1), (IOAction1, builtinNewVectorMatrix, v1));

  const expression_ref setVectorIndexMatrix = var("setVectorIndexMatrix");
  P += Def( (setVectorIndexMatrix, v1, v2, v3), (IOAction3, builtinSetVectorIndexMatrix, v1, v2, v3));

  const expression_ref listToVectorMatrix = var("listToVectorMatrix");
  const expression_ref copyListToVectorMatrix = var("copyListToVectorMatrix");
  // listToVectorMatrix l = do { v <- newVectorMatrix (length l); copyListToVector l v 0 ; return v;}
  // listToVectorMatrix l = newVectorMatrix (length l) <<= (\v -> copyListToVector l v 0 << return v;)
  P += Def( (listToVectorMatrix, v1), (unsafePerformIO, (IOAndPass, (newVectorMatrix, (length, v1)),
							 lambda_quantify(v2,(IOAnd,(copyListToVectorMatrix, v1, v2, 0),(IOReturn, v2))) ) ) );

  // copyListToVectorMatrix [] v i = return ()
  // copyListToVectorMatrix h:t v i = do { setVectorIndexMatrix v i h ; copyListToVectorMatrix t v (i+1) }
  // copyListToVectorMatrix h:t v i = setVectorIndexMatrix v i h << copyListToVectorMatrix t v (i+1)
  P += Def( (copyListToVectorMatrix, ListEnd, v3, v4), (IOReturn, constructor("()",0)))
    ( (copyListToVectorMatrix, v1&v2  , v3, v4), (IOAnd,(setVectorIndexMatrix, v3, v4, v1),(copyListToVectorMatrix, v2,v3,(v4+1))));

  //------------------------------------------------------------------------------------------------//

  P.def_function("reapply", 2, lambda_expression( Reapply() ) );
  P.def_function("join", 2, lambda_expression( Join() ) );
  P.def_function("negate", 1, lambda_expression( Negate() ) );
  P.def_function("exp", 1, lambda_expression( Exp_Op() ) );
  P.def_function("!", 2, lambda_expression( GetIndex() ) );
  P.def_function("getAddress", 1, lambda_expression( Get_Address() ) );

  P.declare_fixity("!!", 9, left_fix);
  // Is this right?
  P.declare_fixity("!", 9, left_fix);

  /*
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
  */
  P.declare_fixity(".", 9, left_fix);

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
  // ":" is builtin, but has precedence 5 and right fixity.
  P.declare_fixity("++", 5, right_fix);

  P.def_function("==", 2, lambda_expression( Equals() ) );
  P.declare_fixity("==", 5, non_fix);
  P.def_function("/=", 2, lambda_expression( NotEquals() ) );
  P.declare_fixity("/=", 5, non_fix);
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

  P += "{head (h:t) = h}";
  P += "{tail (h:t) = t}";

  // FIXME - we have an problem with types here.  This will only work for Int, as-is.
  P += "{enumFrom x = x:(enumFrom (x+1))}";
  P += "{enumFromTo x y = if (x==y) then [x] else x:(enumFromTo (x+1) y)}";
  //  P += "{enumFromThen x y = ... }";
  //  P += "{enumFromThenTo x y z = ... }";

  P += "{zip (x:xs) (y:ys) = (x,y):(zip xs ys);\
         zip []   _        = [];\
         zip _   []        = []}";

  P += "{concat xs = foldr (++) [] xs}";

  P += {"{concatMap f = concat . map f}"};

  P.def_function("doubleToLogDouble", 1, lambda_expression( Conversion<double,log_double_t>() ) );

  P += "{uniformQuantiles q n = map (\\i -> q ((2.0*(intToDouble i)+1.0)/(intToDouble n)) ) (take n [1..])}";

  return P;
}

const Program& get_Prelude()
{
  static const Program P = make_Prelude();
  return P;
}
