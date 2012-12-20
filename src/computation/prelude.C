#include "computation/expression.H"
#include "computation/prelude.H"
#include "computation/program.H"
#include "computation/operations.H"
#include "computation/graph_register.H"
#include "mytypes.H"

using std::vector;

const expression_ref get_list_index = var("!!");
const expression_ref plusplus = var("++");

/* TODO:
 * 1. [DONE] Remove true/false in favor of True/False.
 * 2. Convert strings to [Char]
 * 3. [DONE] Convert Defs to use the machine.
 * 4. SYNTAX: replace a ~ b ( c ) with a ~ b
 * 5. SYNTAX: external a ~ b [To not declare all parameters]
 * 6. [DONE] Allow defs in BUGS files.
 * 7. Rationalize Model_Notes, formula_expression_ref, and program?
 *    - Make Model_Notes into a Program with notes added?
 *    - Could we parse a BUGS file in to a Model_Notes?
 * 8. Try to rewrite e.g. M8b into a BUGS module.
 * 9. Add default values and Bounds to distributions.
 *    - Ah, but how to we add default values to distributions that return random structures?
 *    - Well, do we want to supply Bounds for structure ELEMENTS?  Uh-oh -- we might!
 * 10. [DONE] Convert all of distribution-operations.H to the parser.
 * 11. [DONE] Remove arity argument to def_function.
 * 12. Rationalize Programs, Modules.
 * 13. Allow loading stuff from files.
 */


Program make_Prelude()
{
  Program P("Prelude");

  P.def_function("error", lambda_expression( Error() ) ); 
  P.def_function("intToDouble", lambda_expression( Conversion<int,double>() ) ); 
  P.def_function("mkArray", lambda_expression( MkArray() ) ); 
  P.def_function("reapply", lambda_expression( Reapply() ) );
  P.def_function("join", lambda_expression( Join() ) );
  P.def_function("negate", lambda_expression( Negate() ) );
  P.def_function("exp", lambda_expression( Exp_Op() ) );
  P.def_function("log", lambda_expression( Log_Op() ) );
  P.def_function("!", lambda_expression( GetIndex() ) );
  P.def_function("getAddress", lambda_expression( Get_Address() ) );

  P.def_constructor("True",0);
  P.def_constructor("False",0);
  P.def_constructor("Just",1);
  P.def_constructor("Nothing",0);
  P.def_constructor("DiscreteDistribution",1);

  P.def_constructor("IOAction1",2);
  P.def_constructor("IOAction2",3);
  P.def_constructor("IOAction3",4);
  P.def_constructor("IOAction4",5);

  P.def_constructor("IOReturn",1);
  P.def_constructor("IOAndPass",2);
  P.def_constructor("IOAnd",2);

  // FIXME? IOAction 0 doesn't work, because we don't get a separate cell for each application... to nothing.
  //        Current approach: supply dummy arguments to such a builtin that are not used.

  P += "{unsafePerformIO' (IOAction1 x y ) = x y;\
         unsafePerformIO' (IOAction2 x y z) = x y z;\
         unsafePerformIO' (IOAction3 x y z w) = x y z w;\
         unsafePerformIO' (IOAction4 x y z w u) = x y z w u;\
         unsafePerformIO' (IOReturn x) = x;\
         unsafePerformIO' (IOAndPass f g) = let {x = unsafePerformIO' f} in x `join` (unsafePerformIO' (g x));\
         unsafePerformIO' (IOAnd f g) = (unsafePerformIO' f) `join` (unsafePerformIO' g)}";

  P += "{unsafePerformIO x = reapply unsafePerformIO' x}";

  //------------------------------------------------------------------------------------------------//


  // Is this right?
  P.declare_fixity("!", 9, left_fix);

  /*
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f (g x)
  */

  //  P.declare_fixity("^", 8, right_fix);
  //  P.declare_fixity("^^", 8, right_fix);
  //  P.declare_fixity("**", 8, right_fix);

  P.def_function("*", lambda_expression( Multiply() ) );
  P.declare_fixity("*", 7, left_fix);
  P.def_function("/", lambda_expression( Divide() ) );
  P.declare_fixity("/", 7, left_fix);

  //  P.declare_fixity("div", 7, left_fix);
  //  P.declare_fixity("mod", 7, left_fix);
  //  P.declare_fixity("rem", 7, left_fix);
  //  P.declare_fixity("quot", 7, left_fix);

  P.def_function("+", lambda_expression( Add() ) ); 
  P.declare_fixity("+", 6, left_fix);
  P.def_function("-", lambda_expression( Minus() ) );
  P.declare_fixity("-", 6, left_fix);

  // this needs to be added as a constructor expression
  // ":" is builtin, but has precedence 5 and right fixity.
  P.declare_fixity("++", 5, right_fix);

  P.def_function("==", lambda_expression( Equals() ) );
  P.declare_fixity("==", 5, non_fix);
  P.def_function("/=", lambda_expression( NotEquals() ) );
  P.declare_fixity("/=", 5, non_fix);
  P.def_function("<", lambda_expression( LessThan() ) );
  P.declare_fixity("<", 5, non_fix);
  //  P.declare_fixity("<=", 5, non_fix);
  P.def_function(">", lambda_expression( GreaterThan() ) );
  P.declare_fixity(">", 5, non_fix);
  //  P.declare_fixity(">=", 5, non_fix);
  
  //  P.declare_fixity("elem", 4, non_fix);
  //  P.declare_fixity("notElem", 4, non_fix);

  //  P.declare_fixity("&&", 3, right_fix);
  //  P.declare_fixity("||", 2, right_fix);

  //  P.declare_fixity(">>", 1, left_fix);
  //  P.declare_fixity(">>=", 1, left_fix);

  P.declare_fixity("$", 0, right_fix);
  P += "{f $ x = f x}";

  //  P.declare_fixity("$!", 0, right_fix);
  P.def_function("seq", lambda_expression( Seq() ) );
  P.declare_fixity("seq", 0, right_fix);
  P.declare_fixity("join", 0, right_fix);

  P += "{foldr f z [] = z;\
         foldr f z (x:xs) = (f x (foldr f z xs))}";

  P += "{foldl f z [] = z;\
         foldl f z (x:xs) = foldl f (f z x) xs}";

  P += "{foldl' f z [] = z;\
         foldl' f z (x:xs) = let {z' = (f z x)} in seq z' (foldl' f z' xs)}";

  P += "{head (h:t) = h}";
  P += "{tail (h:t) = t}";

  P += "{take 0 x     = [];\
         take n []    = [];\
         take n (h:t) = h:(take (n-1) t)}";

  P += "{repeat x = x:(repeat x)}";

  P += "{iterate f x = x:iterate f (f x)}";

  P += "{map f []  = [];\
         map f (h:t) = (f h):(map f t)}";
  
  P += "{fmap = map}";

  P += "{[] ++ y = y;\
         h:t ++ y = h:(t ++ y)}";

  P += "{infixr 9 .;\
         (f . g) x = f (g x)}";

  P += "{fst (x,y) = x}";

  P += "{snd (x,y) = y}";

  P += "{swap (x,y) = (y,x)}";

  P.declare_fixity("!!", 9, left_fix);
  P += "{h:t !! 0 = h;\
         h:t !! i = t !! (i-1)}";

  P += "{fmap1 f [] = [];\
         fmap1 f ((x,y):l) = (f x,y):(fmap1 f l);\
         fmap1 f (DiscreteDistribution l) = DiscreteDistribution (fmap1 f l)}";

  P += "{fmap2 f [] = [];\
         fmap2 f ((x,y):l) = (x,f y):(fmap2 f l);\
         fmap2 f (DiscreteDistribution l) = DiscreteDistribution (fmap2 f l)}";

  P += "{extendDiscreteDistribution (DiscreteDistribution d) p x = DiscreteDistribution (p,x):(fmap1 (\\q->q*(1.0-p)) d)}";

  P += "{sum  = foldl' (+) 0}";

  P += "{average (DiscreteDistribution l) = foldl' (\\x y->(x+(fst y)*(snd y))) 0.0 l}";

  // [ We could do this as two nested fmaps, instead. ]
  // [ We could factor out to_double(v2), and 1.0/to_double(v2)
  P += "{uniformDiscretize q n = let {n' = (intToDouble n)} in DiscreteDistribution (map (\\i->(1.0/n',q (2.0*i+1.0)/n')) (take n [0..]))}";

  // FIXME - we have an problem with types here.  This will only work for Int, as-is.
  P += "{enumFrom x = x:(enumFrom (x+1))}";
  P += "{enumFromTo x y = if (x==y) then [x] else x:(enumFromTo (x+1) y)}";
  //  P += "{enumFromThen x y = ... }";
  //  P += "{enumFromThenTo x y z = ... }";

  P += "{zip (x:xs) (y:ys) = (x,y):(zip xs ys);\
         zip []   _        = [];\
         zip _   []        = []}";

  P += "{concat xs = foldr (++) [] xs}";

  P += "{concatMap f = concat . map f}";

  P += "{length l = foldl' (\\x y ->(x+1)) 0 l}";

  P += "{listArray n l = mkArray n (\\i -> l !! i)}";

  P += "{listArray' l = listArray (length l) l}";

  P.def_function("doubleToLogDouble", lambda_expression( Conversion<double,log_double_t>() ) );

  P += "{uniformQuantiles q n = map (\\i -> q ((2.0*(intToDouble i)+1.0)/(intToDouble n)) ) (take n [1..])}";

  P += "{unwrapDD (DiscreteDistribution l) = l}";

  P += "{mixDiscreteDistributions' (h:t) (h2:t2) = DiscreteDistribution (fmap1 (\\q->q*h) h2)++(mixDiscreteDistributions' t t2);\
         mixDiscreteDistributions' [] [] = []}";

  P += "{mixDiscreteDistributions l1 l2 = DiscreteDistribution (mixDiscreteDistributions' l1 (fmap unwrapDD l2))}";

  P.def_function("sizeOfVectorUnsigned", lambda_expression( VectorSizeOp<unsigned>() ) );
  //--------------------------------------- listFromVectorInt ----------------------------------------//
  P.def_function("getVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<int,Int>() ) ); 
  P.def_function("sizeOfVectorInt", lambda_expression( VectorSizeOp<int>() ) );


  P += "{listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):listFromVectorInt' v s (i+1) else []}";

  P += "{listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0}";

  //--------------------------------------- listFromString ----------------------------------------//
  P.def_function("getStringElement", lambda_expression( BuiltinGetStringIndexOp() ) ); 
  P.def_function("sizeOfString", lambda_expression( StringSizeOp() ) );


  P += "{listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else []}";

  P += "{listFromString v = listFromString' v (sizeOfString v) 0}";

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorVectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<Vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorVectorInt", lambda_expression( VectorSizeOp<Vector<int>>() ) );

  P += "{listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):listFromVectorVectorInt' v s (i+1) else []}";

  P += "{listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0}";

  //--------------------------------------- listFromVectorVectorInt ----------------------------------------//
  P.def_function("getVectorvectorIntElement", lambda_expression( BuiltinGetVectorIndexOp<vector<int>,Vector<int>>() ) ); 
  P.def_function("sizeOfVectorvectorInt", lambda_expression( VectorSizeOp<vector<int>>() ) );

  P += "{listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):listFromVectorvectorInt' v s (i+1) else []}";

  P += "{listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0}";

  //--------------------------------------- listToVectorInt ---------------------------------------//

  P.def_function("builtinNewVectorInt", lambda_expression( BuiltinNewVectorOp<int>() ) ); 
  P.def_function("builtinSetVectorIndexInt", lambda_expression( BuiltinSetVectorIndexOp<int,Int>() ) ); 

  P += "{newVectorInt s = IOAction1 builtinNewVectorInt s}";

  P += "{setVectorIndexInt v i x = IOAction3 builtinSetVectorIndexInt v i x}";

  // copyListToVectorInt h:t v i = do { setVectorIndexInt v i h ; copyListToVectorInt t v (i+1) }
  P += "{copyListToVectorInt [] v i = IOReturn ();\
         copyListToVectorInt (h:t) v i = IOAnd (setVectorIndexInt v i h) (copyListToVectorInt t v (i+1))}";

  // listToVectorInt l = do { v <- newVectorInt (length l); copyListToVector l v 0 ; return v }
  P += "{listToVectorInt l = unsafePerformIO (IOAndPass (newVectorInt (length l)) (\\v -> IOAnd (copyListToVectorInt l v 0) (IOReturn v)))}";

  //--------------------------------------- listToString ---------------------------------------//

  P.def_function("builtinNewString", lambda_expression( BuiltinNewStringOp() ) ); 
  P.def_function("builtinSetStringIndexInt", lambda_expression( BuiltinSetStringIndexOp() ) ); 

  P += "{newString s = IOAction1 builtinNewString s}";

  P += "{setStringIndexInt v i x = IOAction3 builtinSetStringIndexInt v i x}";

  // copyListToString h:t v i = do { setStringIndexInt v i h ; copyListToString t v (i+1) }
  P += "{copyListToString [] v i = IOReturn ();\
         copyListToString (h:t) v i = IOAnd (setStringIndexInt v i h) (copyListToString t v (i+1))}";

  // listToString l = do { v <- newString (length l); copyListToString l v 0 ; return v }
  P += "{listToString l = unsafePerformIO (IOAndPass (newString (length l)) (\\v -> IOAnd (copyListToString l v 0) (IOReturn v)))}";

  //--------------------------------------- listToVectorDouble ---------------------------------------//

  P.def_function("builtinNewVectorDouble", lambda_expression( BuiltinNewVectorOp<double>() ) ); 
  P.def_function("builtinSetVectorIndexDouble", lambda_expression( BuiltinSetVectorIndexOp<double,Double>() ) ); 

  P += "{newVectorDouble s = IOAction1 builtinNewVectorDouble s}";

  P += "{setVectorIndexDouble v i x = IOAction3 builtinSetVectorIndexDouble v i x}";

  // copyListToVectorDouble h:t v i = do { setVectorIndexDouble v i h ; copyListToVectorDouble t v (i+1) }
  P += "{copyListToVectorDouble [] v i = IOReturn ();\
         copyListToVectorDouble (h:t) v i = IOAnd (setVectorIndexDouble v i h) (copyListToVectorDouble t v (i+1))}";

  // listToVectorDouble l = do { v <- newVectorDouble (length l); copyListToVector l v 0 ; return v }
  P += "{listToVectorDouble l = unsafePerformIO (IOAndPass (newVectorDouble (length l)) (\\v -> IOAnd (copyListToVectorDouble l v 0) (IOReturn v)))}";

  //--------------------------------------- listToVectorMatrix ---------------------------------------//

  P.def_function("builtinNewVectorMatrix", lambda_expression( BuiltinNewVectorOp<Matrix>() ) ); 
  P.def_function("builtinSetVectorIndexMatrix", lambda_expression( BuiltinSetVectorIndexOp<Matrix,MatrixObject>() ) ); 

  P += "{newVectorMatrix s = IOAction1 builtinNewVectorMatrix s}";

  P += "{setVectorIndexMatrix v i x = IOAction3 builtinSetVectorIndexMatrix v i x}";

  // copyListToVectorMatrix h:t v i = do { setVectorIndexMatrix v i h ; copyListToVectorMatrix t v (i+1) }
  P += "{copyListToVectorMatrix [] v i = IOReturn ();\
         copyListToVectorMatrix (h:t) v i = IOAnd (setVectorIndexMatrix v i h) (copyListToVectorMatrix t v (i+1))}";

  // listToVectorMatrix l = do { v <- newVectorMatrix (length l); copyListToVector l v 0 ; return v }
  P += "{listToVectorMatrix l = unsafePerformIO (IOAndPass (newVectorMatrix (length l)) (\\v -> IOAnd (copyListToVectorMatrix l v 0) (IOReturn v)))}";

  return P;
}

const Program& get_Prelude()
{
  static const Program P = make_Prelude();
  return P;
}
