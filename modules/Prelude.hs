module Prelude (module Prelude,
                module Data.Bool,
                module Data.Tuple,
                module Data.Maybe,
                module Data.List,
                module Data.Function,
                module Data.Ord,
                module Control.Monad,
                module Compiler.Base,
                module Compiler.Num)
    where
{
import Compiler.Base;
import Compiler.Num;
import Data.Bool;
import Data.Tuple;
import Data.Maybe;
import Data.List;
import Data.Function;
import Data.Ord;
import Control.Monad;

infixl 8 ^, ^^, **;
infixl 7 `div`, `mod`, `rem`, `quot`;
infix 4 ==, /=;

infixr 9 !;

x ^ 0 = 1;
x ^ 1 = x;
x ^ n = x*(x^(n-1));

-- not defined
x ^^ y = error "'^^' not defined";
x `rem` y = error "'rem' not defined";
x `div` y = error "'div' not defined";
x `quot` y = error "'quot' not defined";

builtin log 1 "log" "Prelude";
builtin sqrt 1 "sqrt" "Prelude";
builtin truncate 1 "truncate" "Prelude";
builtin ceiling 1 "ceiling" "Prelude";
builtin floor 1 "floor" "Prelude";
builtin round 1 "round" "Prelude";
builtin builtin_vector_from_list 1 "vector_from_list" "Prelude";
builtin doubleToInt 1 "doubleToInt" "Prelude";
builtin ** 2 "pow" "Prelude";
builtin mod 2 "mod" "Prelude";
builtin reapply 2 "reapply" "Prelude";
builtin arraySize 1 "arraySize" "Array";
builtin ! 2 "getIndex" "Array";
builtin mkArray 2 "mkArray" "Array";
builtin intToDouble 1 "intToDouble" "Prelude";
builtin negate 1 "negate" "Prelude";
builtin exp 1 "exp" "Prelude";
builtin doubleToLogDouble 1 "doubleToLogDouble" "Prelude";
builtin builtin_equals 2 "equals" "Prelude";
builtin /= 2 "notequals" "Prelude";
builtin iotaUnsigned 1 "iotaUnsigned" "Prelude";
builtin builtin_putStrLn 1 "putStrLn" "Prelude";
builtin builtin_set_vector_index 3 "set_vector_index" "Vector";
builtin builtin_new_vector 1 "new_vector" "Vector";
builtin get_vector_index 2 "get_vector_index" "Vector";
builtin vector_size 1 "vector_size" "Vector";
builtin sizeOfVectorUnsigned 1 "sizeOfVectorUnsigned" "Vector";
builtin sizeOfVectorInt 1 "sizeOfVectorInt" "Vector";
builtin sizeOfVectorVectorInt 1 "sizeOfVectorVectorInt" "Vector";
builtin sizeOfVectorvectorInt 1 "sizeOfVectorvectorInt" "Vector";
builtin getVectorIntElement 2 "getVectorIntElement" "Vector";
builtin getVectorVectorIntElement 2 "getVectorVectorIntElement" "Vector";
builtin getVectorvectorIntElement 2 "getVectorvectorIntElement" "Vector";
builtin getStringElement 2 "getStringElement" "Vector";
builtin sizeOfString 1 "sizeOfString" "Vector";
builtin builtinNewVectorInt 1 "NewVectorInt" "Vector";
builtin builtinSetVectorIndexInt 3 "SetVectorIndexInt" "Vector";
builtin builtinNewString 1 "NewString" "Vector";
builtin builtinSetStringIndexInt 3 "SetStringIndex" "Vector";
builtin builtinNewVectorDouble 1 "NewVectorDouble" "Vector";
builtin builtinSetVectorIndexDouble 3 "SetVectorIndexDouble" "Vector";
builtin builtinNewVectorMatrix 1 "NewVectorMatrix" "Vector";
builtin builtinSetVectorIndexMatrix 3 "SetVectorIndexMatrix" "Vector";

builtin c_fst 1 "c_fst" "Pair";
builtin c_snd 1 "c_snd" "Pair";
builtin c_pair' 2 "c_pair" "Pair";
builtin builtin_show 1 "show" "Prelude";
builtin builtin_read_int 1 "read_int" "Prelude";
builtin builtin_read_double 1 "read_double" "Prelude";

(x:xs) == (y:ys) = (x==y) && (xs == ys);
(_:_)  == _      = False;
_      == (_:_)  = False;
[]     == []     = True;
[]     == _      = False;
_      == []     = False;
x      == y      = builtin_equals x y;

fmap = map;

undefined = error "Prelude.undefined";

enumFrom x = x:(enumFrom (x+1));
enumFromTo x y = if (x>y) then [] else x:(enumFromTo (x+1) y);

zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs;
zipWith' _ [] []           =  [];

zip' = zipWith' (,);

listArray n l = mkArray n (\i -> l !! i);

listArray' l = listArray (length l) l;

list_from_vector' v s i = if (i<s) then (get_vector_index v i):list_from_vector' v s (i+1) else [];

list_from_vector v = list_from_vector' v (vector_size v) 0;

listFromVectorInt' v s i = if (i<s) then (getVectorIntElement v i):listFromVectorInt' v s (i+1) else [];

listFromVectorInt v = listFromVectorInt' v (sizeOfVectorInt v) 0;

listFromString' v s i = if (i<s) then (getStringElement v i):listFromString' v s (i+1) else [];

listFromString v = listFromString' v (sizeOfString v) 0;

listFromVectorVectorInt' v s i = if (i<s) then (getVectorVectorIntElement v i):listFromVectorVectorInt' v s (i+1) else [];

listFromVectorVectorInt v = listFromVectorVectorInt' v (sizeOfVectorVectorInt v) 0;

listFromVectorvectorInt' v s i = if (i<s) then (getVectorvectorIntElement v i):listFromVectorvectorInt' v s (i+1) else [];

listFromVectorvectorInt v = listFromVectorvectorInt' v (sizeOfVectorvectorInt v) 0;

c_pair (x,y) = c_pair' x y;

pair_from_c p = (c_fst p, c_snd p);

putStrLn line = IOAction1 builtin_putStrLn (listToString line);

new_vector s = IOAction1 builtin_new_vector s;

set_vector_index v i x = IOAction3 builtin_set_vector_index v i x;

copy_list_to_vector [] v i = return ();
copy_list_to_vector (h:t) v i = do {set_vector_index v i h; copy_list_to_vector t v (i+1)};

slow_list_to_vector l = runST $ do {v <- new_vector (length l); copy_list_to_vector l v 0; return v};
list_to_vector = builtin_vector_from_list;

newVectorInt s = IOAction1 builtinNewVectorInt s;

setVectorIndexInt v i x = IOAction3 builtinSetVectorIndexInt v i x;

copyListToVectorInt [] v i = return ();
copyListToVectorInt (h:t) v i = do {setVectorIndexInt v i h; copyListToVectorInt t v (i+1)};

listToVectorInt l = runST $ do {v <- newVectorInt (length l); copyListToVectorInt l v 0; return v};

newString s = IOAction1 builtinNewString s;

setStringIndexInt v i x = IOAction3 builtinSetStringIndexInt v i x;

copyListToString [] v i = return ();
copyListToString (h:t) v i = do {setStringIndexInt v i h ; copyListToString t v (i+1)};

listToString l = runST $ do {v <- newString (length l); copyListToString l v 0; return v};

newVectorDouble s = IOAction1 builtinNewVectorDouble s;
setVectorIndexDouble v i x = IOAction3 builtinSetVectorIndexDouble v i x;
copyListToVectorDouble [] v i = return ();
copyListToVectorDouble (h:t) v i = do { setVectorIndexDouble v i h ; copyListToVectorDouble t v (i+1)};

listToVectorDouble l = runST $ do { v <- newVectorDouble (length l); copyListToVectorDouble l v 0; return v};

newVectorMatrix s = IOAction1 builtinNewVectorMatrix s;
setVectorIndexMatrix v i x = IOAction3 builtinSetVectorIndexMatrix v i x;
copyListToVectorMatrix [] v i = return ();
copyListToVectorMatrix (h:t) v i = do { setVectorIndexMatrix v i h; copyListToVectorMatrix t v (i+1)};

listToVectorMatrix l = runST $ do { v <- newVectorMatrix (length l); copyListToVectorMatrix l v 0 ; return v};

unsafePerformIO (IOAction1 x y ) = x y;
unsafePerformIO (IOAction2 x y z) = x y z;
unsafePerformIO (IOAction3 x y z w) = x y z w;
unsafePerformIO (IOAction4 x y z w u) = x y z w u;
unsafePerformIO (LazyIO f) = unsafePerformIO f;
unsafePerformIO (IOAndPass (LazyIO f) g) = let {x = unsafePerformIO f} in (unsafePerformIO (g x));
unsafePerformIO (IOAndPass f g) = let {x = unsafePerformIO f} in x `seq` (unsafePerformIO (g x));
unsafePerformIO (IOReturn x) = x;

unsafeInterleaveIO x = LazyIO x;
runST x = reapply unsafePerformIO x;

quicksort [] = [];
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
   where { small = [y | y <- xs, y <= x ] ;
           large = [y | y <- xs, y  > x ] };

quicksortWith f [] = [];
quicksortWith f (x:xs) = quicksortWith f small ++ (x : quicksortWith f large)
   where { small = [y | y <- xs, (f y) <= (f x)] ;
           large = [y | y <- xs, (f y)  > (f x)] };
  
show () = "()";
show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")";
show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")";
show [] = "[]";
show (x:y) = "["++show x++show' y++"]" where {show' [] = "";
                                              show' (x:y) = ","++show x++show' y};
show (Just x) = "Just "++show x;
show Nothing = "Nothing";
show x     = listFromString $ builtin_show x;

read_int [] = error "Can't convert empty string to int.";
read_int (h:t) = builtin_read_int (listToString (h:t));
read_int s = builtin_read_int s;

read_double [] = error "Can't convert empty string to double.";
read_double (h:t) = builtin_read_double (listToString (h:t));
read_double s = builtin_read_double s;

-- These should be in Data.List, but use ==
nub = nubBy (==);

nubBy eq (x:xs) = x:nubBy eq (filter (\y -> not (eq x y)) xs);
nubBy eq [] = [];

infix 4 `elem`;
elem x           =  any (== x);

infix 4 `notElem`;
notElem x        =  all (/= x);

lookup key [] = Nothing;
lookup key ((k,v):kvs) = if (key == k) then Just v else lookup key kvs;
}
