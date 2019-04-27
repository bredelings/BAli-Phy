module Prelude (module Prelude,
                module Data.Bool,
                module Data.Eq,
                module Data.Tuple,
                module Data.Maybe,
                module Data.List,
                module Data.Array,
                module Data.Function,
                module Data.Ord,
                module Foreign.Vector,
                module Foreign.String,
                module Control.Monad,
                module Compiler.Base,
                module Compiler.Num,
                module Compiler.Real,
                module Compiler.Enum)
    where

import Compiler.Base
import Compiler.Num
import Compiler.Real
import Compiler.Enum
import Data.Bool
import Data.Eq
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Array
import Data.Function
import Data.Ord
import Control.Monad
import Foreign.Vector
import Foreign.String

builtin reapply 2 "reapply" "Prelude"
builtin iotaUnsigned 1 "iotaUnsigned" "Prelude"
builtin builtin_putStrLn 1 "putStrLn" "Prelude"

builtin is_char 1 "is_char" "Prelude"
builtin is_double 1 "is_double" "Prelude"
builtin is_int 1 "is_int" "Prelude"

builtin c_fst 1 "c_fst" "Pair"
builtin c_snd 1 "c_snd" "Pair"
builtin c_pair' 2 "c_pair" "Pair"
builtin builtin_show 1 "show" "Prelude"
builtin builtin_read_int 1 "read_int" "Prelude"
builtin builtin_read_double 1 "read_double" "Prelude"

fmap = map

undefined = error "Prelude.undefined"

-- zipWith' enforces equal lengths, unlike zipWith
zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith' _ [] []         =  []

zip' = zipWith' (,)

c_pair (x,y) = c_pair' x y

pair_from_c p = (c_fst p, c_snd p)

putStrLn line = IOAction1 builtin_putStrLn (listToString line)


newString s = IOAction1 builtinNewString s

setStringIndexInt v i x = IOAction3 builtinSetStringIndexInt v i x

copyListToString [] v i = return ()
copyListToString (h:t) v i = do setStringIndexInt v i h
                                copyListToString t v (i+1)

listToString l = runST $ do v <- newString (length l)
                            copyListToString l v 0
                            return v

pack_cpp_string = listToString

unsafePerformIO (IOAction1 x y ) = x y
unsafePerformIO (IOAction2 x y z) = x y z
unsafePerformIO (IOAction3 x y z w) = x y z w
unsafePerformIO (IOAction4 x y z w u) = x y z w u
unsafePerformIO (LazyIO f) = unsafePerformIO f
unsafePerformIO (IOAndPass (LazyIO f) g) = let x = unsafePerformIO f in unsafePerformIO (g x)
unsafePerformIO (IOAndPass f g) = let x = unsafePerformIO f in x `join` unsafePerformIO (g x)
unsafePerformIO (MFix f) = let x = unsafePerformIO (f x) in x
unsafePerformIO (IOReturn x) = x

unsafeInterleaveIO x = LazyIO x
mfix f = MFix f
runST x = reapply unsafePerformIO x

quicksort [] = []
quicksort (x:xs) = quicksort small ++ (x : quicksort large)
   where small = [y | y <- xs, y <= x ]
         large = [y | y <- xs, y  > x ]

quicksortWith f [] = []
quicksortWith f (x:xs) = quicksortWith f small ++ (x : quicksortWith f large)
   where small = [y | y <- xs, (f y) <= (f x)]
         large = [y | y <- xs, (f y)  > (f x)]
  
show () = "()"
show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"
show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"
show (x,y,z,w) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w ++ ")"
show [] = "[]"
show (x:y) = "["++show x++show' y++"]" where show' [] = ""
                                             show' (x:y) = ","++show x++show' y
show (Just x) = "Just "++show x
show Nothing = "Nothing"
show True = "True"
show False = "False"
show x     = listFromString $ builtin_show x

read_int [] = error "Can't convert empty string to int."
read_int (h:t) = builtin_read_int (listToString (h:t))
read_int s = builtin_read_int s

read_double [] = error "Can't convert empty string to double."
read_double (h:t) = builtin_read_double (listToString (h:t))
read_double s = builtin_read_double s

