module Prelude (module Prelude,
                module Data.Bool,
                module Data.Eq,
                module Data.Either,
                module Data.Tuple,
                module Data.Maybe,
                module Data.List,
                module Data.Array,
                module Data.Function,
                module Data.Ord,
                module Foreign.Pair,
                module Foreign.Vector,
                module Foreign.String,
                module Control.Monad,
                module Compiler.Base,
                module Compiler.IO,
                module Compiler.ST,
                module Compiler.Num,
                module Compiler.Real,
                module Compiler.Enum)
    where

import Compiler.Base
import Compiler.IO
import Compiler.ST
import Compiler.Num
import Compiler.Real
import Compiler.Enum
import Data.Bool
import Data.Either
import Data.Eq
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Array
import Data.Function
import Data.Ord
import Control.Monad
import Foreign.Pair
import Foreign.Vector
import Foreign.String

builtin builtin_putStrLn 2 "putStrLn" "Prelude"

builtin is_char 1 "is_char" "Prelude"
builtin is_double 1 "is_double" "Prelude"
builtin is_int 1 "is_int" "Prelude"

builtin builtin_show 1 "show" "Prelude"
builtin builtin_read_int 1 "read_int" "Prelude"
builtin builtin_read_double 1 "read_double" "Prelude"

fmap = map

undefined = error "Prelude.undefined"

-- zipWith' enforces equal lengths, unlike zipWith
zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith' _ [] []         =  []

zip' = zipWith' (,)

putStrLn line = IOAction (pair_from_c . builtin_putStrLn (list_to_string line))

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
read_int (h:t) = builtin_read_int (list_to_string (h:t))
read_int s = builtin_read_int s

read_double [] = error "Can't convert empty string to double."
read_double (h:t) = builtin_read_double (list_to_string (h:t))
read_double s = builtin_read_double s

builtin builtin_readFile 1 "readFile" "Data"
readFile filename = IOAction (\s -> (s,listFromString $ builtin_readFile $ list_to_string $ filename))
