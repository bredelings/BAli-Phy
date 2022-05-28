module Prelude (module Prelude,
                module Data.Bool,
                module Data.Eq,
                module Data.Either,
                module Data.Tuple,
                module Data.Maybe,
                module Data.List,
                module Data.Array,
                module Data.Function,
                module Data.Functor,
                module Data.Ord,
                module Foreign.Pair,
                module Foreign.Vector,
                module Foreign.String,
                module Control.Applicative,
                module Control.Monad,
                module Compiler.Base,
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
import Data.Functor
import Data.Ord
import Control.Applicative
import Control.Monad
import Foreign.Pair
import Foreign.Vector
import Foreign.String

foreign import bpcall "Prelude:putStrLn" builtin_putStrLn :: () -> () -> ()

foreign import bpcall "Prelude:is_char" is_char :: () -> ()
foreign import bpcall "Prelude:is_double" is_double :: () -> ()
foreign import bpcall "Prelude:is_int" is_int :: () -> ()

foreign import bpcall "Prelude:show" builtin_show :: () -> ()
foreign import bpcall "Prelude:read_int" builtin_read_int :: () -> ()
foreign import bpcall "Prelude:read_double" builtin_read_double :: () -> ()

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

foreign import bpcall "Data:readFile" builtin_readFile :: () -> ()
readFile filename = IOAction (\s -> (s,listFromString $ builtin_readFile $ list_to_string $ filename))
