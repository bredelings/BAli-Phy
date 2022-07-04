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
                module Compiler.Error,
                module Compiler.Base,
                module Compiler.IO,
                module Compiler.ST,
                module Compiler.Num,
                module Compiler.Real,
                module Compiler.Enum)
    where

import Compiler.Error
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

foreign import bpcall "Prelude:putStrLn" builtin_putStrLn :: CPPString -> RealWorld -> EPair RealWorld ()
putStrLn line = IOAction (pair_from_c . builtin_putStrLn (list_to_string line))

foreign import bpcall "Prelude:" read_int :: CPPString -> Int
foreign import bpcall "Prelude:" read_double :: CPPString -> Double

undefined = error "Prelude.undefined"

-- zipWith' enforces equal lengths, unlike zipWith
zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith' _ [] []         =  []

zip' = zipWith' (,)


class Show a where
    show :: a -> [Char]
    showList :: [a] -> [Char]

    showList [] = "[]"
    showList (x:y) = "["++show x++show' y++"]" where show' [] = ""
                                                     show' (x:y) = ", "++show x++show' y

foreign import bpcall "Prelude:" show_int :: Int -> CPPString
foreign import bpcall "Prelude:" show_double :: Double -> CPPString

instance Show Char where
    show  c = ['\'',c,'\'']
    showList s = "\"" ++ s ++ "\""

instance Show Int where
    show i = unpack_cpp_string $ show_int i

instance Show Double where
    show  d = unpack_cpp_string $ show_double d

instance Show () where
    show _ = "()"

instance (Show a, Show b) => Show (a,b) where 
    show (x,y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance (Show a, Show b, Show c) => Show (a,b,c) where 
    show (x,y,z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance (Show a, Show b, Show c, Show d) => Show (a,b,c,d) where 
    show (x,y,z,w) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show w ++ ")"

instance Show a => Show [a] where
    show s = showList s

instance Show a => Show (Maybe a) where
    show (Just x) = "Just "++show x
    show Nothing = "Nothing"

instance Show Bool where                   
    show True = "True"
    show False = "False"

class Read a where
    read :: [Char] -> a

instance Read Int where
    read [] = error "Can't convert empty string to int."
    read s = read_int (list_to_string s)

instance Read Double where
    read [] = error "Can't convert empty string to double."
    read s = read_double (list_to_string s)

foreign import bpcall "Data:readFile" builtin_readFile :: CPPString -> CPPString
readFile filename = IOAction (\s -> (s,listFromString $ builtin_readFile $ list_to_string $ filename))
