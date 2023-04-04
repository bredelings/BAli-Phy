module Prelude (module Prelude,
                module Data.Bool,
                module Data.Eq,
                module Data.Either,
                module Data.Tuple,
                module Data.Maybe,
                module Data.List,
                module Data.Function,
                module Data.Functor,
                module Data.Foldable,
                module Data.Ord,
                module Text.Show,
                module Text.Read,
                module Foreign.Pair,
                module Foreign.Vector,
                module Foreign.String,
                module Control.Applicative,
                module Control.Monad,
                module Compiler.Base,
                module Compiler.Error,
                module Compiler.Floating,
                module Compiler.Fractional,
                module Compiler.IO,
                module Compiler.Integral,
                module Compiler.ST,
                module Compiler.Num,
                module Compiler.Real,
                module Compiler.RealFrac,
                module Compiler.Enum,
                module Compiler.Classes)
    where

import Compiler.Error
import Compiler.Floating
import Compiler.Fractional
import Compiler.Base
import Compiler.IO
import Compiler.ST
import Compiler.Integral
import Compiler.Num
import Compiler.Real
import Compiler.RealFrac
import Compiler.Enum
import Compiler.Classes
import Data.Bool
import Data.Either
import Data.Eq
import Data.Tuple
import Data.Maybe
import Data.List
import Data.Foldable hiding (fold, foldMap', toList, foldl', foldr')
import Data.Function
import Data.Functor
import Data.Ord
import Text.Show
import Text.Read
import Control.Applicative
import Control.Monad
import Foreign.Pair
import Foreign.Vector
import Foreign.String

foreign import bpcall "Prelude:putStrLn" builtin_putStrLn :: CPPString -> RealWorld -> ()
putStrLn line = makeIO $ builtin_putStrLn (list_to_string line)

undefined = error "Prelude.undefined"

-- zipWith' enforces equal lengths, unlike zipWith
zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith' _ [] []         =  []

zip' = zipWith' (,)


foreign import bpcall "Data:readFile" builtin_readFile :: CPPString -> RealWorld -> CPPString
readFile filename = makeIO $ builtin_readFile (list_to_string $ filename)


instance Show LogDouble where
    show x = show $ exp $ ln x
