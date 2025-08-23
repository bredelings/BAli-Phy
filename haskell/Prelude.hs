module Prelude (
        module Prelude,

        Bool(False,True), (&&), (||), not, otherwise,

        Maybe(Nothing, Just), maybe,

        module System.IO,
             
        module Data.Eq,
        module Data.Either,
        module Data.Tuple,
        module Data.List,
        module Data.Function,

        Functor(fmap, (<$)), (<$>),

        module Data.Foldable,
        module Data.Ord,
        module Data.IORef,
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
        module Compiler.Ratio,
        module Compiler.Real,
        module Compiler.RealFrac,
        module Compiler.RealFloat,
        module Compiler.Enum,
        module Compiler.Classes
  ) where

import Compiler.Base
import Compiler.Classes
import Compiler.Enum
import Compiler.Error
import Compiler.Floating
import Compiler.Fractional
import Compiler.IO
import Compiler.Integral
import Compiler.Num
import Compiler.Ratio
import Compiler.Real
import Compiler.RealFrac
import Compiler.RealFloat
import Compiler.ST
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
import Data.IORef
import Text.Show
import Text.Read
import Control.Applicative
import Control.Monad
import Foreign.Pair
import Foreign.Vector
import Foreign.String
import System.IO (putChar, putStr, putStrLn, print, getChar, getLine, getContents, interact,
                  FilePath,readFile, writeFile, appendFile, readIO, readLn)

import Compiler.Translate -- ensure this is typechecked
    
undefined = error "Prelude.undefined"

-- zipWith' enforces equal lengths, unlike zipWith
zipWith' z (a:as) (b:bs) =  z a b : zipWith z as bs
zipWith' _ [] []         =  []

zip' = zipWith' (,)

foreign import bpcall "Vector:showObject" showVector :: EVector a -> CPPString
instance Show (EVector a) where
    show = unpack_cpp_string . showVector


