{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal
    (intVectorNativeView)
import qualified Data.Vector as V
import Foreign.NativeVector (NativeVector)
import Numeric.Log (Log)
import System.IO (print)

foreign import bpcall "Distribution:multinomial_density"
    invalidRange :: Int -> V.Vector (Log Double)
                 -> Int -> Int -> NativeVector Int -> Log Double

main = print (invalidRange 1 probabilities 2 1 countOwner)
  where
    probabilities = V.fromList [1]
    (_, _, countOwner) = intVectorNativeView (U.fromList [1])
