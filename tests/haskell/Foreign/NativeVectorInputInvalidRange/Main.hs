{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal
    (doubleVectorNativeView, intVectorNativeView)
import Foreign.NativeVector (NativeVector)
import Numeric.LogDouble (LogDouble)
import System.IO (print)

foreign import bpcall "Distribution:multinomial_density"
    invalidRange :: Int -> Int -> Int -> NativeVector Double
                 -> Int -> Int -> NativeVector Int -> LogDouble

main = print (invalidRange 1 2 1 probabilityOwner
                             countOffset countLength countOwner)
  where
    (_, _, probabilityOwner) = doubleVectorNativeView (U.fromList [1])
    (countOffset, countLength, countOwner) = intVectorNativeView
                                                 (U.fromList [1])
