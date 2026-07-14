{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (intVectorNativeView)
import Foreign.NativeVector (NativeVector)
import Numeric.LogDouble (LogDouble)
import System.IO (print)

foreign import bpcall "Distribution:multinomial_density"
    wrongOwner :: Int -> Int -> Int -> NativeVector Int
               -> Int -> Int -> NativeVector Int -> LogDouble

main = print (wrongOwner 1 offset count owner offset count owner)
  where
    (offset, count, owner) = intVectorNativeView (U.fromList [1])
