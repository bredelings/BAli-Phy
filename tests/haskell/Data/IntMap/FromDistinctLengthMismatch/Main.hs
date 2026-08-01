{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import qualified Data.IntMap as IM
import Data.Function (($))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Force construction from unequal logical lengths so the interface rejects
-- the inputs before indexing either collection.
main = print $ IM.size $ IM.fromDistinctKeysAndValues
  (U.fromList [1, 2]) (V.fromList [10])
