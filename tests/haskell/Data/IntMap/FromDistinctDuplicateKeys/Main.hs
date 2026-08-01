{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import qualified Data.IntMap as IM
import Data.Function (($))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO (print)

-- Force construction from duplicate keys so the distinct-key contract is
-- checked without demanding either boxed value.
main = print $ IM.size $ IM.fromDistinctKeysAndValues
  (U.fromList [4, 4]) (V.fromList [10, 20])
