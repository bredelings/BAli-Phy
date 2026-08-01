{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import qualified Data.IntMap as IM
import Data.Function (($))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import System.IO (IO)

assert condition message = if condition then return () else error message

-- Check positional alignment for a sliced, nonascending key view and verify
-- that constructing a map does not force an unselected boxed value.
main :: IO ()
main = do
  let keys = U.slice 1 3 (U.fromList [99, 7, -2, 5, 100])
      values = V.fromList [70, -20, 50]
      aligned = IM.fromDistinctKeysAndValues keys values
      empty = IM.fromDistinctKeysAndValues U.empty V.empty :: IM.IntMap Int
      lazyValues = V.fromList [10, error "unselected IntMap value was forced"]
      lazyMap = IM.fromDistinctKeysAndValues (U.fromList [1, 2]) lazyValues
  assert (IM.size aligned == 3) "aligned-vector constructor returned the wrong size"
  assert (aligned IM.! 7 == 70 && aligned IM.! (-2) == (-20) && aligned IM.! 5 == 50)
         "aligned-vector constructor changed key/value positions"
  assert (IM.size empty == 0) "aligned-vector constructor did not preserve emptiness"
  assert (lazyMap IM.! 1 == 10) "aligned-vector constructor returned the wrong lazy value"
