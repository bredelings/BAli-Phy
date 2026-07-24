{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional (Fractional(recip), (/))
import Compiler.Floating (Pow(ln), log)
import Compiler.Integral ((^))
import Compiler.Num (Num(..))
import Data.Bool ((&&))
import Data.Eq ((==))
import Data.Floating.Types (FloatConvert(toFloating))
import Data.Ord ((<))
import Numeric.Prob (LogDouble, Prob, fromLogOdds, logOdds)
import System.IO (IO, print)

near x y = abs (x-y) < 1.0e-12

-- Check canonical conversions, ordering, multiplication, and large integers.
main :: IO ()
main = do
  print
    ( toFloating (fromLogOdds 0) :: Double
    , fromLogOdds (-1 / 0) == (0 :: Prob)
    , fromLogOdds (1 / 0) == (1 :: Prob)
    )
  print
    ( logOdds (fromLogOdds (-1 / 0)) == (-1 / 0)
    , logOdds (fromLogOdds (1 / 0)) == (1 / 0)
    )
  let zero = (0 :: Prob)
      one = (1 :: Prob)
      half = fromLogOdds 0
      two = recip half
      infinity = recip zero
      zeroFromLog = toFloating (0 :: LogDouble) :: Prob
      oneFromLog = toFloating (1 :: LogDouble) :: Prob
      infinityFromLog = toFloating (1/0 :: LogDouble) :: Prob
  print
    ( zeroFromLog == zero
    , oneFromLog == one
    , infinityFromLog == infinity
    , zero < half && half < one && one < two && two < infinity
    )
  print
    ( near (toFloating (half * half) :: Double) 0.25
    , near (toFloating (half * two) :: Double) 1.0
    , near (toFloating (two * (3 :: Prob)) :: Double) 6.0
    )
  let largeInteger = (2 :: Integer) ^ (1100 :: Int)
      largeProb = fromInteger largeInteger :: Prob
  print (near (ln largeProb) (1100 * log 2))
