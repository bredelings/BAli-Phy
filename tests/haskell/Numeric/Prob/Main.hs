{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional (Fractional(recip), (/))
import Compiler.Floating (Pow(ln, pow, expTo), exp, log, log1p)
import Compiler.Integral ((^))
import Compiler.Num (Num(..))
import Compiler.RealFloat (isNaN)
import Data.Bool ((&&))
import Data.Eq ((==))
import Data.Floating.Types (FloatConvert(toFloating))
import Data.Ord (Ordering(LT), compare, max, min, (<), (<=))
import Numeric.Prob (LogDouble, Prob, complement, fromLogOdds, isNaNProb, logOdds)
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
      largeProb = toFloating largeInteger :: Prob
      threeFromInt = toFloating (3 :: Int) :: Prob
  print (near (ln largeProb) (1100 * log 2), threeFromInt == (3 :: Prob))
  let p = fromLogOdds (-0.25)
      tiny = fromLogOdds (-1000)
      p40 = fromLogOdds 40
      p41 = fromLogOdds 41
  print
    ( ( near (toFloating (p+p) :: Double) 0.8756469982284039
      , near (ln (tiny+tiny)) (-1000+log 2)
      , near (ln (recip p40-p40)) (-40+log (2+exp(-40))-log1p(exp(-40)))
      , near (ln (recip p40-recip p41)) (-40+log1p(-exp(-1)))
      )
    , near (toFloating ((3 :: Prob)-(2 :: Prob)) :: Double) 1
    )
  let base = toFloating (0.4 :: Double) :: Prob
  print
    ( ( near (ln (pow base 2000)) (2000*log 0.4)
      , near (toFloating (pow base (-2)) :: Double) 6.25
      , near (logOdds (pow base 1.0e-300)) (negate (log 1.0e-300+log (negate (log 0.4))))
      )
    , ( near (logOdds (pow (fromLogOdds 1000) 2)) (1000-log 2)
      , near (toFloating (pow (2 :: Prob) 3) :: Double) 8
      )
    )
  let nanDouble = 0/0 :: Double
      nan = toFloating nanDouble :: Prob
      nanLog = toFloating nan :: LogDouble
  print
    ( isNaNProb nan
    , nan == nan
    , nan < zero
    , compare nan zero == LT
    )
  print
    ( isNaN (toFloating nan :: Double)
    , isNaN (ln nanLog)
    , isNaNProb (fromLogOdds nanDouble)
    , isNaNProb (toFloating nanLog :: Prob)
    )
  print
    ( nan < zero && zero < half && half < one && one < two && two < infinity
    , nan <= nan
    , min nan zero == nan
    , max nan zero == zero
    )
  print
    ( isNaNProb (nan + one)
    , isNaNProb (one - nan)
    , isNaNProb (nan * zero)
    , isNaNProb (recip nan)
    )
  print
    ( isNaNProb (complement nan)
    , isNaNProb (signum nan)
    , isNaNProb (infinity - infinity)
    , isNaNProb (zero * infinity)
    )
  print
    ( isNaNProb (zero / zero)
    , isNaNProb (infinity / infinity)
    , isNaNProb (pow nan 2)
    , isNaNProb (pow half nanDouble)
    )
  print
    ( pow nan 0 == one
    , isNaNProb (expTo nanDouble :: Prob)
    , isNaN (logOdds nan)
    )
