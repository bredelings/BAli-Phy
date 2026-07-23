{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional ((/))
import Compiler.Num (Num(..))
import Data.Eq ((==))
import Data.Floating.Types (FloatConvert(toFloating))
import Numeric.Prob (Prob, fromLogOdds, logOdds)
import System.IO (IO, print)

-- Check finite conversion and canonical behavior at both infinite endpoints.
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
