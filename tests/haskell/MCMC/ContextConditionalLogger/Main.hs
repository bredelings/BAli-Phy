{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Eq ((==))
import Data.Function (($))
import Data.JSON (Key)
import Data.Ord ((>=))
import MCMC (binaryIndicatorFields, runMCMC)
import Probability.Distribution.Uniform (uniform)
import Probability.Logger (makeJSONLogger)
import Probability.Random
  (LoggerValues(..), (%=%), (%>!), addLogger, condition, contextFields, makeMCMCState, modifiable,
   parameterLogValues, prefixContextFields, prior)
import SModel (positiveSelectionFields)
import System.IO (IO, stdout)

-- Log a conditional probability whose alternate selector state creates a random variable.
model = do
  let selector = modifiable (0 :: Int)
  x <- if selector == 0 then return 0.0 else prior $ uniform 0 1
  condition (x >= 0)
  let loggerValues =
        LoggerValues
          [("selector" :: Key) %=% selector]
          (contextFields
            [ "S1" %>! prefixContextFields "m3_test:" (positiveSelectionFields selector)
            , "S2" %>! prefixContextFields "m3_test:" (binaryIndicatorFields "BranchDifference" selector)
            ])
  addLogger $ makeJSONLogger stdout loggerValues
  return $ parameterLogValues loggerValues

main :: IO ()
main = do
  mcmcState <- makeMCMCState model
  runMCMC 1 mcmcState
