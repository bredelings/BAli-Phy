{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional ((/))
import Compiler.Floating (Pow(expTo))
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Function (($))
import MCMC (runMCMC)
import MCMC.Moves.Integer (discreteUniformAvoidMH)
import MCMC.Moves.MH (metropolisHastings, propose)
import Probability.Distribution.Bernoulli (bernoulli)
import Probability.Distribution.Geometric (geometric)
import Probability.Distribution.NegativeBinomial (negativeBinomial)
import Probability.Random (addMove, makeMCMCModel, modifiable, sample, toProb)
import System.IO (IO, putStrLn)

-- Protect candidate-local density and sampling failures across the separate native and generic MH
-- transaction paths. This is obsolete when both paths share one typed proposal-rejection boundary.
model = do
  let densitySelector = modifiable (0 :: Int)
      samplingSelector = modifiable (0 :: Int)
      geometricValue = modifiable (0 :: Int)
      negativeBinomialValue = modifiable (0 :: Int)
  let probability = if densitySelector == 0 then 0.5 else 0/0
  _ <- sample $ bernoulli $ toProb probability
  addMove 1 $ discreteUniformAvoidMH densitySelector 0 1
  addMove 1 $ discreteUniformAvoidMH samplingSelector 0 1
  addMove 2 $ metropolisHastings $ propose geometricValue (\_ -> geometric $ expTo (-1000))
  addMove 2 $ metropolisHastings $
    propose negativeBinomialValue (\_ -> negativeBinomial 2 $ expTo (-1000))
  if samplingSelector == 0
    then return []
    else do
      _ <- sample $ bernoulli (0/0)
      return []

main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 20 context
  putStrLn "completed"
