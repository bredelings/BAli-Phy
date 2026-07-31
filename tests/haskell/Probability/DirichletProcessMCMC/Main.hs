{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Classes
import Compiler.Error (error)
import Compiler.Fractional ((/))
import Compiler.Integral (fromIntegral)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Function (($))
import Data.OldList ((!!), filter, length, nub)
import Data.Ord
import Data.Tuple (fst, snd)
import MCMC (logLikelihood, runContextAction, runMCMC)
import Probability.Distribution.Bernoulli (bernoulli)
import Probability.Distribution.Categorical (categorical)
import Probability.Distribution.DirichletProcess (dirichletProcess)
import Probability.Distribution.Normal (normal)
import Probability.Random (makeMCMCModel, observe, prior)
import System.IO (IO)

assert condition message = if condition then return () else error message

-- Encode alpha and partition state in two likelihoods so the driver can observe both externally.
mcmcPosteriorModel = do
  alphaChoice <- prior $ categorical [0.5, 0.5]
  let alpha = if alphaChoice == 0 then 1 else 4
  values <- dirichletProcess 2 alpha (normal 0 1)
  let reused = (values !! 0) == (values !! 1)
  observe (1 :: Int) $ bernoulli (if reused then 0.9 else 0.1)
  observe (1 :: Int) $ bernoulli (if alphaChoice == 0 then 0.8 else 0.2)
  return []

-- The likelihood products 0.72, 0.18, 0.08, and 0.02 uniquely identify the four states.
classifyPosteriorState logDataProbability
  | logDataProbability > -1 = (0, True)
  | logDataProbability > -2 = (1, True)
  | logDataProbability > -3 = (0, False)
  | otherwise = (1, False)

-- Run short MCMC blocks and inspect their live contexts without putting mutable state in Random.
collectPosteriorSamples 0 _ samples = return samples
collectPosteriorSamples remaining context samples = do
  runMCMC 2 context
  logDataProbability <- runContextAction logLikelihood context
  let state = classifyPosteriorState logDataProbability
  collectPosteriorSamples (remaining - 1) context (state : samples)

-- The four unnormalized joint weights are 0.18, 0.02, 0.018, and 0.008.
-- Thus P(alpha=1|y)=100/113 and P(reused|y)=99/113 under the MCMC target.
verifyMCMCPosterior samples = do
  assert (length samples == 5000) "the MCMC posterior test retained the wrong number of samples"
  assert (length (nub samples) == 4) "the MCMC posterior test did not visit every joint state"
  assert (alphaOneFrequency > 0.85 && alphaOneFrequency < 0.92)
         "the MCMC alpha posterior did not match 100/113"
  assert (reuseFrequency > 0.84 && reuseFrequency < 0.92)
         "the MCMC partition posterior did not match 99/113"
  where
    total = fromIntegral (length samples)
    alphaOneFrequency = fromIntegral (length (filter (\state -> fst state == 0) samples)) / total
    reuseFrequency = fromIntegral (length (filter snd samples)) / total

-- Burn in the joint chain, then retain thinned samples for both analytic marginal checks.
main :: IO ()
main = do
  context <- makeMCMCModel mcmcPosteriorModel
  runMCMC 2000 context
  samples <- collectPosteriorSamples 5000 context []
  verifyMCMCPosterior samples
