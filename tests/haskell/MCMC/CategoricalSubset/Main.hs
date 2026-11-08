{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Base (seq)
import Compiler.Classes
import Compiler.Error (error)
import Compiler.Floating (log)
import Compiler.Fractional ((/))
import Compiler.Integral (div, fromIntegral)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Function (($))
import Data.OldList ((++), replicate)
import Data.Ord
import MCMC (logPrior, runContextAction, runMCMC)
import Probability.Distribution.Categorical (categorical)
import Probability.Random (makeMCMCState, prior)
import System.IO (IO)

-- Install a sparse categorical whose only supported categories are far more than seven apart.
model = do
  _ <- prior $ categorical ([0.75] ++ replicate 48 0 ++ [0.25])
  return []

-- Check movement in both directions and the long-run probability of category 49.
verifyCounts :: Int -> Int -> IO ()
verifyCounts low high =
  if low > 0 && high > 0 && frequency > 0.20 && frequency < 0.30
    then return ()
    else error "bounded categorical Gibbs did not preserve the sparse target distribution"
  where
    frequency = fromIntegral high / fromIntegral (low + high)

-- The sole prior identifies the two supported states without exporting values from a logger.
-- Force both counters before returning them so observations do not retain pending additions.
countSample context low high = do
  probability <- runContextAction logPrior context
  let low' = if abs (probability - log 0.75) < 1e-10 then low + 1 else low
      high' = if abs (probability - log 0.25) < 1e-10 then high + 1 else high
  if low' == low && high' == high
    then error "bounded categorical Gibbs visited an unsupported state"
    else seq low' $ seq high' $ return (low', high')

-- Work around the evaluator's deep reference chains for a flat recursive IO loop. Splitting the
-- run forces intermediate results while retaining every transition and its subsequent observation.
-- A flat loop can return once the evaluator follows these references without native recursion.
collectCounts 0 _ low high = return (low, high)
collectCounts 1 context low high = do
  runMCMC 1 context
  countSample context low high
collectCounts remaining context low high = do
  let first = remaining `div` 2
  (low', high') <- collectCounts first context low high
  collectCounts (remaining - first) context low' high'

-- Sparse support detects subsets that cannot move between distant categories or bias their weights.
-- Observe through outer IO so this test does not require mutable state to escape temporary contexts.
main :: IO ()
main = do
  mcmcState <- makeMCMCState model
  (low, high) <- countSample mcmcState 0 0
  (low', high') <- collectCounts 20000 mcmcState low high
  verifyCounts low' high'
