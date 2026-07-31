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
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.OldList ((++), all, filter, length, replicate)
import Data.Ord
import MCMC (runMCMC)
import MCMC.Moves.Context (getAtomicModifiableValueInContext)
import Probability.Distribution.Categorical (categorical)
import Probability.Logger (emptyContextObject)
import Probability.Random (addLogger, makeMCMCModel, prior)
import System.IO (IO)

recordValue ref value = modifyIORef' ref (value :)

-- Read the selector from the logger's execution context before recording the sample.
recordContext ref selector _ context = do
  value <- getAtomicModifiableValueInContext selector context
  recordValue ref value
  return emptyContextObject

ignoreSample _ _ = return ()

sampleLogger ref selector = (recordContext ref selector, ignoreSample)

-- Install a sparse categorical whose only supported categories are far more than seven apart.
model ref = do
  selector <- prior $ categorical ([0.75] ++ replicate 48 0 ++ [0.25])
  addLogger $ sampleLogger ref selector
  return []

-- Check support, movement in both directions, and the long-run probability of category 49.
verifySamples samples =
  if supported && low > 0 && high > 0 && frequency > 0.20 && frequency < 0.30
    then return ()
    else error "bounded categorical Gibbs did not preserve the sparse target distribution"
  where
    supported = all (\value -> value == 0 || value == 49) samples
    low = length (filter (\value -> value == 0) samples)
    high = length (filter (\value -> value == 49) samples)
    frequency = fromIntegral high / fromIntegral (low + high)

-- Run enough updates to include thousands of subsets containing both supported endpoints.
main :: IO ()
main = do
  samples <- newIORef []
  context <- makeMCMCModel (model samples)
  runMCMC 20000 context
  finalSamples <- readIORef samples
  verifySamples finalSamples
