{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import BAliPhy.Run
import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.List (map, replicate, sum)
import Data.Ord ((>))
import Data.Tuple (snd)
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

-- Vary the ordered domain while a likelihood forces every keyed DP value.
model = do
  domainChoice <- prior $ categorical (replicate 5 (1 / 5))
  let keys :: [Int]
      keys = case domainChoice of
               0 -> []
               1 -> [3, 1]
               2 -> [1, 3]
               3 -> [1, 4]
               _ -> [0, 1, 3]
  values <- dirichletProcessOn keys 1 (normal 0 1)
  observe 0 $ normal (sum (map snd values)) 1
  return ["choice" %=% domainChoice, "keys" %=% keys, "values" %=% values]

main = do
  options <- execParser $ modelRunParser "Model" 200000
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
