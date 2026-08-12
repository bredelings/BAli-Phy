{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import BAliPhy.Run
import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (replicate, sum)
import Data.Ord ((>))
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

-- Vary the integer-key domain while a likelihood forces every map-indexed DP value.
model = do
  domainChoice <- prior $ categorical (replicate 4 (1 / 4))
  let keyList :: [Int]
      keyList = case domainChoice of
                  0 -> []
                  1 -> [3, 1]
                  2 -> [1, 4]
                  _ -> [0, 1, 3]
      keys = IS.fromList keyList
  values <- dirichletProcessMap keys 1 (normal 0 1)
  observe 0 $ normal (sum (IM.elems values)) 1
  return ["choice" %=% domainChoice, "keys" %=% keyList, "values" %=% values]

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) model

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
