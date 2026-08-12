{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Model where

import BAliPhy.Run
import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Eq ((/=))
import Data.Function (($))
import Data.OldList (filter, length)
import Data.String (String)
import MCMC (runMCMC)
import Options.Applicative
import Probability.Distribution.Discrete (delta)
import Probability.Distribution.Laplace (laplace)
import Probability.Distribution.List (iid)
import Probability.Distribution.Mixture
import Probability.Random ((%=%), prior, writeTraceGraph)

-- Sample several spike-and-slab coefficients so categorical moves can change
-- both the active component labels and their total cardinality.
model = do
    coefficients <- prior $ iid 12
        ((1 / 2) .*. delta 0 |+| (1 / 2) .*. laplace 0 1)
    return [("active" :: String) %=% length (filter (/= 0) coefficients)]

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
    if verbosity /= 0 then writeTraceGraph context else return ()
