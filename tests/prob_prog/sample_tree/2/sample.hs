module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)
import           Tree
import           Tree.Newick
import           Control.Monad.Fix

model = do
    tree <- sample $ uniformTopology 5
    let rtree = addRoot 0 tree

    let ps    = map (show . parentNode rtree) [0 .. 5]

    let mu xs Nothing  = 0.0
        mu xs (Just p) = xs !! p
    xs <- mfix (\xs -> sample $ independent [ normal (mu xs p) 1.0 | n <- nodes rtree, let p = parentNode rtree n ])

    return ["tree" %=% writeNewick rtree, "xs" %=% xs, "ps" %=% ps]

main = do
  options <- execParser $
    info (modelRunOptions "sample" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) model

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
