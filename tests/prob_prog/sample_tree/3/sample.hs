{-# LANGUAGE RecursiveDo #-}
module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)
import           Tree
import           Tree.Newick

model = do
    tree <- prior $ uniformTopology 5
    let rtree = addRoot 0 tree

    let ps    = map (show . parentNode rtree) [0 .. 5]

    rec let mu node = case parentNode rtree node of
                Nothing   -> 0.0
                Just node -> xs !! node
        xs <- prior $ independent [ normal (mu node) 1.0 | node <- nodes rtree ]
    -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% writeNewick rtree, "xs" %=% xs, "ps" %=% ps]

main = do
  options <- execParser $ modelRunParser "sample" 200000
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
