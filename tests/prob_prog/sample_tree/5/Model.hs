{-# LANGUAGE RecursiveDo #-}
module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)
import           Tree
import           Tree.Newick
import qualified Data.Text as Text
import qualified Data.IntMap as IntMap

nLeaves = 5

allTexts = fmap Text.pack allStrings

allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'j'] ]

model taxa = do
    let lambda = 1
    tree <- sample $ yule taxa lambda

    let rootValue = 0

    rec let meanFor node = case parentNode tree node of
                             Nothing   -> rootValue
                             Just node -> xs IntMap.! node
        xs <- sample $ independentMap (getNodesSet tree) (\node -> sample $ normal (meanFor node) 1)

    return ["tree" %=% writeNewick tree,
            "xs" %=% xs]

main = do
  options <- execParser $ modelRunParser "Model" 200000
  runInfo <- initializeModelRun (testMode options) (outputName options)

  let taxa = take nLeaves allTexts
  context <- makeModelContext runInfo (logFormats options) $ model taxa

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
