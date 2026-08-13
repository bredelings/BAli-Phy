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

nLeaves = 3

allStrings = [ c : s | s <- "" : allStrings, c <- ['a'..'j'] ]

allTexts = fmap Text.pack allStrings

model = do
    tree <- sample $ uniformTimeTree 1 nLeaves
    let labels = take nLeaves $ zip [0..] allTexts
        ltree = addLabels labels tree
    let pr = uniformTimeTreePr 1 nLeaves ltree

    let ps    = map (\n -> show (n, parentNode tree n)) [0 .. numNodes tree - 1]

    rec let mu node = case parentNode tree node of
                Nothing   -> 0
                Just node -> xs !! node
        xs <- sample $ independent [ normal (mu node) 1 | node <- nodes tree ]
  -- can we _observe_ from this? -- why or why not?

    return ["tree" %=% writeNewick ltree] --,"pr" %=% pr, "xs" %=% xs, "ps" %=% ps]

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
