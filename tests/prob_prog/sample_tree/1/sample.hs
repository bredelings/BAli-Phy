module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

import           Tree
import           Tree.Newick

model = do
    tree <- sample $ uniformTopology 5
    return ["tree" %=% writeNewick tree]

main = do
  options <- execParser $ modelRunParser "sample" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
