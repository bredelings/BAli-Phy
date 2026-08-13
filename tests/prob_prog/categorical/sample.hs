module Test where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability

model = do
    xs <- prior $ iid 10 (categorical [0.1, 0.2, 0.3, 0.4])
    return ["xs" %=% xs]

main = do
  options <- execParser $ modelRunParser "sample" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
