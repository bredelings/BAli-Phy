module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability.Random
import           Probability.Distribution.Normal

model = do
  x <- sample $ normal 0 1
  y <- sample $ normal x 1
  return []

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
