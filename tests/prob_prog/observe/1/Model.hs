module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability.Random
import           Probability.Distribution.Normal

observe_data x = do
    observe x $ normal 0 1
    return []

main = do
  options <- execParser $
    withModelDescription "Run the observation example" $
      modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  let model = observe_data 1

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
