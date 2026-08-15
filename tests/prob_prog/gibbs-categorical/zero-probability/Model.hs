module Model where

import BAliPhy.Run
import Probability
import MCMC (gibbsSampleCategorical, runMCMC)
import Options.Applicative

-- Requires the selected candidate to retain the sampled value that made it possible.
model = do
  let i = modifiable 0
  addMove 1 $ gibbsSampleCategorical i 3
  x <- if i == 0 then return 0 else prior $ uniform 0 1
  condition (i /= 1 || x > 0.5)
  return ["i" %=% i, "x" %=% x]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
