module Model where

-- See https://github.com/probmods/webppl/blob/dev/examples/gaussianRandomWalk.wppl
import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

markov next state0 = lazy $ do state1 <- next state0
                               states <- markov next state1
                               return (state0:states)

transition xs = sample $ independent [normal x 10 | x <- xs]

gaussianRandomWalk n dim = do
  state0 <- sample $ iid dim (normal 200 1)
  states <- markov transition state0
  return $ take n states

model = do
  states <- gaussianRandomWalk 100 2
  return ["states" %=% states]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
