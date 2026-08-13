module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability

fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

model n = do
  r <- prior $ poisson 4.0
  l <- if 4 < r
       then return 6
       else do tmp <- prior $ poisson 4.0
               return $ fib (2 + r) + tmp
  observe n $ (poisson $ fromIntegral l)
  return ["r" %=% r]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) $ model 6

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context
