module Model where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)

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
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) $ model 6

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
