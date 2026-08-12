module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

my_iid n dist =
    let body = do
            x  <- dist
            xs <- my_iid (n - 1) dist
            return (x : xs)
    in  if n == 0 then return [] else body

model x = do
    n  <- min 100 <$> sample (geometric 0.25)
    xs <- lazy $ my_iid n (sample $ normal 0 1)
    let total = sum xs
    let loggers = ["n" %=% n, "xs" %=% xs]
    observe x $ normal total 1
    return loggers

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  run <- prepareModelRun (testMode options) (outputName options)
  context <- makeModelContext run (logFormats options) $ model 20

  case run of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
