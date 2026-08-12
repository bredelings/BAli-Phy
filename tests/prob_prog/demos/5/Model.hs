module Model where

import           BAliPhy.Run
import           MCMC (runMCMC)
import           Options.Applicative
import           Probability
import           Probability.Random (writeTraceGraph)

-- sequence of @n points [from, next,....,to] where the distribution
-- of the point after x is (f x).

bridge 2 f from to = do
  observe to (f from)
  return [from,to]

bridge n f from to = do
    next <- prior $ f from
    xs <- bridge (n-1) f next to
    return (from:xs)


-- 20 element brownian bridge from 0 to 4
model = do
    xs <- bridge 20 (\x -> normal x 1) 0 4

    return ["xs" %=% xs]

main = do
  options <- execParser $
    info (modelRunOptions "Model" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)
  context <- makeModelContext runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
