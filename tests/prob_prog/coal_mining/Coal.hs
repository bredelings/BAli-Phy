module Coal where

import BAliPhy.Run
import MCMC (runMCMC)
import Options.Applicative
import Probability
import Probability.Random (writeTraceGraph)
import Data.Frame
import qualified Data.Vector as V

-- When we get polymorphism, maybe we can create a temporary vector as a view on xs'
-- I presume that using n directly is more efficient that using [length xs']
orderedSample n dist = do
  xs' <- sample $ iid n dist
  let xs = sort xs'
  return $ V.fromList xs

get_intervals (r:[]) (t:[]) tend = [(r,t,tend)]
get_intervals (r:rs) (t1:ts@(t2:_)) tend = (r,t1,t2):get_intervals rs ts tend

model (t1,t2) times = do
  let lam = 3; nMax = 30; a = 1; b = 365/200

  -- n change points, n+1 intervals
  n <- min nMax <$> prior (poisson lam)

  -- even numbered order statistics over the interval [t1,t2]
  s' <- orderedSample (2*n+1) (uniform t1 t2)
  let s = t1:[s' V.! (2*i-1) | i <- [1..n]]

  -- n+1 rates
  g <- prior $ iid (n+1) (gamma a b)

  let intervals = get_intervals g s t2

  observe times $ poisson_processes intervals

  return [ "n" %=% n, "s" %=% s, "g" %=% g, "intervals" %=% intervals]


main = do
  options <- execParser $
    info (modelRunOptions "Coal" 200000 (pure ()) <**> helper) fullDesc
  runInfo <- initializeModelRun (testMode options) (outputName options)

  frame <- readTable "coal-times.csv"

  let times = frame $$ "time" :: [Double]

  context <- makeModelContext runInfo (logFormats options) $ model (1851, 1963) times

  case runInfo of
    TestRun -> printInitialModel (logFormats options) context
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) context

  verbosity <- getVerbosity
  if verbosity > 0 then writeTraceGraph context else return ()
