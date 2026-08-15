{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import BAliPhy.Run
import Compiler.Enum
import Compiler.Error (error)
import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Function (($))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (sum)
import MCMC (runMCMC)
import Options.Applicative
import Probability
import System.Environment (getEnv)
import Text.Read (read)

changingKeys size choice =
  if choice == 0 then [0 .. size - 1] else [size .. 2 * size - 1]

changingSize size choice =
  if choice == 0 then [0 .. size - 1] else [0 .. size]

forceMapValues values = observe 0 $ normal (sum (IM.elems values)) 1

-- Construct and force one fixed-domain DP map so its one-time assembly cost
-- remains visible separately from changing-domain invalidation.
fixedModel size = do
  values <- dirichletProcessMap (IS.fromList [0 .. size - 1]) 1 (delta 0)
  forceMapValues values
  return []

-- Switch between disjoint, equal-sized domains so each accepted selector
-- change rebuilds the key conversion and map without changing DP dimension.
changingKeysModel size = do
  choice <- prior $ categorical [0.5, 0.5]
  let keys = IS.fromList (changingKeys size choice)
  values <- dirichletProcessMap keys 1 (delta 0)
  forceMapValues values
  return []

-- Add or remove the final numeric key so changing the selector also exercises
-- variable-dimensional DP rebuilding while keeping the workloads comparable.
changingSizeModel size = do
  choice <- prior $ categorical [0.5, 0.5]
  let keys = IS.fromList (changingSize size choice)
  values <- dirichletProcessMap keys 1 (delta 0)
  forceMapValues values
  return []

-- Log a nondegenerate map so fixed-seed hashes detect changed clustering,
-- key/value alignment, or random-number consumption across revisions.
traceModel size = do
  choice <- prior $ categorical [0.5, 0.5]
  let keys = IS.fromList (changingKeys size choice)
  values <- dirichletProcessMap keys 1 (normal 0 1)
  forceMapValues values
  return ["choice" %=% choice, "values" %=% values]

benchmarkModel scenario size =
  case scenario of
    "fixed" -> fixedModel size
    "changing-keys" -> changingKeysModel size
    "changing-size" -> changingSizeModel size
    "trace" -> traceModel size
    _ -> error "unknown dirichlet-process-map benchmark scenario"

-- Select the benchmark outside Random so command-line configuration does not
-- add changing probabilistic dependencies to the measured model.
main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  scenario <- getEnv "BALIPHY_DP_MAP_SCENARIO"
  sizeText <- getEnv "BALIPHY_DP_MAP_SIZE"
  let model = benchmarkModel scenario (read sizeText :: Int)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
