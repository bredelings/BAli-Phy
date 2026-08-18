{-# LANGUAGE NoImplicitPrelude #-}
module Model where

import BAliPhy.Run
import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.List (replicate, sum)
import qualified Data.Map as Map
import Data.Ord ((>))
import qualified Data.Set as Set
import Data.String (String)
import MCMC (runMCMC)
import Options.Applicative
import Probability

-- Vary a Set domain while likelihood and logging force every keyed DP value;
-- the static unit test does not exercise dynamic domain rebuilding.
model = do
  domainChoice <- prior $ categorical (replicate 5 (1 / 5))
  let keys :: Set.Set String
      keys = case domainChoice of
               0 -> Set.empty
               1 -> Set.fromList ["c", "a"]
               2 -> Set.fromList ["a", "d"]
               3 -> Set.fromList ["a", "b", "c"]
               _ -> Set.fromList ["a", "b", "d"]
  values <- dirichletProcessOn keys 1 (normal 0 1)
  observe 0 $ normal (sum (Map.elems values)) 1
  return ["choice" %=% domainChoice, "keys" %=% Set.toAscList keys, "values" %=% values]

main = do
  options <- execParser $ modelRunParser "Model" 200000

  runInfo <- initializeModelRun (runMode options)

  mcmcState <- makeLoggedMCMCState runInfo (logFormats options) model

  case runInfo of
    TestRun -> printInitialModel (logFormats options) mcmcState
    MCMCRun directory -> do
      reportModelRun (iterations options) (logFormats options) directory
      runMCMC (iterations options) mcmcState
