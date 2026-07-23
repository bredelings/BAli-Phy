{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.OldList (replicate)
import Data.Ord ((<))
import qualified Data.Vector.Unboxed as U
import MCMC (condLogOdds, condLogOddsValues, condPr, runMCMC)
import MCMC.Types (TransitionKernel(..), runContextAction)
import Probability.Distribution.List (iid)
import Probability.Distribution.Uniform (uniform)
import Probability.Random (addMove, condition, makeMCMCModel, modifiable, prior)
import System.IO (IO, putStrLn)
import Text.Show (show)

-- Report conditional probabilities for dimension changes, an impossible state, and a constant selector.
report dimensionSelector restrictedSelector = TransitionKernel (\context -> do
  dimensionLogOdds <- runContextAction (condLogOddsValues dimensionSelector 3) context
  restrictedLogOdds <- runContextAction (condLogOddsValues restrictedSelector 3) context
  fixedLogOdds <- runContextAction (condLogOdds (1 :: Int) 1 3) context
  dimensionProb <- runContextAction (condPr dimensionSelector 1 3) context
  restrictedProb <- runContextAction (condPr restrictedSelector 2 3) context
  fixedProb <- runContextAction (condPr (1 :: Int) 1 3) context
  putStrLn (show (U.toList dimensionLogOdds, U.toList restrictedLogOdds, fixedLogOdds))
  putStrLn (show (dimensionProb, restrictedProb, fixedProb)))

-- Give all dimension-changing states equal mass and make the third restricted state impossible.
model = do
  let dimensionSelector = modifiable (0 :: Int)
      restrictedSelector = modifiable (0 :: Int)
  _ <- prior $ iid dimensionSelector (uniform 0 1)
  condition (restrictedSelector < 2)
  addMove 1 (report dimensionSelector restrictedSelector)
  return []

main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 1 context
