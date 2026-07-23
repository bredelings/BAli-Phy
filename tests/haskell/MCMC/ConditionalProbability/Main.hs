{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Fractional
import Compiler.Num
import Control.Monad (return)
import Data.Function (($))
import Data.OldList (replicate)
import Data.Ord ((<))
import qualified Data.Vector.Unboxed as U
import MCMC (condPr, condPrs, runMCMC)
import MCMC.Types (TransitionKernel(..), runContextAction)
import Probability.Distribution.List (iid)
import Probability.Distribution.Uniform (uniform)
import Probability.Random (addMove, condition, makeMCMCModel, modifiable, prior)
import System.IO (IO, putStrLn)
import Text.Show (show)

-- Report conditional probabilities for dimension changes, an impossible state, and a constant selector.
report dimensionSelector restrictedSelector = TransitionKernel (\context -> do
  dimensionVector <- runContextAction (condPrs dimensionSelector 3) context
  restrictedVector <- runContextAction (condPrs restrictedSelector 3) context
  fixedProb <- runContextAction (condPr (1 :: Int) 1 3) context
  putStrLn (show (U.toList dimensionVector, U.toList restrictedVector, fixedProb)))

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
