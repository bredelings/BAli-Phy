{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import MCMC (runMCMC)
import MCMC.Moves.Context (setAtomicModifiableValueInContext)
import MCMC.Moves.MH (metropolisHastings)
import MCMC.Types (Proposal(..))
import Probability.Random (addMove, condition, makeMCMCModel, modifiable)
import System.IO (IO, putStrLn)

-- Change the proposed context but signal rejection with a zero proposal ratio.
zeroProposal x = Proposal (\context -> do
  setAtomicModifiableValueInContext x 1 context
  return 0)

-- Evaluating the changed context is an error, so completion proves that it was skipped.
model = do
  let x = modifiable (0 :: Int)
      valid = if x == 0 then True else error "zero-ratio proposal state was evaluated"
  addMove 1 (metropolisHastings (zeroProposal x))
  condition valid
  return []

-- Exercise repeated zero-ratio rejection and context reuse.
main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 20 context
  putStrLn "completed"
