{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Exception
import MCMC (runMCMC)
import MCMC.Moves.Integer (discreteUniformAvoidMH)
import Probability.Random (addMove, condition, makeMCMCModel, modifiable)
import System.IO (IO, putStrLn)

-- Create a deterministic proposal whose new likelihood throws during ratio calculation.
model = do
  let x = modifiable (0 :: Int)
  addMove 1 (discreteUniformAvoidMH x 0 1)
  condition (if x == 0 then True else throw (IOException "boom"))
  return []

handle :: IOException -> IO ()
handle _ = putStrLn "caught"

-- Repeat the failed transition so stale effect callbacks would run after their captures died.
main = do
  context <- makeMCMCModel model
  catch (runMCMC 1 context) handle
  catch (runMCMC 1 context) handle
