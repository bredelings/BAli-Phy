{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Compiler.Error (error)
import Compiler.Num
import Control.Monad (return)
import Data.Bool
import Data.Eq
import Data.Function (($))
import MCMC (runMCMC)
import MCMC.Moves.Context (getAtomicModifiableValueInContext)
import MCMC.Types (ContextIndex, TransitionKernel(..))
import Probability.Random (addMove, condition, makeMCMCModel, modifiable)
import System.IO (IO, putStrLn)

foreign import bpcall "MCMC:sum_out_coals"
  sumOutCoalsNative :: Int -> [Int] -> ContextIndex -> IO ()

-- Check the indicators immediately after every joint update.
checkingSumOutCoals t i1 i2 = TransitionKernel (\context -> do
  sumOutCoalsNative t [i1, i2] context
  value1 <- getAtomicModifiableValueInContext i1 context
  value2 <- getAtomicModifiableValueInContext i2 context
  if value1 == 0 && value2 == 0
    then return ()
    else error "sum_out_coals did not recover all zero-density indicators")

-- Protect joint recovery from multiple zero factors, which the algebra unit test cannot exercise
-- through MCMC contexts.  This becomes redundant if sum_out_coals is replaced by a generic kernel.
model = do
  let t = modifiable (0 :: Int)
      i1 = modifiable (1 :: Int)
      i2 = modifiable (1 :: Int)
  condition (i1 == 0)
  condition (i2 == 0)
  addMove 1 $ checkingSumOutCoals t i1 i2
  return []

-- Completion proves that the model's only transition kernel ran its checks repeatedly.
main :: IO ()
main = do
  context <- makeMCMCModel model
  runMCMC 20 context
  putStrLn "completed"
