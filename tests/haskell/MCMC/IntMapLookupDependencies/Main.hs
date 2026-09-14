{-# LANGUAGE NoImplicitPrelude #-}
module Main where
import Prelude
import System.Environment (getArgs)
import qualified Data.IntMap as IM
import MCMC (runMCMC)
import MCMC.Loggers (likelihoodRaw)
import MCMC.Moves.Context (setAtomicModifiableValueInContext)
import MCMC.Types (TransitionKernel(..))
import Numeric.Prob (toProb)
import Probability.Distribution.Multinomial (multinomial)
import Probability.Random (addMove, makeMCMCState, modifiable, observe)

-- Static map tests cannot check register dependencies: switch keys and maps, then change
-- selected entries/defaults. Both native return paths must keep the likelihood up to date.
checkDependencies key selector value fallback = TransitionKernel (\context -> do
  setAtomicModifiableValueInContext key 2 context
  first <- likelihoodRaw context
  setAtomicModifiableValueInContext value 0.25 context
  second <- likelihoodRaw context
  setAtomicModifiableValueInContext selector 1 context
  third <- likelihoodRaw context
  setAtomicModifiableValueInContext fallback 0.3 context
  fourth <- likelihoodRaw context
  if and [abs (ln actual - log expected) < 1e-10 |
          (actual, expected) <- zip [first,second,third,fourth] [0.6,0.25,0.4,0.3]]
    then return ()
    else error "IntMap lookup dependencies were not updated")

-- Exercise Maybe construction and direct register forwarding with the same transitions.
model useDefault = do
  let key = modifiable (1 :: Int)
      selector = modifiable (0 :: Int)
      value = modifiable (0.6 :: Double)
      fallback = modifiable (0.4 :: Double)
      m = if selector == 0 then IM.fromList [(1,0.8),(2,value)] else IM.empty
      selected = if useDefault then IM.findWithDefault fallback key m
                 else case IM.lookup key m of Just x -> x; Nothing -> fallback
      p = toProb selected
  observe [1,0] $ multinomial 1 [p, 1-p]
  addMove 1 $ checkDependencies key selector value fallback
  return []

-- Each invocation uses one MCMC context to exercise one builtin's dependencies.
main = do
  args <- getArgs
  state <- makeMCMCState (model (args == ["default"]))
  runMCMC 1 state
  putStrLn "completed"
