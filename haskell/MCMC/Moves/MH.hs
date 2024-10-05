module MCMC.Moves.MH where

import Probability.Dist -- for several things in propose

import MCMC.Types
import MCMC.Moves.Context

foreign import bpcall "MCMC:" acceptMH :: ContextIndex -> ContextIndex -> LogDouble -> IO Bool

metropolisHastings :: Proposal -> TransitionKernel
metropolisHastings (Proposal proposal) = TransitionKernel $ \c1 -> do
  c2 <- copyContext c1
  ratio <- proposal c2
  accept <- acceptMH c1 c2 ratio
  if accept then switchToContext c1 c2 else return ()
  releaseContext c2
  return ()              -- Should we allow `return accept`?

propose :: (Dist d, IOSampleable d, HasPdf d, Result d ~ a) => Modifiable a -> (a -> d) -> Proposal
propose x dist = Proposal $ \c -> do
  x1 <- getAtomicModifiableValueInContext x c -- This is guaranteed to be an Int, Double, LogDouble, Char, or Object.
                                              -- x has to be an atomic modifiable.  We can't use it to propose trees...
  x2 <- sampleIO (dist x1)
  setAtomicModifiableValueInContext x x2 c
  let rho12 = pdf (dist x1) x2
      rho21 = pdf (dist x2) x1
      hastingsRatio = rho21/rho12
  return hastingsRatio

