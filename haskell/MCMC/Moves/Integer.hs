module MCMC.Moves.Integer where

import Range
import MCMC.Types

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
foreign import bpcall "MCMC:" gibbsSampleCategoricalRaw :: Modifiable Int -> Int -> ContextIndex -> IO ()
gibbsSampleCategorical i n = TransitionKernel (gibbsSampleCategoricalRaw i n)

foreign import bpcall "MCMC:" discreteUniformAvoidMHRaw :: Modifiable Int -> Int -> Int -> ContextIndex -> IO ()
discreteUniformAvoidMH i l u = TransitionKernel (discreteUniformAvoidMHRaw i l u)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
foreign import bpcall "MCMC:" incDecMHRaw :: Modifiable Int -> BuiltinBounds -> ContextIndex -> IO ()
incDecMH x bnds = TransitionKernel $ incDecMHRaw x (c_range bnds)

foreign import bpcall "MCMC:" sliceSampleIntegerRaw :: Modifiable Int -> BuiltinBounds -> ContextIndex -> IO ()
sliceSampleInteger x bnds = TransitionKernel $ sliceSampleIntegerRaw x (c_range bnds)

