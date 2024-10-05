module MCMC.Types (module MCMC.Types,
                   module Numeric.LogDouble,
                   module Range)
    where

import Numeric.LogDouble
import Range

-- data ContextIndex = ContextIndex Int
type ContextIndex = Int

data TransitionKernel = TransitionKernel (ContextIndex -> IO ())

runTK c (TransitionKernel kernel) = kernel c

data Proposal = Proposal (ContextIndex -> IO LogDouble)

-- It is unfortunate that modifiable-ness is not visible at the type level.
type Modifiable a = a

