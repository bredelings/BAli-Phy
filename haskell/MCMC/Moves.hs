module MCMC.Moves (module MCMC.Moves,
                   module MCMC.Moves.MH,
                   module MCMC.Moves.Integer,
                   module MCMC.Moves.Real,
                   module MCMC.Moves.Tree)
    where

import MCMC.Moves.MH
import MCMC.Moves.Integer
import MCMC.Moves.Real
import MCMC.Moves.Tree

import MCMC.Types -- for registerTransitionKernel
import Effect     -- for registerTransitionKernel

foreign import bpcall "MCMC:" registerTransitionKernelRaw :: Double -> (ContextIndex -> IO ()) -> IO Effect
registerTransitionKernel rate (TransitionKernel kernel) = registerTransitionKernelRaw rate kernel                                                             
