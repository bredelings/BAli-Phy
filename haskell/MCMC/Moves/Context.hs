module MCMC.Moves.Context where

import MCMC.Types

foreign import bpcall "MCMC:" copyContext :: ContextIndex -> IO ContextIndex

foreign import bpcall "MCMC:" releaseContext :: ContextIndex -> IO ()

foreign import bpcall "MCMC:" switchToContext :: ContextIndex -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" getAtomicModifiableValueInContext :: Modifiable a -> ContextIndex -> IO a

foreign import bpcall "MCMC:" setAtomicModifiableValueInContext :: Modifiable a -> a -> ContextIndex -> IO ()

