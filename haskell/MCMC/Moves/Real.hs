module MCMC.Moves.Real where

import MCMC.Types
import MCMC.Moves.MH
import Data.Foldable (toList)

foreign import bpcall "MCMC:" sliceSampleRaw :: Modifiable Double -> BuiltinBounds -> ContextIndex -> IO ()
sliceSample x bnds = TransitionKernel $ sliceSampleRaw x (c_range bnds)

foreign import bpcall "MCMC:" scaleGroupsSliceRaw :: [Double] -> [Double] -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" scaleGroupsProposalRaw :: [Double] -> [Double] -> ContextIndex -> IO LogDouble

scaleGroupsSlice xs ys = TransitionKernel $ scaleGroupsSliceRaw (toList xs) (toList ys)

scaleGroupSlice xs = scaleGroupsSlice xs []

scaleGroupsProposal xs ys = Proposal $ scaleGroupsProposalRaw (toList xs) (toList ys)

scaleGroupProposal xs = scaleGroupsProposal xs []

scaleGroupsMH xs ys = metropolisHastings $ scaleGroupsProposal xs ys

