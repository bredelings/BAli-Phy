module MCMC.Moves.Tree where

import MCMC.Types
import Foreign.Vector
import Tree

foreign import bpcall "MCMC:" walkTreePathRaw :: Modifiable t -> ContextIndex -> EVector Int
walk_tree_path tree c = vectorToList $ walkTreePathRaw tree c

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" fnprUnsafeProposalRaw :: Modifiable t -> Int -> ContextIndex -> IO LogDouble
fnprUnsafeProposal tree branch = Proposal $ fnprUnsafeProposalRaw tree branch

foreign import bpcall "MCMC:" walkTreeSampleNNIRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNI tree = TransitionKernel $ walkTreeSampleNNIRaw tree

foreign import bpcall "MCMC:" walkTreeSampleNNIandBranchLengthsRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNIandBranchLengths tree = TransitionKernel $ walkTreeSampleNNIandBranchLengthsRaw tree

foreign import bpcall "MCMC:" walkTimeTreeSampleNNIandNodeTimesRaw :: Modifiable t -> ContextIndex -> IO ()
walkTimeTreeSampleNNIandNodeTimes tree = TransitionKernel $ walkTimeTreeSampleNNIandNodeTimesRaw tree

foreign import bpcall "MCMC:" walkTreeSampleBranchLengthsRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleBranchLengths tree = TransitionKernel $ walkTreeSampleBranchLengthsRaw tree                                                             
foreign import bpcall "MCMC:" sampleSPRAllRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRAll tree = TransitionKernel $ sampleSPRAllRaw tree

foreign import bpcall "MCMC:" sampleSPRNodesRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRNodes tree = TransitionKernel $ sampleSPRNodesRaw tree

foreign import bpcall "MCMC:" sampleSPRFlatRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRFlat tree = TransitionKernel $ sampleSPRFlatRaw tree


