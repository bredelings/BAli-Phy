module MCMC.Moves.Alignment where

import MCMC.Types
import Data.IntMap (IntMap)
import Bio.Alignment (PairwiseAlignment)

foreign import bpcall "MCMC:" walkTreeSampleAlignmentsRaw :: Modifiable t -> IntMap PairwiseAlignment -> ContextIndex -> IO ()
walkTreeSampleAlignments tree as = TransitionKernel $ walkTreeSampleAlignmentsRaw tree as

foreign import bpcall "MCMC:" walkTreeSampleNNIandARaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNIandA tree = TransitionKernel $ walkTreeSampleNNIandARaw tree

foreign import bpcall "MCMC:" realignFromTipsRaw :: Modifiable t -> IntMap PairwiseAlignment -> ContextIndex -> IO ()
realignFromTips tree as = TransitionKernel $ realignFromTipsRaw tree as


