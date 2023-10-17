module Probability.Distribution.PairwiseAlignment where

import IModel
import Probability
import Bio.Alignment.Pairwise
import Bio.Alignment
import Tree
import qualified Data.IntMap as IM
import Control.Monad.Fix

-- Deletion, insertion ,mean indel length, time starting sequence length
foreign import bpcall "Alignment:" simulateLongIndelsGeometric :: Double -> Double -> Double -> Double -> Int -> IO PairwiseAlignment

data LongIndels = LongIndels { deletionRate, insertionRate, meanLength, time :: Double, startLength :: Int}

instance Dist LongIndels where
    type Result LongIndels = PairwiseAlignment
    dist_name _ = "LongIndels"

instance IOSampleable LongIndels where
    sampleIO (LongIndels mu lambda mean_length t startLength) = simulateLongIndelsGeometric mu lambda mean_length t startLength

---------------------------------------------------------------------------------------------

data IndelsOnTree t d = IndelsOnTree t (Double -> Int -> d) Int

instance Dist d => Dist (IndelsOnTree t d) where
    type Result (IndelsOnTree t d) = AlignmentOnTree t
    dist_name _ = "IndelsOnTree"

{- Note: Indentation handling is not working with this rec statement! -}

instance (IOSampleable d, Result d ~ PairwiseAlignment, HasRoot t, HasBranchLengths t) => IOSampleable (IndelsOnTree t d) where
    sampleIO (IndelsOnTree rtree distFn startLength) =
        do { rec let lengths = getNodesSet rtree & IM.fromSet lengthForNode
                     lengthForNode node  = case branchFromParent rtree node of
                                             Nothing -> startLength
                                             Just b -> pairwise_alignment_length2 (as IM.! b)
                     alignmentForBranch b | toward_root rtree b = return $ flip_alignment $ as IM.! (reverseEdge b)
                                          | otherwise           = sampleIO $ distFn (branch_length rtree b) (lengths IM.! sourceNode rtree b)
                 as <- lazySequence $ (getEdgesSet rtree & IM.fromSet alignmentForBranch)
          ; return $ AlignmentOnTree rtree (numNodes rtree) lengths as}


{- Note: Could we sample from an IntMap dist instead of an IntMap (IO PairwiseAlignment) ?

Ideally we would sample the alignments from an IntMap dist.  Then instead of using lazySequence,
we could do something like:

   as <- sampleIO $ independent $ distsMap

The problem with this is that we can only sample the alignments for branches going away from the root.
-}

instance Dist d => HasAnnotatedPdf (IndelsOnTree t d) where
    annotated_densities dist _ = undefined

instance (IOSampleable d, Result d ~ PairwiseAlignment, HasRoot t, HasBranchLengths t) => Sampleable (IndelsOnTree t d) where
    sample dist = RanDistribution2 dist do_nothing
