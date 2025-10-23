module Probability.Distribution.PhyloCTMC.VariableA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment
import Data.Matrix
import qualified Data.IntMap as IntMap

import Control.Monad.Fix -- for rec

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt

foreign import bpcall "Likelihood:" simulateSequenceFrom :: VectorPairIntInt -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

sampleComponentStates rtree alignment smodel =  do
  let smodelOnTree = SModelOnTree rtree smodel
      as = pairwiseAlignments alignment
      ps = transitionPsMap smodelOnTree
      f = (weightedFrequencyMatrix smodelOnTree)

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence (sequenceLength alignment node) f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateSequenceFrom (stateSequences IntMap.! parent) (as IntMap.! b) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences
