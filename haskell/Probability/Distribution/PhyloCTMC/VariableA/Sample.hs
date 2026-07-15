{-# LANGUAGE RecursiveDo #-}

module Probability.Distribution.PhyloCTMC.VariableA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.IntMap as IntMap

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import trcall "Likelihood:simulateRootSequence" simulateRootSequence :: Int -> Matrix Double -> IO ComponentStateSequence

foreign import trcall "Likelihood:simulateSequenceFrom" simulateSequenceFrom :: ComponentStateSequence -> PairwiseAlignment -> EVector (NativeMatrix Double) -> Matrix Double -> IO ComponentStateSequence

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
