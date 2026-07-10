{-# LANGUAGE RecursiveDo #-}

module Probability.Distribution.PhyloCTMC.FixedA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment (VectorPairIntInt)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.IntMap as IntMap

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import bpcall "Likelihood:simulateRootSequence" simulateRootSequenceNative :: Int -> NativeMatrix Double -> IO VectorPairIntInt

foreign import bpcall "Likelihood:simulateFixedSequenceFrom" simulateFixedSequenceFromNative :: VectorPairIntInt -> EVector (NativeMatrix Double) -> NativeMatrix Double -> IO VectorPairIntInt

simulateRootSequence count frequencies =
    simulateRootSequenceNative count (nativeMatrix frequencies)

simulateFixedSequenceFrom sequence probabilities frequencies =
    simulateFixedSequenceFromNative sequence probabilities (nativeMatrix frequencies)

sampleComponentStatesFixed rtree rootLength smodel =  do
  let smodelOnTree = SModelOnTree rtree smodel
      ps = transitionPsMap smodelOnTree
      f = weightedFrequencyMatrix smodelOnTree

  rec let simulateSequenceForNode node = case branchToParent rtree node of
                                   Nothing -> simulateRootSequence rootLength f
                                   Just b' -> let b = reverseEdge b'
                                                  parent = sourceNode rtree b
                                             in simulateFixedSequenceFrom (stateSequences IntMap.! parent) (ps IntMap.! b) f
      stateSequences <- lazySequence $ IntMap.fromSet simulateSequenceForNode (getNodesSet rtree)
  return stateSequences
