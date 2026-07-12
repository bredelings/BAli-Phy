{-# LANGUAGE RecursiveDo #-}

module Probability.Distribution.PhyloCTMC.VariableA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Foreign.NativeVector (NativeVector)
import qualified Data.IntMap as IntMap

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import bpcall "Likelihood:simulateRootSequence" simulateRootSequenceNative :: Int -> NativeMatrix Double -> IO NativeComponentStateSequence

foreign import bpcall "Likelihood:simulateSequenceFrom" simulateSequenceFromNative :: Int -> Int -> NativeVector Int -> Int -> NativeVector Int -> PairwiseAlignment -> EVector (NativeMatrix Double) -> NativeMatrix Double -> IO NativeComponentStateSequence

simulateRootSequence count frequencies =
    fmap (componentStateSequenceFromNative count) $
        simulateRootSequenceNative count (nativeMatrix frequencies)

-- Pass cached native transition payloads and unwrap the frequency matrix at
-- the builtin boundary.
simulateSequenceFrom sequence alignment probabilities frequencies =
    fmap (componentStateSequenceFromNative outputCount) $
        simulateSequenceFromNative parentCount componentOffset componentNative
            stateOffset stateNative
            alignment probabilities (nativeMatrix frequencies)
  where
    outputCount = pairwise_alignment_length2 alignment
    (parentCount, componentOffset, componentNative, stateOffset, stateNative) =
        componentStateSequenceNativeView sequence

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
