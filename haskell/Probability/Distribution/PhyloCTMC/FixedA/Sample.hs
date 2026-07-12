{-# LANGUAGE RecursiveDo #-}

module Probability.Distribution.PhyloCTMC.FixedA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment (NativeComponentStateSequence,
                      componentStateSequenceFromNative,
                      componentStateSequenceNativeView)
import Foreign.NativeVector (NativeVector)
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import qualified Data.IntMap as IntMap

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import bpcall "Likelihood:simulateRootSequence" simulateRootSequenceNative :: Int -> NativeMatrix Double -> IO NativeComponentStateSequence

foreign import bpcall "Likelihood:simulateFixedSequenceFrom" simulateFixedSequenceFromNative :: Int -> Int -> NativeVector Int -> Int -> NativeVector Int -> EVector (NativeMatrix Double) -> NativeMatrix Double -> IO NativeComponentStateSequence

simulateRootSequence count frequencies =
    fmap (componentStateSequenceFromNative count) $
        simulateRootSequenceNative count (nativeMatrix frequencies)

-- Simulate directly from both native parent views and retain the unchanged
-- parent length as the Haskell-owned result length.
simulateFixedSequenceFrom sequence probabilities frequencies =
    fmap (componentStateSequenceFromNative count) $
        simulateFixedSequenceFromNative count componentOffset componentNative
                                              stateOffset stateNative
                                              probabilities (nativeMatrix frequencies)
  where
    (count, componentOffset, componentNative, stateOffset, stateNative) =
        componentStateSequenceNativeView sequence

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
