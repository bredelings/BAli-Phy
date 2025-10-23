module Probability.Distribution.PhyloCTMC.FixedA.Sample where

import Probability.Random
import Tree
import SModel
import Bio.Alignment (VectorPairIntInt)
import Data.Matrix
import qualified Data.IntMap as IntMap

import Control.Monad.Fix -- for rec

-- This is imported for both FixedA and VariableA, which is ugly.
foreign import bpcall "Likelihood:" simulateRootSequence :: Int -> Matrix Double -> IO VectorPairIntInt

foreign import bpcall "Likelihood:" simulateFixedSequenceFrom :: VectorPairIntInt -> EVector (Matrix Double) -> Matrix Double -> IO VectorPairIntInt

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

