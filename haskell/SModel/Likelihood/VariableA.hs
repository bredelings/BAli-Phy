module SModel.Likelihood.VariableA  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Data
import Data.Maybe (maybeToList)
import Foreign.Vector
import Numeric.LogDouble
import Bio.Sequence

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import SModel.Likelihood.CLV

-- peeling for connected-CLVs
foreign import bpcall "Likelihood:" simpleSequenceLikelihoods :: Alphabet -> EVector Int -> Int -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:calcProbAtRoot" calcProbAtRootNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> NativeMatrix Double -> LogDouble
foreign import bpcall "Likelihood:calcProb" calcProbNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> NativeMatrix Double -> LogDouble
foreign import bpcall "Likelihood:peelBranchTowardRoot" peelBranchTowardRootNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (NativeMatrix Double) -> NativeMatrix Double -> CondLikes
foreign import bpcall "Likelihood:peelBranchAwayFromRoot" peelBranchAwayFromRootNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (NativeMatrix Double) -> NativeMatrix Double -> CondLikes

foreign import bpcall "Likelihood:calcProbNonEq" calcProbNonEqNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> NativeMatrix Double -> LogDouble
foreign import bpcall "Likelihood:peelBranchTowardRootNonEq" peelBranchTowardRootNonEq :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (NativeMatrix Double) -> CondLikes
foreign import bpcall "Likelihood:peelBranchAwayFromRootNonEq" peelBranchAwayFromRootNonEqNative :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (NativeMatrix Double) -> NativeMatrix Double -> CondLikes

foreign import bpcall "Likelihood:propagateFrequencies" propagateFrequenciesNative :: NativeMatrix Double -> EVector (NativeMatrix Double) -> NativeMatrix Double

calcProbAtRoot node branch alignments frequencies =
    calcProbAtRootNative node branch alignments (nativeMatrix frequencies)

calcProb node branch alignments frequencies =
    calcProbNative node branch alignments (nativeMatrix frequencies)

peelBranchTowardRoot node branch alignments probabilities frequencies =
    peelBranchTowardRootNative node branch alignments probabilities (nativeMatrix frequencies)

peelBranchAwayFromRoot node branch alignments probabilities frequencies =
    peelBranchAwayFromRootNative node branch alignments probabilities (nativeMatrix frequencies)

calcProbNonEq node branch alignments frequencies =
    calcProbNonEqNative node branch alignments (nativeMatrix frequencies)

peelBranchAwayFromRootNonEq node branch alignments probabilities frequencies =
    peelBranchAwayFromRootNonEqNative node branch alignments probabilities (nativeMatrix frequencies)

propagateFrequencies frequencies probabilities = matrixFromNative (rows frequencies) (cols frequencies)
    (propagateFrequenciesNative (nativeMatrix frequencies) probabilities)

peelBranch toward nodeCLs branchCLs asIn ps eqF | toward    = peelBranchTowardRoot   nodeCLs branchCLs asIn ps eqF
                                                | otherwise = peelBranchAwayFromRoot nodeCLs branchCLs asIn ps eqF

peelBranchNonEq toward nodeCLs branchCLs asIn ps rootF | toward    = peelBranchTowardRootNonEq   nodeCLs branchCLs asIn ps
                                                       | otherwise = peelBranchAwayFromRootNonEq nodeCLs branchCLs asIn ps rootF

-- ancestral sequence sampling for connected-CLVs
foreign import trcall "Likelihood:sampleRootSequence" sampleRootSequence :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> ComponentStateSequence

foreign import trcall "Likelihood:sampleBranchSequence" sampleBranchSequence :: ComponentStateSequence -> PairwiseAlignment -> EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (NativeMatrix Double) -> Matrix Double -> ComponentStateSequence

simpleNodeCLVs :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int)) -> IntMap (Maybe CondLikes)
simpleNodeCLVs alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoods alpha smap nModels

cachedConditionalLikelihoodsWith peelBranch t nodeCLVs as ps f
    = let lc    = getEdgesSet t & IntMap.fromSet lcf
          lcf b = let p = ps IntMap.! b
                      inEdges = edgesBeforeEdgeSet t b
                      nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! (sourceNode t b)
                      branchCLVs = IntMap.restrictKeysToVector lc inEdges
                      asIn  = IntMap.restrictKeysToVector as inEdges
                  in peelBranch b nodeCLV branchCLVs asIn p f
      in lc

peelLikelihoodWith  calcProbAtRoot t nodeCLVs cls as f root
    = let inEdges = edgesTowardNodeSet t root
          nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! root
          clsIn = IntMap.restrictKeysToVector cls inEdges
          asIn  = IntMap.restrictKeysToVector as inEdges
      in calcProbAtRoot nodeCLV clsIn asIn f

--- Eq Rev
cachedConditionalLikelihoodsEqRev = cachedConditionalLikelihoodsWith (\_ -> peelBranchTowardRoot)

peelLikelihoodEqRev = peelLikelihoodWith calcProbAtRoot

--- Eq NonRev
cachedConditionalLikelihoodsEqNonRev t = cachedConditionalLikelihoodsWith (\b -> peelBranch (towardRoot t b)) t

peelLikelihoodEqNonRev = peelLikelihoodWith calcProb

-- NonEq
cachedConditionalLikelihoodsNonEq t = cachedConditionalLikelihoodsWith (\b -> peelBranchNonEq (towardRoot t b)) t

peelLikelihoodNonEq = peelLikelihoodWith calcProbNonEq

-- Generic

frequenciesOnTree t f ps = let fs = getNodesSet t & IntMap.fromSet getF
                               getF node | Just b <- branchFromParent t node = propagateFrequencies (fs IntMap.! sourceNode t b) (ps IntMap.! b)
                                         | otherwise      = f
                           in fs

sampleAncestralSequences t root nodeCLVs as ps f cl =
    let rt = addRoot root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node $ getNodesSet t

        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)

        ancestor_for_branch n Nothing = let inEdges = edgesTowardNodeSet t n
                                            nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! root
                                            clsIn = IntMap.restrictKeysToVector cl inEdges
                                            asIn  = IntMap.restrictKeysToVector as inEdges
                                        in sampleRootSequence nodeCLV clsIn asIn f

        ancestor_for_branch n (Just to_p) = let parent_seq = ancestor_seqs IntMap.! (targetNode t to_p)
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                a0 = as IntMap.! b0
                                                inEdges = edgesBeforeEdgeSet t to_p
                                                clsIn = IntMap.restrictKeysToVector cl inEdges
                                                asIn  = IntMap.restrictKeysToVector as inEdges
                                                nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! (sourceNode t to_p)
                                            in sampleBranchSequence parent_seq a0 nodeCLV clsIn asIn ps_for_b0 f
    in ancestor_seqs
