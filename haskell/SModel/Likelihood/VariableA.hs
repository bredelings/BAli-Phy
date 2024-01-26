module SModel.Likelihood.VariableA  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
import Data.Maybe (maybeToList)
import Data.Array
import Foreign.Vector
import Numeric.LogDouble
import Bio.Sequence (bitmask_from_sequence, strip_gaps)

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import SModel.Likelihood.CLV

-- peeling for connected-CLVs
foreign import bpcall "Likelihood:" simpleSequenceLikelihoods :: Alphabet -> EVector Int -> Int -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:" calcProbAtRoot :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" calcProb :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peelBranchTowardRoot :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "Likelihood:" peelBranchAwayFromRoot :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "Likelihood:" propagateFrequencies :: Matrix Double -> EVector (Matrix Double) -> Matrix Double

peelBranch nodeCLs branchCLs asIn ps f toward | toward    = peelBranchTowardRoot   nodeCLs branchCLs asIn ps f
                                              | otherwise = peelBranchAwayFromRoot nodeCLs branchCLs asIn ps f

-- ancestral sequence sampling for connected-CLVs
foreign import bpcall "Likelihood:" sampleRootSequence :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleBranchSequence :: VectorPairIntInt -> PairwiseAlignment -> EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> VectorPairIntInt

simpleNodeCLVs :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int)) -> IntMap (Maybe CondLikes)
simpleNodeCLVs alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoods alpha smap nModels

cached_conditional_likelihoods t nodeCLVs as ps f = let lc    = getEdgesSet t & IntMap.fromSet lcf
                                                        lcf b = let p = ps IntMap.! b
                                                                    inEdges = edgesBeforeEdgeSet t b
                                                                    nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! (sourceNode t b)
                                                                    branchCLVs = IntMap.restrictKeysToVector lc inEdges
                                                                    asIn  = IntMap.restrictKeysToVector as inEdges
                                                                in peelBranchTowardRoot nodeCLV branchCLVs asIn p f
                                                    in lc

peel_likelihood t nodeCLVs cls as f root = let inEdges = edgesTowardNodeSet t root
                                               nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
                                               clsIn = IntMap.restrictKeysToVector cls inEdges
                                               asIn  = IntMap.restrictKeysToVector as inEdges
                                           in calcProbAtRoot nodeCLV clsIn asIn f

cachedConditionalLikelihoodsNonRev t nodeCLVs as ps fs = let lc    = getEdgesSet t & IntMap.fromSet lcf
                                                             lcf b = let p = ps IntMap.! b
                                                                         inEdges = edgesBeforeEdgeSet t b
                                                                         node = sourceNode t b
                                                                         nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! node
                                                                         branchCLVs = IntMap.restrictKeysToVector lc inEdges
                                                                         asIn  = IntMap.restrictKeysToVector as inEdges
                                                                     in peelBranch nodeCLV branchCLVs asIn p (fs IntMap.! node) (toward_root t b)
                                                         in lc

peelLikelihoodNonRev t nodeCLVs cls as fs root = let inEdges = edgesTowardNodeSet t root
                                                     nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
                                                     clsIn = IntMap.restrictKeysToVector cls inEdges
                                                     asIn  = IntMap.restrictKeysToVector as inEdges
                                                 in calcProb nodeCLV clsIn asIn (fs IntMap.! root)

frequenciesOnTree t f ps = let fs = getNodesSet t & IntMap.fromSet getF
                               getF node | Just b <- branchFromParent t node = propagateFrequencies (fs IntMap.! sourceNode t b) (ps IntMap.! b)
                                         | otherwise      = f
                           in fs

sample_ancestral_sequences t root nodeCLVs as ps f cl =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node $ getNodesSet t

        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)

        ancestor_for_branch n Nothing = let inEdges = edgesTowardNodeSet t n
                                            nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
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
                                                nodeCLV = list_to_vector $ maybeToList $ nodeCLVs IntMap.! (sourceNode t to_p)
                                            in sampleBranchSequence parent_seq a0 nodeCLV clsIn asIn ps_for_b0 f
    in ancestor_seqs

