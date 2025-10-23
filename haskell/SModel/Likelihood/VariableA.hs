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
import Bio.Sequence

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

foreign import bpcall "Likelihood:" calcProbNonEq :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peelBranchTowardRootNonEq :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "Likelihood:" peelBranchAwayFromRootNonEq :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes

foreign import bpcall "Likelihood:" propagateFrequencies :: Matrix Double -> EVector (Matrix Double) -> Matrix Double

peelBranch nodeCLs branchCLs asIn ps f toward | toward    = peelBranchTowardRoot   nodeCLs branchCLs asIn ps f
                                              | otherwise = peelBranchAwayFromRoot nodeCLs branchCLs asIn ps f

peelBranchNonEq nodeCLs branchCLs asIn ps rootF toward | toward    = peelBranchTowardRootNonEq   nodeCLs branchCLs asIn ps
                                                       | otherwise = peelBranchAwayFromRootNonEq nodeCLs branchCLs asIn ps rootF

-- ancestral sequence sampling for connected-CLVs
foreign import bpcall "Likelihood:" sampleRootSequence :: EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleBranchSequence :: VectorPairIntInt -> PairwiseAlignment -> EVector CondLikes -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> VectorPairIntInt

simpleNodeCLVs :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int)) -> IntMap (Maybe CondLikes)
simpleNodeCLVs alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoods alpha smap nModels

--- Eq Rev
cachedConditionalLikelihoods t nodeCLVs as ps f = let lc    = getEdgesSet t & IntMap.fromSet lcf
                                                      lcf b = let p = ps IntMap.! b
                                                                  inEdges = edgesBeforeEdgeSet t b
                                                                  nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! (sourceNode t b)
                                                                  branchCLVs = IntMap.restrictKeysToVector lc inEdges
                                                                  asIn  = IntMap.restrictKeysToVector as inEdges
                                                              in peelBranchTowardRoot nodeCLV branchCLVs asIn p f
                                                  in lc

peelLikelihood t nodeCLVs cls as f root = let inEdges = edgesTowardNodeSet t root
                                              nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! root
                                              clsIn = IntMap.restrictKeysToVector cls inEdges
                                              asIn  = IntMap.restrictKeysToVector as inEdges
                                          in calcProbAtRoot nodeCLV clsIn asIn f
--- Eq NonRev
cachedConditionalLikelihoodsEqNonRev t nodeCLVs as ps f = let lc    = getEdgesSet t & IntMap.fromSet lcf
                                                              lcf b = let p = ps IntMap.! b
                                                                          inEdges = edgesBeforeEdgeSet t b
                                                                          node = sourceNode t b
                                                                          nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! node
                                                                          branchCLVs = IntMap.restrictKeysToVector lc inEdges
                                                                          asIn  = IntMap.restrictKeysToVector as inEdges
                                                                      in peelBranch nodeCLV branchCLVs asIn p f (towardRoot t b)
                                                          in lc

peelLikelihoodEqNonRev t nodeCLVs cls as f root = let inEdges = edgesTowardNodeSet t root
                                                      nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! root
                                                      clsIn = IntMap.restrictKeysToVector cls inEdges
                                                      asIn  = IntMap.restrictKeysToVector as inEdges
                                                  in calcProb nodeCLV clsIn asIn f

-- NonEq
cachedConditionalLikelihoodsNonEq t nodeCLVs as ps rootF = let lc    = getEdgesSet t & IntMap.fromSet lcf
                                                               lcf b = let p = ps IntMap.! b
                                                                           inEdges = edgesBeforeEdgeSet t b
                                                                           node = sourceNode t b
                                                                           nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! node
                                                                           branchCLVs = IntMap.restrictKeysToVector lc inEdges
                                                                           asIn  = IntMap.restrictKeysToVector as inEdges
                                                                       in peelBranchNonEq nodeCLV branchCLVs asIn p rootF (towardRoot t b)
                                                           in lc

peelLikelihoodNonEq t nodeCLVs cls as rootF root = let inEdges = edgesTowardNodeSet t root
                                                       nodeCLV = toVector $ maybeToList $ nodeCLVs IntMap.! root
                                                       clsIn = IntMap.restrictKeysToVector cls inEdges
                                                       asIn  = IntMap.restrictKeysToVector as inEdges
                                                   in calcProbNonEq nodeCLV clsIn asIn rootF
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

