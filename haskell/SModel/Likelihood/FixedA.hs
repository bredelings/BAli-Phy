module SModel.Likelihood.FixedA  where 

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

-- peeling for SEV
foreign import bpcall "LikelihoodSEV:" calcProbAtRootSEV :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "LikelihoodSEV:" calcProbSEV :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "LikelihoodSEV:" peelBranchTowardRootSEV :: EVector CondLikes -> EVector CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "LikelihoodSEV:" peelBranchSEV :: EVector CondLikes -> EVector CondLikes -> EVector (Matrix Double) -> Matrix Double -> Bool -> CondLikes

-- ancestral sequence sampling for SEV
foreign import bpcall "LikelihoodSEV:" sampleRootSequenceSEV :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "LikelihoodSEV:" sampleSequenceSEV :: VectorPairIntInt -> EVector CondLikes -> EVector (Matrix Double) -> EVector CondLikes -> EVector Int -> VectorPairIntInt

foreign import bpcall "LikelihoodSEV:" simpleSequenceLikelihoodsSEV :: Alphabet -> EVector Int -> Int -> EPair (EVector Int) CBitVector -> CondLikes

-- Could we move the conversion from sequence-with-gaps to (sequence,bitvector) into here?
simpleNodeCLVsSEV :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int, CBitVector)) -> IntMap (Maybe CondLikes)
simpleNodeCLVsSEV alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoodsSEV alpha smap nModels . c_pair'

{- Note:
   An alternative implementation would be to pass in a frequency matrix full of 1.0s.
   But actually, maybe in that case we want to collect the matrices and multiply them all at once, and there
     isn't really a matrix of 1s in the mix.
 -}

cachedConditionalLikelihoodsSEV2 t nodeCLVs ps f = let clvs = getEdgesSet t & IntMap.fromSet clvForBranch
                                                       clvForBranch b = let p = ps IntMap.! b
                                                                            inEdges = edgesBeforeEdgeSet t b
                                                                            clsIn = IntMap.restrictKeysToVector clvs inEdges
                                                                            node = sourceNode t b
                                                                            nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! node
                                                                        in peelBranchSEV nodeCLs clsIn p f (not $ toward_root t b)
                                                   in clvs

peelLikelihoodSEV2 nodeCLVs t cls f alpha smap root counts = let inEdges = edgesTowardNodeSet t root
                                                                 nModels = nrows f
                                                                 nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
                                                                 clsIn = IntMap.restrictKeysToVector cls inEdges
                                                             in calcProbSEV nodeCLs clsIn f counts

cached_conditional_likelihoods_SEV t nodeCLVs ps =
    let lc    = IntMap.fromSet lcf $ getEdgesSet t
        lcf b = let p = ps IntMap.! b
                    inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector lc inEdges
                    node = sourceNode t b
                    nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! node
                in peelBranchTowardRootSEV nodeCLs clsIn p
    in lc

peel_likelihood_SEV nodeCLVs t cls f alpha smap root counts = let inEdges = edgesTowardNodeSet t root
                                                                  nModels = nrows f
                                                                  nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
                                                                  clsIn = IntMap.restrictKeysToVector cls inEdges
                                                              in calcProbAtRootSEV nodeCLs clsIn f counts

sample_ancestral_sequences_SEV t root nodeCLVs alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node (getNodesSet t)
        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)
        ancestor_for_branch n Nothing = let nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! n
                                            inEdges = edgesTowardNodeSet t n
                                            clsIn = IntMap.restrictKeysToVector cl inEdges
                                        in sampleRootSequenceSEV nodeCLs
                                                                 clsIn
                                                                 f
                                                                 col_to_compressed
        ancestor_for_branch n (Just to_p) = let parent_seq = ancestor_seqs IntMap.! (targetNode t to_p)
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                inEdges = edgesBeforeEdgeSet t to_p
                                                clsIn = IntMap.restrictKeysToVector cl inEdges
                                                nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! n
                                            in sampleSequenceSEV parent_seq
                                                                 nodeCLs
                                                                 ps_for_b0
                                                                 clsIn
                                                                 col_to_compressed
    in ancestor_seqs
                                                                       
