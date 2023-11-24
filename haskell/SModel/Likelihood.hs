module SModel.Likelihood  where 

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

data CondLikes

-- peeling for connected-CLVs
foreign import bpcall "Likelihood:" simpleSequenceLikelihoods :: Alphabet -> EVector Int -> Int -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:" calcRootProb :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:peelBranch" builtinPeelBranch :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes

peelBranch sequences alphabet smap cls as ps f = builtinPeelBranch (list_to_vector sequences) alphabet smap cls as ps f


-- ancestral sequence sampling for connected-CLVs
foreign import bpcall "Likelihood:" sampleRootSequence :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleBranchSequence :: VectorPairIntInt -> PairwiseAlignment -> EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> VectorPairIntInt

-- peeling for SEV
foreign import bpcall "Likelihood:" calcRootProbSEV :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" peelBranchSEV :: EVector CondLikes -> EVector CondLikes -> EVector (Matrix Double) -> CondLikes

-- ancestral sequence sampling for SEV
foreign import bpcall "Likelihood:" sampleRootSequenceSEV :: EVector CondLikes -> EVector CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleSequenceSEV :: VectorPairIntInt -> EVector CondLikes -> EVector (Matrix Double) -> EVector CondLikes -> EVector Int -> VectorPairIntInt

foreign import bpcall "Likelihood:" simpleSequenceLikelihoodsSEV :: Alphabet -> EVector Int -> Int -> EPair (EVector Int) CBitVector -> CondLikes

cached_conditional_likelihoods t seqs as alpha ps f smap = let lc    = IntMap.fromSet lcf $ getEdgesSet t
                                                               lcf b = let p = ps IntMap.! b
                                                                           inEdges = edgesBeforeEdgeSet t b
                                                                           sequences = maybeToList $ seqs IntMap.! (sourceNode t b)
                                                                           clsIn = IntMap.restrictKeysToVector lc inEdges
                                                                           asIn  = IntMap.restrictKeysToVector as inEdges
                                                                       in peelBranch sequences alpha smap clsIn asIn p f
                                                           in lc

peel_likelihood t seqs cls as alpha f smap root = let inEdges = edgesTowardNodeSet t root
                                                      sequences = maybeToList $ seqs IntMap.! root
                                                      clsIn = IntMap.restrictKeysToVector cls inEdges
                                                      asIn  = IntMap.restrictKeysToVector as inEdges
                                                  in calcRootProb (list_to_vector sequences) alpha smap clsIn asIn f


substitution_likelihood t root seqs as alpha ps f smap = let cls = cached_conditional_likelihoods t seqs as alpha ps f smap
                                                         in peel_likelihood t seqs cls as alpha f smap root

sample_ancestral_sequences :: IsTree t =>
                              t ->
                              Int ->
                              IntMap (Maybe (EVector Int)) ->
                              IntMap PairwiseAlignment ->
                              Alphabet ->
                              IntMap (EVector (Matrix Double)) ->
                              Matrix Double ->
                              IntMap CondLikes ->
                              EVector Int ->
                              IntMap VectorPairIntInt
sample_ancestral_sequences t root seqs as alpha ps f cl smap =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node $ getNodesSet t

        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)

        ancestor_for_branch n Nothing = let inEdges = edgesTowardNodeSet t n
                                            sequences = []
                                            clsIn = IntMap.restrictKeysToVector cl inEdges
                                            asIn  = IntMap.restrictKeysToVector as inEdges
                                        in sampleRootSequence (list_to_vector sequences) alpha smap clsIn asIn f

        ancestor_for_branch n (Just to_p) = let parent_seq = ancestor_seqs IntMap.! (targetNode t to_p)
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                a0 = as IntMap.! b0
                                                inEdges = edgesBeforeEdgeSet t to_p
                                                clsIn = IntMap.restrictKeysToVector cl inEdges
                                                asIn  = IntMap.restrictKeysToVector as inEdges
                                                sequences = maybeToList $ seqs IntMap.! (sourceNode t to_p)
                                            in sampleBranchSequence parent_seq a0 (list_to_vector sequences) alpha smap clsIn asIn ps_for_b0 f
    in ancestor_seqs

-- Could we move the conversion from sequence-with-gaps to (sequence,bitvector) into here?
simpleNodeCLVsSEV :: Alphabet -> EVector Int -> Int -> IntMap (Maybe (EVector Int, CBitVector)) -> IntMap (Maybe CondLikes)
simpleNodeCLVsSEV alpha smap nModels seqs = (sequenceToCL <$>) <$> seqs
    where sequenceToCL = simpleSequenceLikelihoodsSEV alpha smap nModels . c_pair'

cached_conditional_likelihoods_SEV t nodeCLVs ps =
    let lc    = IntMap.fromSet lcf $ getEdgesSet t
        lcf b = let p = ps IntMap.! b
                    inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector lc inEdges
                    node = sourceNode t b
                    nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! node
                in peelBranchSEV nodeCLs clsIn p
    in lc

peel_likelihood_SEV nodeCLVs t cls f alpha smap root counts = let inEdges = edgesTowardNodeSet t root
                                                                  nModels = nrows f
                                                                  nodeCLs = list_to_vector $ maybeToList $ nodeCLVs IntMap.! root
                                                                  clsIn = IntMap.restrictKeysToVector cls inEdges
                                                              in calcRootProbSEV nodeCLs clsIn f counts

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
                                                                       
