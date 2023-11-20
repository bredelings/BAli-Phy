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
foreign import bpcall "Likelihood:" peel_leaf_branch :: EVector Int -> Alphabet -> EVector (Matrix Double) -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:" alignment_index2 :: PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "Likelihood:" alignment_index3 :: PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "Likelihood:" peel_internal_branch :: CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "Likelihood:" calc_root_probability :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_2 :: EVector Int -> EVector Int -> Alphabet -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_1 :: EVector Int -> Alphabet -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" calcRootProb :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:peelBranch" builtinPeelBranch :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes

peelBranch sequences alphabet smap cls as ps f = builtinPeelBranch (list_to_vector sequences) alphabet smap cls as ps f


-- ancestral sequence sampling for connected-CLVs
foreign import bpcall "Likelihood:" sampleRootSequence :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleBranchSequence :: VectorPairIntInt -> PairwiseAlignment -> EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> VectorPairIntInt

foreign import bpcall "Likelihood:" sample_root_sequence :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_internal_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_leaf_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> Alphabet -> EVector Int -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt

-- peeling for SEV
foreign import bpcall "Likelihood:" peel_leaf_branch_SEV :: EVector Int -> Alphabet -> EVector (Matrix Double) -> CBitVector -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:" peel_internal_branch_SEV :: CondLikes -> CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "Likelihood:" peel_deg2_branch_SEV :: CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "Likelihood:" calc_root_probability_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" calc_root_deg2_probability_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_2_SEV :: (EVector Int) -> (EVector Int) -> Alphabet -> EVector (Matrix Double) -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_1_SEV :: (EVector Int) -> Alphabet -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" calcRootProbSEV :: EVector (EPair (EVector Int) CBitVector)-> Alphabet -> EVector Int -> EVector CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:peelBranchSEV" builtinPeelBranchSEV :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector (Matrix Double) -> CondLikes

peelBranchSEV sequences alphabet smap cls ps = builtinPeelBranchSEV (list_to_vector sequences) alphabet smap cls ps

-- ancestral sequence sampling for SEV
foreign import bpcall "Likelihood:" sampleRootSequenceSEV :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector Int -> EVector CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sampleSequenceSEV :: VectorPairIntInt -> EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector Int -> EVector (Matrix Double) -> EVector CondLikes -> EVector Int -> VectorPairIntInt

foreign import bpcall "Likelihood:" sample_internal_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_deg2_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_leaf_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> CondLikes -> Alphabet -> EVector Int -> EVector Int -> VectorPairIntInt



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

cached_conditional_likelihoods_SEV t seqs alpha ps smap =
    let lc    = IntMap.fromSet lcf $ getEdgesSet t
        lcf b = let p = ps IntMap.! b
                    inEdges = edgesBeforeEdgeSet t b
                    clsIn = IntMap.restrictKeysToVector lc inEdges
                    -- FIXME!  How to determine if the node has any data?
                    node = sourceNode t b
                    sequences = maybeToList $ c_pair' <$> seqs IntMap.! node
                in peelBranchSEV sequences alpha smap clsIn p
    in lc

peel_likelihood_SEV seqs t cls f alpha smap root counts = let inEdges = edgesTowardNodeSet t root
                                                              sequences = maybeToList $ c_pair' <$> seqs IntMap.! root
                                                              clsIn = IntMap.restrictKeysToVector cls inEdges
                                                          in calcRootProbSEV (list_to_vector sequences) alpha smap clsIn f counts

sample_ancestral_sequences_SEV t root seqsBits alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node (getNodesSet t)
        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)
        ancestor_for_branch n Nothing = let sequences = if is_leaf_node t n then [c_pair' $ seqsBits IntMap.! n] else []
                                            inEdges = edgesTowardNodeSet t n
                                            clsIn = IntMap.restrictKeysToVector cl inEdges
                                        in sampleRootSequenceSEV (list_to_vector sequences)
                                                                 alpha
                                                                 smap
                                                                 clsIn
                                                                 f
                                                                 col_to_compressed
        ancestor_for_branch n (Just to_p) = let parent_seq = ancestor_seqs IntMap.! (targetNode t to_p)
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                inEdges = edgesBeforeEdgeSet t to_p
                                                clsIn = IntMap.restrictKeysToVector cl inEdges
                                                sequences = if is_leaf_node t n then [c_pair' $ seqsBits IntMap.! n] else []
                                            in sampleSequenceSEV parent_seq
                                                                 (list_to_vector sequences)
                                                                 alpha
                                                                 smap
                                                                 ps_for_b0
                                                                 clsIn
                                                                 col_to_compressed
    in ancestor_seqs
                                                                       
