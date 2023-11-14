module SModel.Likelihood  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
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
foreign import bpcall "Likelihood:peelBranch" builtinPeelBranch :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes

peelBranch sequences alphabet smap cls as ps f = builtinPeelBranch (list_to_vector sequences) alphabet smap cls as ps f


-- ancestral sequence sampling for connected-CLVs
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
foreign import bpcall "Likelihood:" calcRootProb :: EVector (EVector Int) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:peelBranchSEV" builtinPeelBranchSEV :: EVector (EPair (EVector Int) CBitVector) -> Alphabet -> EVector Int -> EVector CondLikes -> EVector (Matrix Double) -> CondLikes

peelBranchSEV sequences alphabet smap cls ps = builtinPeelBranchSEV (list_to_vector sequences) alphabet smap (list_to_vector cls) ps

-- ancestral sequence sampling for SEV
foreign import bpcall "Likelihood:" sample_root_sequence_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_root_deg2_sequence_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_internal_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_deg2_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_leaf_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> CondLikes -> Alphabet -> EVector Int -> EVector Int -> VectorPairIntInt



cached_conditional_likelihoods t seqs as alpha ps f smap = let lc    = IntMap.fromSet lcf $ getEdgesSet t
                                                               lcf b = let p = ps IntMap.! b
                                                                           inEdges = edgesBeforeEdgeSet t b
                                                                           -- FIXME!  How to determine if the node has any data?
                                                                           sequences = if IntSet.null inEdges then [seqs IntMap.! (sourceNode t b)] else []
                                                                           clsIn = IntMap.restrictKeysToVector lc inEdges
                                                                           asIn  = IntMap.restrictKeysToVector as inEdges
                                                                       in peelBranch sequences alpha smap clsIn asIn p f
                                                           in lc

peel_likelihood t seqs cls as alpha f smap root = let likelihoods = IntMap.fromSet peel_likelihood_at_node $ getNodesSet t
                                                      peel_likelihood_at_node root = let inEdges = edgesTowardNodeSet t root
                                                                                         sequences = if is_leaf_node t root then [seqs IntMap.! root] else []
                                                                                         clsIn = IntMap.restrictKeysToVector cls inEdges
                                                                                         asIn  = IntMap.restrictKeysToVector as inEdges
                                                                                     in calcRootProb (list_to_vector sequences) alpha smap clsIn asIn f
                                                  in likelihoods IntMap.! root


substitution_likelihood t root seqs as alpha ps f smap = let cls = cached_conditional_likelihoods t seqs as alpha ps f smap
                                                         in peel_likelihood t seqs cls as alpha f smap root

sample_ancestral_sequences :: IsTree t =>
                              t ->
                              Int ->
                              IntMap (EVector Int) ->
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

        ancestor_for_branch n Nothing = case edgesTowardNode t n of
                                          [b0,b1,b2] -> sample_root_sequence (cl IntMap.! b0) (cl IntMap.! b1) (cl IntMap.! b2)
                                                            (as IntMap.! b0) (as IntMap.! b1) (as IntMap.! b2)
                                                            f
                                          e -> error $ "ancestor_for_branch: bad number of nodes " ++ show (length e)
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs IntMap.! p
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                a0 = as IntMap.! b0
                                            in case edgesBeforeEdge t to_p of
                                                 [] -> sample_leaf_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs IntMap.! n)
                                                          alpha
                                                          smap
                                                          a0
                                                          f
                                                 [b1,b2] -> sample_internal_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl IntMap.! b1)
                                                          (cl IntMap.! b2)
                                                          a0
                                                          (as IntMap.! b1)
                                                          (as IntMap.! b2)
                                                          f
                                                 e -> error $ "bad number of edges: " ++ show (length e)
    in ancestor_seqs

cached_conditional_likelihoods_SEV t seqs alpha ps smap =
    let lc    = IntMap.fromSet lcf $ getEdgesSet t
        lcf b = let p = ps IntMap.! b
                in case edgesBeforeEdge t b of
                     [] -> let node = sourceNode t b
                               (node_seq, bitmask) = seqs IntMap.! node
                           in peel_leaf_branch_SEV node_seq alpha p bitmask smap
                     [b1] -> peel_deg2_branch_SEV (lc IntMap.! b1) p
                     [b1,b2] -> peelBranchSEV [] alpha smap [lc IntMap.! b1, lc IntMap.! b2] p
    in lc

peel_likelihood_SEV t cl f root counts = case edgesTowardNode t root of
                                              [b1,b2,b3] -> calc_root_probability_SEV      (cl IntMap.! b1) (cl IntMap.! b2) (cl IntMap.! b3) f counts
                                              [b1,b2]    -> calc_root_deg2_probability_SEV (cl IntMap.! b1) (cl IntMap.! b2) f counts

sample_ancestral_sequences_SEV t root seqs alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node (getNodesSet t)
        ancestor_for_node n = ancestor_for_branch n (branchToParent rt n)
        ancestor_for_branch n Nothing = case edgesTowardNode t n of
                                             [b0,b1,b2] -> sample_root_sequence_SEV (cl IntMap.! b0) (cl IntMap.! b1) (cl IntMap.! b2) f col_to_compressed
                                             [b0,b1] -> sample_root_deg2_sequence_SEV (cl IntMap.! b0) (cl IntMap.! b1) f col_to_compressed
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs IntMap.! p
                                                b0 = reverseEdge to_p
                                                ps_for_b0 = ps IntMap.! b0
                                            in case edgesBeforeEdge t to_p of
                                                 [] -> sample_leaf_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs IntMap.! n)
                                                          (cl IntMap.! to_p)
                                                          alpha
                                                          smap
                                                          col_to_compressed
                                                 [b1] -> sample_deg2_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl IntMap.! b1)
                                                          col_to_compressed
                                                 [b1,b2] -> sample_internal_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl IntMap.! b1)
                                                          (cl IntMap.! b2)
                                                          col_to_compressed
    in ancestor_seqs
                                                                       
