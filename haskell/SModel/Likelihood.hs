module SModel.Likelihood  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Matrix
import Foreign.Vector

data CondLikes

-- peeling for connected-CLVs
foreign import bpcall "SModel:peel_leaf_branch" peel_leaf_branch :: EVector Int -> Alphabet -> EVector (Matrix Double) -> EVector Int -> CondLikes
foreign import bpcall "SModel:alignment_index2" alignment_index2 :: PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "SModel:alignment_index3" alignment_index3 :: PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "SModel:peel_internal_branch" peel_internal_branch :: CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "SModel:calc_root_probability" calc_root_probability :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "SModel:peel_likelihood_2" peel_likelihood_2 :: EVector Int -> EVector Int -> Alphabet -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> LogDouble
foreign import bpcall "SModel:peel_likelihood_1" peel_likelihood_1 :: EVector Int -> Alphabet -> Matrix Double -> LogDouble

-- ancestral sequence sampling for connected-CLVs
data VectorPairIntInt
foreign import bpcall "SModel:sample_root_sequence" sample_root_sequence :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "SModel:sample_internal_node_sequence" sample_internal_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "SModel:sample_leaf_node_sequence" sample_leaf_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> Alphabet -> EVector Int -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt

-- peeling for SEV
foreign import bpcall "Alignment:bitmask_from_alignment" bitmask_from_alignment :: AlignmentMatrix -> Int -> CBitVector
foreign import bpcall "SModel:peel_leaf_branch_SEV" peel_leaf_branch_SEV :: EVector Int -> Alphabet -> EVector (Matrix Double) -> CBitVector -> EVector Int -> CondLikes
foreign import bpcall "SModel:peel_internal_branch_SEV" peel_internal_branch_SEV :: CondLikes -> CondLikes -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "SModel:peel_deg2_branch_SEV" peel_deg2_branch_SEV :: CondLikes -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "SModel:calc_root_probability_SEV" calc_root_probability_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:calc_root_deg2_probability_SEV" calc_root_deg2_probability_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:peel_likelihood_2_SEV" peel_likelihood_2_SEV :: AlignmentMatrix -> Alphabet -> EVector (Matrix Double) -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:peel_likelihood_1_SEV" peel_likelihood_1_SEV :: AlignmentMatrix -> Alphabet -> Matrix Double -> EVector Int -> LogDouble

-- ancestral sequence sampling for SEV
foreign import bpcall "SModel:sample_root_sequence_SEV" sample_root_sequence_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:sample_root_deg2_sequence_SEV" sample_root_deg2_sequence_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:sample_internal_node_sequence_SEV" sample_internal_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:sample_deg2_node_sequence_SEV" sample_deg2_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:sample_leaf_node_sequence_SEV" sample_leaf_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> CondLikes -> Alphabet -> EVector Int -> EVector Int -> VectorPairIntInt


cached_conditional_likelihoods t seqs as alpha ps f smap = let lc    = mkArray (2*numBranches t) lcf
                                                               lcf b = let bb = b `mod` (numBranches t)
                                                                       in case edgesBeforeEdge t b of 
                                                                            []      -> let n=sourceNode t b
                                                                                       in peel_leaf_branch (seqs!n) alpha (ps!bb) smap
                                                                            [b1,b2] -> peel_internal_branch (lc!b1) (lc!b2) (as!b1) (as!b2) (ps!bb) f
                                                           in lc

peel_likelihood t cl as f root = let likelihoods = mkArray (numNodes t) peel_likelihood'
                                     peel_likelihood' root = let branches_in = edgesTowardNode t root
                                                             in case branches_in of [b1,b2,b3]-> calc_root_probability (cl!b1) (cl!b2) (cl!b3) (as!b1) (as!b2) (as!b3) f
                                 in likelihoods!root


substitution_likelihood t root seqs as alpha ps f smap = let cl = cached_conditional_likelihoods t seqs as alpha ps f smap
                                                         in peel_likelihood t cl as f root

sample_ancestral_sequences t root seqs as alpha ps f cl smap =
    let rt = add_root root t
        ancestor_seqs = mkArray (numNodes t) ancestor_for_node
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = sample_root_sequence (cl!b0) (cl!b1) (cl!b2) (as!b0) (as!b1) (as!b2) f where [b0,b1,b2] = edgesTowardNode t n
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs!p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps!(b0 `mod` (numBranches t))
                                                a0 = as!b0
                                            in case edgesBeforeEdge t to_p of
                                                 [] -> sample_leaf_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs!n)
                                                          alpha
                                                          smap
                                                          a0
                                                          f
                                                 [b1,b2] -> sample_internal_sequence
                                                               parent_seq
                                                               ps_for_b0
                                                               (cl!b1)
                                                               (cl!b2)
                                                               a0
                                                               (as!b1)
                                                               (as!b2)
                                                               f
    in ancestor_seqs

cached_conditional_likelihoods_SEV t seqs alpha ps f a smap =
    let lc    = mkArray (2*numBranches t) lcf
        lcf b = let bb = b `mod` (numBranches t) in
                case edgesBeforeEdge t b of []      -> peel_leaf_branch_SEV (seqs!sourceNode t b) alpha (ps!bb) (bitmask_from_alignment a $ sourceNode t b) smap
                                            [b1]    -> peel_deg2_branch_SEV (lc!b1) (ps!bb) f
                                            [b1,b2] -> peel_internal_branch_SEV (lc!b1) (lc!b2) (ps!bb) f
    in lc

peel_likelihood_SEV t cl f root counts = let branches_in = map (reverseEdge t) (edgesOutOfNode t root) in
                                         case branches_in of [b1,b2,b3]-> calc_root_probability_SEV (cl!b1) (cl!b2) (cl!b3) f counts
                                                             [b1,b2] -> calc_root_deg2_probability_SEV (cl!b1) (cl!b2) f counts

sample_ancestral_sequences_SEV t root seqs alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = mkArray (numNodes t) ancestor_for_node
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = case edgesTowardNode t n of [b0,b1,b2] -> sample_root_sequence_SEV (cl!b0) (cl!b1) (cl!b2) f col_to_compressed
                                                                    [b0,b1]    -> sample_root_deg2_sequence_SEV (cl!b0) (cl!b1) f col_to_compressed
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs!p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps!(b0 `mod` (numBranches t))
                                            in case edgesBeforeEdge t to_p of
                                                 [] -> sample_leaf_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs!n)
                                                          (cl!to_p)
                                                          alpha
                                                          smap
                                                          col_to_compressed
                                                 [b1] -> sample_deg2_sequence_SEV
                                                               parent_seq
                                                               ps_for_b0
                                                               (cl!b1)
                                                               col_to_compressed
                                                 [b1,b2] -> sample_internal_sequence_SEV
                                                               parent_seq
                                                               ps_for_b0
                                                               (cl!b1)
                                                               (cl!b2)
                                                               col_to_compressed
    in ancestor_seqs
                                                                       
