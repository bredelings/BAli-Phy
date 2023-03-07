module SModel.Likelihood  where 

import Tree
import Bio.Alphabet
import Bio.Alignment
import Data.BitVector
import Data.Foldable
import Data.Matrix
import Data.Array
import Foreign.Vector

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data CondLikes

-- peeling for connected-CLVs
foreign import bpcall "SModel:" peel_leaf_branch :: EVector Int -> Alphabet -> EVector (Matrix Double) -> EVector Int -> CondLikes
foreign import bpcall "SModel:" alignment_index2 :: PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "SModel:" alignment_index3 :: PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "SModel:" peel_internal_branch :: CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "SModel:" calc_root_probability :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "SModel:" peel_likelihood_2 :: EVector Int -> EVector Int -> Alphabet -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> LogDouble
foreign import bpcall "SModel:" peel_likelihood_1 :: EVector Int -> Alphabet -> Matrix Double -> LogDouble

-- ancestral sequence sampling for connected-CLVs
foreign import bpcall "SModel:" sample_root_sequence :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "SModel:" sample_internal_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt
foreign import bpcall "SModel:" sample_leaf_sequence :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> Alphabet -> EVector Int -> PairwiseAlignment -> Matrix Double -> VectorPairIntInt

-- peeling for SEV
foreign import bpcall "Alignment:" bitmask_from_alignment :: AlignmentMatrix -> Int -> CBitVector
foreign import bpcall "SModel:" peel_leaf_branch_SEV :: EVector Int -> Alphabet -> EVector (Matrix Double) -> CBitVector -> EVector Int -> CondLikes
foreign import bpcall "SModel:" peel_internal_branch_SEV :: CondLikes -> CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "SModel:" peel_deg2_branch_SEV :: CondLikes -> EVector (Matrix Double) -> CondLikes
foreign import bpcall "SModel:" calc_root_probability_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:" calc_root_deg2_probability_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:" peel_likelihood_2_SEV :: AlignmentMatrix -> Alphabet -> EVector (Matrix Double) -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "SModel:" peel_likelihood_1_SEV :: AlignmentMatrix -> Alphabet -> Matrix Double -> EVector Int -> LogDouble

-- ancestral sequence sampling for SEV
foreign import bpcall "SModel:" sample_root_sequence_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:" sample_root_deg2_sequence_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:" sample_internal_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:" sample_deg2_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "SModel:" sample_leaf_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> CondLikes -> Alphabet -> EVector Int -> EVector Int -> VectorPairIntInt


cached_conditional_likelihoods t seqs as alpha ps f smap = let lc    = mkArray (2*numBranches t) lcf
                                                               lcf b = let p = ps!(undirectedName t b)
                                                                           edges = edgesBeforeEdge t b
                                                                           b1 = edges!0
                                                                           b2 = edges!1
                                                                       in case numElements edges of
                                                                            0 -> let n=sourceNode t b in peel_leaf_branch (seqs IntMap.! n) alpha p smap
                                                                            2 -> peel_internal_branch (lc!b1) (lc!b2) (as IntMap.! b1) (as IntMap.! b2) p f
                                                           in lc

peel_likelihood t cl as f root = let likelihoods = mkArray (numNodes t) peel_likelihood'
                                     peel_likelihood' root = let branches_in = edgesTowardNode t root
                                                                 b1 = branches_in!0
                                                                 b2 = branches_in!1
                                                                 b3 = branches_in!2
                                                             in calc_root_probability (cl!b1) (cl!b2) (cl!b3) (as IntMap.! b1) (as IntMap.! b2) (as IntMap.! b3) f
                                 in likelihoods!root


substitution_likelihood t root seqs as alpha ps f smap = let cl = cached_conditional_likelihoods t seqs as alpha ps f smap
                                                         in peel_likelihood t cl as f root

sample_ancestral_sequences :: Tree t =>
                              t ->
                              Int ->
                              IntMap (EVector Int) ->
                              IntMap PairwiseAlignment ->
                              Alphabet ->
                              Array Int (EVector (Matrix Double)) ->
                              Matrix Double ->
                              Array Int CondLikes ->
                              EVector Int ->
                              Array Int VectorPairIntInt
sample_ancestral_sequences t root seqs as alpha ps f cl smap =
    let rt = add_root root t
        ancestor_seqs = mkArray (numNodes t) ancestor_for_node
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = sample_root_sequence (cl!b0) (cl!b1) (cl!b2) (as IntMap.! b0) (as IntMap.! b1) (as IntMap.! b2) f where edges = edgesTowardNode t n
                                                                                                                                                b0 = edges!0
                                                                                                                                                b1 = edges!1
                                                                                                                                                b2 = edges!2
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs!p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps!(undirectedName t b0)
                                                a0 = as IntMap.! b0
                                                edges = edgesBeforeEdge t to_p
                                                b1 = edges!0
                                                b2 = edges!1
                                            in case numElements edges of
                                                 0 -> sample_leaf_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs IntMap.! n)
                                                          alpha
                                                          smap
                                                          a0
                                                          f
                                                 2 -> sample_internal_sequence
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl!b1)
                                                          (cl!b2)
                                                          a0
                                                          (as IntMap.! b1)
                                                          (as IntMap.! b2)
                                                          f
    in ancestor_seqs

cached_conditional_likelihoods_SEV t seqs alpha ps a smap =
    let lc    = mkArray (2*numBranches t) lcf
        lcf b = let p = ps!(undirectedName t b)
                    edges = edgesBeforeEdge t b
                    b1 = edges!0
                    b2 = edges!1
                in case numElements edges of
                     0 -> peel_leaf_branch_SEV (seqs IntMap.! sourceNode t b) alpha p (bitmask_from_alignment a $ sourceNode t b) smap
                     1 -> peel_deg2_branch_SEV (lc!b1) p
                     2 -> peel_internal_branch_SEV (lc!b1) (lc!b2) p
    in lc

peel_likelihood_SEV t cl f root counts = let branches_in = fmap (reverseEdge t) (edgesOutOfNode t root)
                                             b1 = branches_in!0
                                             b2 = branches_in!1
                                             b3 = branches_in!2
                                         in case numElements branches_in of
                                              3 -> calc_root_probability_SEV (cl!b1) (cl!b2) (cl!b3) f counts
                                              2 -> calc_root_deg2_probability_SEV (cl!b1) (cl!b2) f counts

sample_ancestral_sequences_SEV t root seqs alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = mkArray (numNodes t) ancestor_for_node
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = let edges = edgesTowardNode t n
                                            b0 = edges!0
                                            b1 = edges!1
                                            b2 = edges!2
                                        in case numElements edges of
                                             3 -> sample_root_sequence_SEV (cl!b0) (cl!b1) (cl!b2) f col_to_compressed
                                             2 -> sample_root_deg2_sequence_SEV (cl!b0) (cl!b1) f col_to_compressed
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs!p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps!(undirectedName t b0)
                                                edges = edgesBeforeEdge t to_p
                                                b1 = edges!0
                                                b2 = edges!1
                                            in case numElements edges of
                                                 0 -> sample_leaf_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs IntMap.! n)
                                                          (cl!to_p)
                                                          alpha
                                                          smap
                                                          col_to_compressed
                                                 1 -> sample_deg2_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl!b1)
                                                          col_to_compressed
                                                 2 -> sample_internal_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl!b1)
                                                          (cl!b2)
                                                          col_to_compressed
    in ancestor_seqs
                                                                       
