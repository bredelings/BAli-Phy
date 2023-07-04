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

data CondLikes

-- peeling for connected-CLVs
foreign import bpcall "Likelihood:" peel_leaf_branch :: EVector Int -> Alphabet -> EVector (Matrix Double) -> EVector Int -> CondLikes
foreign import bpcall "Likelihood:" alignment_index2 :: PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "Likelihood:" alignment_index3 :: PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Int
foreign import bpcall "Likelihood:" peel_internal_branch :: CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> CondLikes
foreign import bpcall "Likelihood:" calc_root_probability :: CondLikes -> CondLikes -> CondLikes -> PairwiseAlignment -> PairwiseAlignment -> PairwiseAlignment -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_2 :: EVector Int -> EVector Int -> Alphabet -> PairwiseAlignment -> EVector (Matrix Double) -> Matrix Double -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_1 :: EVector Int -> Alphabet -> Matrix Double -> LogDouble

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
foreign import bpcall "Likelihood:" peel_likelihood_2_SEV :: AlignmentMatrix -> Alphabet -> EVector (Matrix Double) -> Matrix Double -> EVector Int -> LogDouble
foreign import bpcall "Likelihood:" peel_likelihood_1_SEV :: AlignmentMatrix -> Alphabet -> Matrix Double -> EVector Int -> LogDouble

-- ancestral sequence sampling for SEV
foreign import bpcall "Likelihood:" sample_root_sequence_SEV :: CondLikes -> CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_root_deg2_sequence_SEV :: CondLikes -> CondLikes -> Matrix Double -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_internal_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_deg2_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> CondLikes -> EVector Int -> VectorPairIntInt
foreign import bpcall "Likelihood:" sample_leaf_sequence_SEV :: VectorPairIntInt -> EVector (Matrix Double) -> EVector Int -> CondLikes -> Alphabet -> EVector Int -> EVector Int -> VectorPairIntInt



cached_conditional_likelihoods t seqs as alpha ps f smap = let lc    = IntMap.fromSet lcf $ getEdgesSet t
                                                               lcf b = let p = ps IntMap.! b
                                                                           edges = edgesBeforeEdge t b
                                                                           b1 = edges!0
                                                                           b2 = edges!1
                                                                       in case numElements edges of
                                                                            0 -> let n=sourceNode t b in peel_leaf_branch (seqs IntMap.! n) alpha p smap
                                                                            2 -> peel_internal_branch (lc IntMap.! b1) (lc IntMap.! b2) (as IntMap.! b1) (as IntMap.! b2) p f
                                                                            _ -> error $ "bad number of edges: " ++ show (numElements edges)
                                                           in lc

peel_likelihood t cl as f root = let likelihoods = IntMap.fromSet peel_likelihood' $ getNodesSet t
                                     peel_likelihood' root = let branches_in = edgesTowardNode t root
                                                                 b1 = branches_in!0
                                                                 b2 = branches_in!1
                                                                 b3 = branches_in!2
                                                             in calc_root_probability (cl IntMap.! b1) (cl IntMap.! b2) (cl IntMap.! b3) (as IntMap.! b1) (as IntMap.! b2) (as IntMap.! b3) f
                                 in likelihoods IntMap.! root


substitution_likelihood t root seqs as alpha ps f smap = let cl = cached_conditional_likelihoods t seqs as alpha ps f smap
                                                         in peel_likelihood t cl as f root

sample_ancestral_sequences :: Tree t =>
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
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = sample_root_sequence (cl IntMap.! b0) (cl IntMap.! b1) (cl IntMap.! b2)
                                                             (as IntMap.! b0) (as IntMap.! b1) (as IntMap.! b2)
                                                             f
            where edges = edgesTowardNode t n
                  b0 = edges!0
                  b1 = edges!1
                  b2 = edges!2

        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs IntMap.! p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps IntMap.! b0
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
                                                          (cl IntMap.! b1)
                                                          (cl IntMap.! b2)
                                                          a0
                                                          (as IntMap.! b1)
                                                          (as IntMap.! b2)
                                                          f
                                                 _ -> error $ "bad number of edges: " ++ show (numElements edges)
    in ancestor_seqs

cached_conditional_likelihoods_SEV t seqs alpha ps smap =
    let lc    = IntMap.fromSet lcf $ getEdgesSet t
        lcf b = let p = ps IntMap.! b
                    edges = edgesBeforeEdge t b
                    b1 = edges!0
                    b2 = edges!1
                in case numElements edges of
                     0 -> let node = sourceNode t b
                              (node_seq, bitmask) = seqs IntMap.! node
                          in peel_leaf_branch_SEV node_seq alpha p bitmask smap
                     1 -> peel_deg2_branch_SEV (lc IntMap.! b1) p
                     2 -> peel_internal_branch_SEV (lc IntMap.! b1) (lc IntMap.! b2) p
    in lc

peel_likelihood_SEV t cl f root counts = let branches_in = fmap (reverseEdge t) (edgesOutOfNode t root)
                                             b1 = branches_in!0
                                             b2 = branches_in!1
                                             b3 = branches_in!2
                                         in case numElements branches_in of
                                              3 -> calc_root_probability_SEV (cl IntMap.! b1) (cl IntMap.! b2) (cl IntMap.! b3) f counts
                                              2 -> calc_root_deg2_probability_SEV (cl IntMap.! b1) (cl IntMap.! b2) f counts

sample_ancestral_sequences_SEV t root seqs alpha ps f cl smap col_to_compressed =
    let rt = add_root root t
        ancestor_seqs = IntMap.fromSet ancestor_for_node (getNodesSet t)
        ancestor_for_node n = ancestor_for_branch n (parentBranch rt n)
        ancestor_for_branch n Nothing = let edges = edgesTowardNode t n
                                            b0 = edges!0
                                            b1 = edges!1
                                            b2 = edges!2
                                        in case numElements edges of
                                             3 -> sample_root_sequence_SEV (cl IntMap.! b0) (cl IntMap.! b1) (cl IntMap.! b2) f col_to_compressed
                                             2 -> sample_root_deg2_sequence_SEV (cl IntMap.! b0) (cl IntMap.! b1) f col_to_compressed
        ancestor_for_branch n (Just to_p) = let p = targetNode t to_p
                                                parent_seq = ancestor_seqs IntMap.! p
                                                b0 = reverseEdge t to_p
                                                ps_for_b0 = ps IntMap.! b0
                                                edges = edgesBeforeEdge t to_p
                                                b1 = edges!0
                                                b2 = edges!1
                                            in case numElements edges of
                                                 0 -> sample_leaf_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (seqs IntMap.! n)
                                                          (cl IntMap.! to_p)
                                                          alpha
                                                          smap
                                                          col_to_compressed
                                                 1 -> sample_deg2_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl IntMap.! b1)
                                                          col_to_compressed
                                                 2 -> sample_internal_sequence_SEV
                                                          parent_seq
                                                          ps_for_b0
                                                          (cl IntMap.! b1)
                                                          (cl IntMap.! b2)
                                                          col_to_compressed
    in ancestor_seqs
                                                                       
