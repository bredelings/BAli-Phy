module Bio.Alignment (module Bio.Alignment,
                      module Bio.Sequence,
                      module Bio.Alignment.Matrix,
                      module Bio.Alignment.Pairwise) where

import Tree
import Data.BitVector
import Parameters
import Foreign.Vector
import Foreign.Pair
import Bio.Sequence
import Bio.Alphabet -- for Alphabet type
import Bio.Alignment.Matrix
import Bio.Alignment.Pairwise

data VectorPairIntInt -- ancestral sequences with (int letter, int category) for each site.

foreign import bpcall "Alignment:leaf_sequence_counts" builtin_leaf_sequence_counts :: AlignmentMatrix -> Int -> EVector Int -> EVector (EVector Int)

branch_hmms (model,_) distances n_branches = listArray' $ map (model distances) [0..n_branches-1]
  
seqlength as tree node = pairwise_alignment_length1 (as!b) where
    b = head $ edgesOutOfNode tree node


minimally_connect_characters a t = mkArray (numNodes t) node_to_bits where
    pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
    node_to_bits n = if is_leaf_node t n then
                         alignment_row_to_bitvector a n
                     else
                         foldl1 (.|.) $ [(character_behind_branch_array!b1) .&. (character_behind_branch_array!b2) | (b1,b2) <- pairs $ edgesTowardNode t n]
    character_behind_branch b = case edgesBeforeEdge t b of [] -> alignment_row_to_bitvector a (sourceNode t b)
                                                            [b1,b2] -> (character_behind_branch_array!b1) .|. (character_behind_branch_array!b2)
    character_behind_branch_array = mkArray (2*numBranches t) character_behind_branch

pairwise_alignments_from_matrix a tree = [ pairwise_alignment_from_bits bits1 bits2 | b <- [0..2*numBranches tree-1],
                                                                                           let bits1 = bits ! sourceNode tree b,
                                                                                           let bits2 = bits ! targetNode tree b]
    where bits = minimally_connect_characters a tree

-- We can't just do forall t.AlignmentOnTree t, because then any constraints on t will be on existential variables, resulting in ambiguity.
data AlignmentOnTree t = AlignmentOnTree t Int (Array Int Int) (Array Int PairwiseAlignment)
n_sequences         (AlignmentOnTree _ n _  _) = n
sequence_lengths    (AlignmentOnTree _ _ ls _) = ls
pairwise_alignments (AlignmentOnTree _ _ _ as) = as

-- not using this right now
-- get_sequence_lengths leaf_seqs_array = mkArray (length leaf_seqs_array) (\node -> vector_size (leaf_seqs_array!node))

-- This function handles the case where we have only 1 sequence.
compute_sequence_lengths seqs tree as = [ if node < n_leaves then vector_size (seqs!node) else seqlength as tree node | node <- [0..numNodes tree-1] ]
    where n_leaves = length seqs

-- Current a' is an alignment, but counts and mapping are EVector
-- AlignmentMatrix -> ETuple (AlignmentMatrix, EVector Int, EVector Int)
foreign import bpcall "Alignment:compress_alignment" builtin_compress_alignment :: AlignmentMatrix ->
                                                                                   EPair AlignmentMatrix (EPair (EVector Int) (EVector Int))
compress_alignment a = (compressed, counts, mapping) where tmp123 = builtin_compress_alignment a
                                                           (compressed, tmp23) = pair_from_c tmp123
                                                           (counts,   mapping) = pair_from_c tmp23

alignment_on_tree_length (AlignmentOnTree t _ ls as) = (ls!0) + sum [numInsert (as!b) | b <- allEdgesFromNode t 0]

foreign import bpcall "Alignment:uncompress_alignment" builtin_uncompress_alignment :: AlignmentMatrix -> EVector Int -> AlignmentMatrix

uncompress_alignment (a, counts, mapping) = builtin_uncompress_alignment a mapping

-- Alignment -> Int -> EVector Int -> [EVector Int]
leaf_sequence_counts a n counts = list_from_vector $ builtin_leaf_sequence_counts a n counts

foreign import bpcall "Alignment:ancestral_sequence_alignment" builtin_ancestral_sequence_alignment :: AlignmentMatrix -> EVector VectorPairIntInt -> EVector Int -> AlignmentMatrix
ancestral_sequence_alignment a0 states smap = builtin_ancestral_sequence_alignment a0 states smap

foreign import bpcall "Alignment:select_alignment_columns" builtin_select_alignment_columns :: AlignmentMatrix -> EVector Int -> AlignmentMatrix
select_alignment_columns alignment sites = builtin_select_alignment_columns alignment (list_to_vector sites)

foreign import bpcall "Alignment:select_alignment_pairs" builtin_select_alignment_pairs :: AlignmentMatrix -> EVector (EPair Int Int) -> Alphabet -> AlignmentMatrix
select_alignment_pairs alignment sites doublets = builtin_select_alignment_pairs alignment sites' doublets
    where sites' = list_to_vector $ map (\(x,y) -> c_pair x y) sites
