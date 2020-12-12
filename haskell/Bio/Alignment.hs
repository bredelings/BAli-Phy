module Bio.Alignment (module Bio.Alignment,
                      module Bio.Sequence,
                      module Bio.Alignment.Matrix,
                      module Bio.Alignment.Pairwise) where

import Tree
import Data.BitVector
import Parameters
import Foreign.Vector
import Bio.Sequence
import Bio.Alignment.Matrix
import Bio.Alignment.Pairwise

-- Alignment -> Int -> EVector Int-> EVector (EVector Int)
builtin builtin_leaf_sequence_counts 3 "leaf_sequence_counts" "Alignment"

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

data AlignmentOnTree = AlignmentOnTree Tree Int (Array Int Int) (Array Int PairwiseAlignment)
n_sequences         (AlignmentOnTree _ n _  _) = n
sequence_lengths    (AlignmentOnTree _ _ ls _) = ls
pairwise_alignments (AlignmentOnTree _ _ _ as) = as

-- not using this right now
-- get_sequence_lengths leaf_seqs_array = mkArray (numElements leaf_seqs_array) (\node -> vector_size (leaf_seqs_array!node))

-- This function handles the case where we have only 1 sequence.
compute_sequence_lengths seqs tree as = [ if node < n_leaves then vector_size (seqs!node) else seqlength as tree node | node <- [0..numNodes tree-1] ]
    where n_leaves = numElements seqs

-- Current a' is an alignment, but counts and mapping are EVector
builtin builtin_compress_alignment 1 "compress_alignment" "Alignment"
compress_alignment a = (compressed, counts, mapping) where ca = builtin_compress_alignment a
                                                           compressed = get_vector_index ca 0
                                                           counts = get_vector_index ca 1
                                                           mapping = get_vector_index ca 2


builtin builtin_uncompress_alignment 2 "uncompress_alignment" "Alignment"

uncompress_alignment (a, counts, mapping) = builtin_uncompress_alignment a mapping

-- Alignment -> Int -> EVector Int -> [EVector Int]
leaf_sequence_counts a n counts = list_from_vector $ builtin_leaf_sequence_counts a n counts

builtin builtin_ancestral_sequence_alignment 3 "ancestral_sequence_alignment" "Alignment"
ancestral_sequence_alignment a0 states smap = builtin_ancestral_sequence_alignment a0 states smap
