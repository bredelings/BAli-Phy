module Alignment where

import Tree
import Data.BitVector
import Parameters
import Probability

builtin pairwise_alignment_length1 1 "pairwise_alignment_length1" "Alignment"
builtin pairwise_alignment_length2 1 "pairwise_alignment_length2" "Alignment"
builtin transition_counts 1 "transition_counts" "Alignment"
builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts" "Alignment"

builtin builtin_load_alignment 2 "load_alignment" "Alignment"
builtin builtin_sequences_from_alignment 1 "sequences_from_alignment" "Alignment"

builtin builtin_alignment_row_to_bitvector 2 "alignment_row_to_presence_bitvector" "Bits"
builtin builtin_pairwise_alignment_from_bits 2 "pairwise_alignment_from_bits" "Bits"
builtin unaligned_pairwise_alignment 2 "unaligned_pairwise_alignment" "Alignment"
builtin flip_alignment 1 "flip_alignment" "Alignment"

branch_hmms (model,_) distances n_branches = listArray' $ map (model distances) [0..2*n_branches-1]
  
alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a!b)) (hmms!b)
  
seqlength as tree node = pairwise_alignment_length1 (as!b) where
    b = head $ edgesOutOfNode tree node

product' = foldl' (*) (doubleToLogDouble 1.0)
alignment_pr_top as tree hmms = product' $ map (alignment_branch_pr as hmms) [0..numBranches tree - 1]
alignment_pr_bot as tree (_,lengthp) = (product' $ map (lengthp . seqlength as tree) (internal_nodes tree))^2
alignment_pr1 length (_,lengthp) = lengthp length
load_alignment alphabet filename = builtin_load_alignment alphabet (listToString filename)
-- sequence_from_alignment :: AlignmentMatrix -> [ Vector<int> ]
sequences_from_alignment a = list_from_vector $ builtin_sequences_from_alignment a

alignment_row_to_bitvector a row = BitVector $ builtin_alignment_row_to_bitvector a row

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

pairwise_alignment_from_bits (BitVector x) (BitVector y) = builtin_pairwise_alignment_from_bits x y

data PairwiseAlignment = PairwiseAlignment

data AlignmentOnTree = AlignmentOnTree Tree Int (Array Int Int) (Array Int PairwiseAlignment)
n_sequences         (AlignmentOnTree _ n _  _) = n
sequence_lengths    (AlignmentOnTree _ _ ls _) = ls
pairwise_alignments (AlignmentOnTree _ _ _ as) = as

alignment_pr (AlignmentOnTree tree n_seqs lengths as) hmms model = if numElements lengths == 1 then
                                                                       alignment_pr1 (lengths!0) model 
                                                                   else
                                                                       (alignment_pr_top as tree hmms)/(alignment_pr_bot as tree model)

modifiable_alignment (AlignmentOnTree tree n_seqs seqlengths as) = AlignmentOnTree tree n_seqs (map modifiable seqlengths) (map modifiable as)

random_alignment tree hmms model = ProbDensity (\a -> [alignment_pr a hmms model]) (no_quantile "random_alignment") (RandomStructure modifiable_alignment ()) ()

compute_sequence_lengths seqs tree as = [ if node < n_leaves then vector_size (seqs!node) else seqlength as tree node | node <- [0..numNodes tree-1] ]
    where n_leaves = numElements seqs
