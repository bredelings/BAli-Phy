module Alignment where

import Tree
import Data.BitVector
import Parameters
import Probability

builtin pairwise_alignment_length1 1 "pairwise_alignment_length1" "Alignment"
builtin pairwise_alignment_length2 1 "pairwise_alignment_length2" "Alignment"
builtin alignment_length 1 "alignment_length" "Alignment"
builtin transition_counts 1 "transition_counts" "Alignment"
builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts" "Alignment"

builtin builtin_load_alignment 2 "load_alignment" "Alignment"
builtin builtin_sequences_from_alignment 1 "sequences_from_alignment" "Alignment"

builtin builtin_alignment_row_to_bitvector 2 "alignment_row_to_presence_bitvector" "Bits"
builtin builtin_pairwise_alignment_from_bits 2 "pairwise_alignment_from_bits" "Bits"
builtin unaligned_pairwise_alignment 2 "unaligned_pairwise_alignment" "Alignment"
builtin flip_alignment 1 "flip_alignment" "Alignment"

branch_hmms (model,_) distances n_branches = listArray' $ map (model distances) [0..n_branches-1]
  
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

alignment_pr (AlignmentOnTree tree n_seqs ls as) hmms model | numNodes tree < 1  = error "Tree only has "++numNodes++" nodes."
                                                            | numNodes tree == 1 = alignment_pr1 (ls!0) model
                                                            | otherwise          = (alignment_pr_top as tree hmms)/(alignment_pr_bot as tree model)

alignment_pr' alignment hmms model var_a = if var_a then
                                               alignment_pr alignment hmms model
                                           else
                                               doubleToLogDouble 1.0

modifiable_alignment a@(AlignmentOnTree tree n_seqs ls as) | numNodes tree < 2 = a
modifiable_alignment   (AlignmentOnTree tree n_seqs ls as) | otherwise         = AlignmentOnTree tree n_seqs ls' as'
    where as' = mkArray (numElements as) (\i -> modifiable $ (as!i))
          ls' = listArray' [ seqlength as' tree node | node <- [0..numNodes tree - 1]]

-- Compare to unaligned_alignments_on_tree in parameters.cc
unaligned_alignments_on_tree t ls = [ make_a' b | b <- [0..2*numBranches t-1]]
    where make_a' b = let b' = reverseEdge t b in
                      if (b > b') then
                          flip_alignment $ make_a b'
                      else
                          make_a b
          make_a b = let n1 = sourceNode t b
                         n2 = targetNode t b
                         l1 = if n1 < numElements ls then ls!n1 else 0
                         l2 = if n2 < numElements ls then ls!n2 else 0
                     in unaligned_pairwise_alignment l1 l2

deepseq_array array obj = go (numElements array-1) array obj
    where go i array obj | i == -1   = obj
                         | otherwise = (array!i) `seq` (go (i-1) array obj)

-- Here we want to (i) force the tree, (ii) force the hmms, and (iii) match parameters.cc:unaligned_alignments_on_tree 
sample_alignment tree hmms tip_lengths = return (hmms `deepseq_array` (AlignmentOnTree tree n_nodes ls as))
    where n_leaves = numElements tip_lengths
          ls  = listArray' $ [ if node < n_leaves then tip_lengths!node else seqlength as tree node | node <- [0..n_nodes-1] ]
          as  = listArray' $ unaligned_alignments_on_tree tree tip_lengths
          n_nodes = numNodes tree

get_sequence_lengths leaf_seqs_array = mkArray (numElements leaf_seqs_array) (\node -> vector_size (leaf_seqs_array!node))

random_alignment tree hmms model tip_lengths var_a = Distribution (\a -> [alignment_pr' a hmms model var_a]) (no_quantile "random_alignment") (RandomStructure do_nothing modifiable_alignment do_sample) ()
    where do_sample = sample_alignment tree hmms tip_lengths

-- This function handles the case where we have only 1 sequence.
compute_sequence_lengths seqs tree as = [ if node < n_leaves then vector_size (seqs!node) else seqlength as tree node | node <- [0..numNodes tree-1] ]
    where n_leaves = numElements seqs
