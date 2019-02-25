module Alignment where

import Tree
import Data.BitVector

builtin pairwise_alignment_length1 1 "pairwise_alignment_length1" "Alignment"
builtin pairwise_alignment_length2 1 "pairwise_alignment_length2" "Alignment"
builtin transition_counts 1 "transition_counts" "Alignment"
builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts" "Alignment"

builtin builtin_load_alignment 2 "load_alignment" "Alignment"
builtin builtin_sequences_from_alignment 1 "sequences_from_alignment" "Alignment"

builtin builtin_alignment_row_to_presence_bitvector 2 "alignment_row_to_presence_bitvector" "Bits"
  
branch_hmms (model,_) distances n_branches = listArray' $ map (model distances) [0..2*n_branches-1]
  
alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a!b)) (hmms!b)
  
seqlength a tree node = pairwise_alignment_length1 (a!b) where
    b = head $ edgesOutOfNode tree node

product' = foldl' (*) (doubleToLogDouble 1.0)
alignment_pr_top a tree hmm = product' $ map (alignment_branch_pr a hmm) [0..numBranches tree - 1]
alignment_pr_bot a tree (_,lengthp) = (product' $ map (lengthp . seqlength a tree) (internal_nodes tree))^2
alignment_pr a tree hmm model = (alignment_pr_top a tree hmm)/(alignment_pr_bot a tree model)
alignment_pr1 seq (_,lengthp) = lengthp (sizeOfVectorInt seq)
load_alignment alphabet filename = builtin_load_alignment alphabet (listToString filename)
-- sequence_from_alignment :: AlignmentMatrix -> [ Vector<int> ]
sequences_from_alignment a = list_from_vector $ builtin_sequences_from_alignment a

alignment_row_to_presence_bitvector a row = BitVector $ builtin_alignment_row_to_presence_bitvector a row

presence_bitvectors_from_alignment a t = mkArray (numNodes t) node_to_bits where
    pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]
    node_to_bits n = if is_leaf_node t n then
                         alignment_row_to_presence_bitvector a n
                     else
                         foldl1 (.|.) $ pairs $ map (character_behind_branch!) $ edgesTowardNode t n
    character_behind_branch b = case edgesBeforeEdge t b of [] -> alignment_row_to_presence_bitvector a (sourceNode t b)
                                                            [b1,b2] -> (character_behind_branch_array!b1) .|. (character_behind_branch_array!b2)
    character_behind_branch_array = mkArray (2*numBranches t) character_behind_branch


