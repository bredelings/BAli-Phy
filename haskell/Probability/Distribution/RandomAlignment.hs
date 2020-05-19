module Probability.Distribution.RandomAlignment where

import           Probability.Random
import           Tree
import           Bio.Alignment
import           Control.DeepSeq
import qualified Data.Map as Map

modifiable_alignment a@(AlignmentOnTree tree n_seqs ls as) | numNodes tree < 2 = a
modifiable_alignment (AlignmentOnTree tree n_seqs ls as) | otherwise           = AlignmentOnTree tree n_seqs ls' as'
  where
    as' = mkArray (numElements as) (\i -> modifiable $ (as ! i))
    ls' = listArray' [ seqlength as' tree node | node <- [0 .. numNodes tree - 1] ]

-- Compare to unaligned_alignments_on_tree in parameters.cc
unaligned_alignments_on_tree tree ls = [ make_a' b | b <- [0 .. 2 * numBranches tree - 1] ]
  where
    taxa = listArray' $ get_labels tree
    length_for_node node = if node < numElements taxa then ls Map.! (taxa ! node) else 0
    make_a' b = let b' = reverseEdge tree b in if (b > b') then flip_alignment $ make_a b' else make_a b
    make_a b =
        let l1 = length_for_node $ sourceNode tree b
            l2 = length_for_node $ targetNode tree b
        in  unaligned_pairwise_alignment l1 l2

-- Here we want to (i) force the tree, (ii) force the hmms, and (iii) match parameters.cc:unaligned_alignments_on_tree 
sample_alignment tree hmms tip_lengths = return (hmms `deepseq` (AlignmentOnTree tree n_nodes ls as))
  where
    n_leaves = numElements tip_lengths
    taxa     = listArray' $ get_labels tree
    ls       = listArray' $ [ if node < n_leaves then tip_lengths Map.! (taxa ! node) else seqlength as tree node | node <- [0 .. n_nodes - 1] ]
    as       = listArray' $ unaligned_alignments_on_tree tree tip_lengths
    n_nodes  = numNodes tree

product' = foldl' (*) (doubleToLogDouble 1.0)

builtin transition_counts 1 "transition_counts" "Alignment"
builtin pairwise_alignment_probability_from_counts 2 "pairwise_alignment_probability_from_counts" "Alignment"
alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a ! b)) (hmms ! b)

alignment_pr_top as tree hmms = product' $ map (alignment_branch_pr as hmms) [0 .. numBranches tree - 1]
alignment_pr_bot as tree (_, lengthp) = (product' $ map (lengthp . seqlength as tree) (internal_nodes tree)) ^ 2
alignment_pr1 length (_, lengthp) = lengthp length

alignment_pr (AlignmentOnTree tree n_seqs ls as) hmms model | numNodes tree < 1 = error "Tree only has " ++ numNodes ++ " nodes."
                                                            | numNodes tree == 1 = alignment_pr1 (ls ! 0) model
                                                            | otherwise = (alignment_pr_top as tree hmms) / (alignment_pr_bot as tree model)

alignment_pr' alignment hmms model var_a = if var_a then alignment_pr alignment hmms model else doubleToLogDouble 1.0

force_alignment a@(AlignmentOnTree tree n_seqs ls as) = force_ls `seq` force_as where
    force_as = force_struct as
    force_ls = force_struct ls

--FIXME: I should make this only trigger if you start looking at the VALUES of the pairwise alignments!
--FIXME: Maybe I should also reduce this to just a list of pairwise alignments?
triggered_modifiable_alignment value effect = (raw_a, triggered_a) where
    raw_a       = modifiable_alignment value
    effect'     = force_alignment raw_a `seq` effect
    triggered_a = effect' `seq` raw_a

random_alignment tree hmms model tip_lengths var_a = Distribution (\a -> [alignment_pr' a hmms model var_a])
                                                                  (no_quantile "random_alignment")
                                                                  (RandomStructure do_nothing triggered_modifiable_alignment do_sample)
                                                                  ()
    where do_sample = sample_alignment tree hmms tip_lengths

