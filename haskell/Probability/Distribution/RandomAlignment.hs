module Probability.Distribution.RandomAlignment where

import           Probability.Random
import           Tree
import           Bio.Alignment
import           Control.DeepSeq

import qualified Data.Map as Map

type IModel = (Array Int Double -> Int -> PairHMM, Int -> LogDouble)

modifiable_alignment a@(AlignmentOnTree tree n_seqs ls as) | numNodes tree < 2 = a
modifiable_alignment (AlignmentOnTree tree n_seqs ls as) | otherwise           = AlignmentOnTree tree n_seqs ls' as'
  where
    as' = fmap modifiable as
    ls' = listArray' [ seqlength as' tree node | node <- [0 .. numNodes tree - 1] ]

-- Compare to unaligned_alignments_on_tree in parameters.cc
unaligned_alignments_on_tree tree ls = [ make_a' b | b <- [0 .. 2 * numBranches tree - 1] ]
  where
    taxa = listArray' $ get_labels tree
    length_for_node node = if node < length taxa then ls Map.! (taxa ! node) else 0
    make_a' b = let b' = reverseEdge tree b in if (b > b') then flip_alignment $ make_a b' else make_a b
    make_a b =
        let l1 = length_for_node $ sourceNode tree b
            l2 = length_for_node $ targetNode tree b
        in  unaligned_pairwise_alignment l1 l2

median xs = ys !! n where
    ys = sort xs
    n = length xs `div` 2

-- Compare to unaligned_alignments_on_tree in parameters.cc
left_aligned_alignments_on_tree tree ls = [ make_A b | b <- [0 .. 2 * numBranches tree - 1] ]
  where
    taxa = listArray' $ get_labels tree
    internal_sequence_length = median $ Map.elems $ ls
    -- FIXME: can we assume that the leaf nodes are [0..n_leaves-1]?
    -- If we check leaf nodes instead, then what if the root node was an unnamed leaf?
    length_for_node node = if node < length taxa then ls Map.! (taxa ! node) else internal_sequence_length
    make_A b = let b2 = reverseEdge tree b in
               if (b > b2) then
                   flip_alignment $ make_A' b2
               else
                   make_A' b
    make_A' branch =
        let l1 = length_for_node $ sourceNode tree branch
            l2 = length_for_node $ targetNode tree branch
        in  left_aligned_pairwise_alignment l1 l2


-- Here we want to (i) force the tree, (ii) force the hmms, and (iii) match parameters.cc:unaligned_alignments_on_tree 
sample_alignment tree hmms tip_lengths = return (hmms `deepseq` (AlignmentOnTree tree n_nodes ls as))
  where
    n_leaves = Map.size tip_lengths
    taxa     = listArray' $ get_labels tree
    ls       = listArray' $ [ if node < n_leaves then tip_lengths Map.! (taxa ! node) else seqlength as tree node | node <- [0 .. n_nodes - 1] ]
    as       = listArray' $ unaligned_alignments_on_tree tree tip_lengths
    n_nodes  = numNodes tree

alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a ! b)) (hmms ! b)

alignment_pr_top as tree hmms = product $ map (alignment_branch_pr as hmms) [0 .. numBranches tree - 1]
alignment_pr_bot as tree (_, lengthp) = (product $ map (lengthp . seqlength as tree) (internal_nodes tree)) ^ 2
alignment_pr1 length (_, lengthp) = lengthp length

-- FIXME: Maybe the alignment argument should be last?
-- QUESTION: Should I be passing the tree in here separately?
alignment_pr (AlignmentOnTree tree n_seqs ls as) hmms model | numNodes tree < 1 = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                            | numNodes tree == 1 = alignment_pr1 (ls ! 0) model
                                                            | otherwise = (alignment_pr_top as tree hmms) / (alignment_pr_bot as tree model)

alignment_prs_bot as tree (_, lengthp) = map ((\x -> x*x) . (1/) . lengthp . seqlength as tree) (internal_nodes tree)
alignment_prs_top as tree hmms = map (alignment_branch_pr as hmms) [0 .. numBranches tree - 1]
alignment_prs hmms model (AlignmentOnTree tree n_seqs ls as) | numNodes tree < 1  = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                             | numNodes tree == 1 = [alignment_pr1 (ls ! 0) model]
                                                             | otherwise = alignment_prs_top as tree hmms ++ alignment_prs_bot as tree model -- [ doubleToLogDouble 1.0 / alignment_pr_bot as tree model]

force_alignment a@(AlignmentOnTree tree n_seqs ls as) = force_ls `seq` force_as where
    force_as = force_struct as
    force_ls = force_struct ls

--FIXME: I should make this only trigger if you start looking at the VALUES of the pairwise alignments!
--FIXME: Maybe I should also reduce this to just a list of pairwise alignments?
triggered_modifiable_alignment value effect = (raw_a, triggered_a) where
    raw_a       = modifiable_alignment value
    effect'     = force_alignment raw_a `seq` effect raw_a
    triggered_a = effect' `seq` raw_a


data RandomAlignmentProperties = RandomAlignmentProperties 
    {
      probability :: LogDouble,
      hmms :: Array Int PairHMM,
      lengthp :: Int -> LogDouble,
      as :: Array Int PairwiseAlignment,
      get_lengths :: Array Int Int,
      get_length_prs :: Array Int LogDouble
    }

annotated_alignment_prs tree hmms model alignment = do
  in_edge "tree" tree
  in_edge "imodel" model
  let prs = alignment_prs hmms model alignment
      pr = product prs
      as = pairwise_alignments alignment
      ls = sequence_lengths alignment
      lengthp = snd model
      length_prs = fmap lengthp ls
  property "lengthp" lengthp
  property "hmms" hmms
  property "pr" pr
  property "properties" (RandomAlignmentProperties pr hmms lengthp as ls length_prs)
  return $ prs


class HasRandomAlignment d where
    random_alignment:: (LabelledTree t, Tree t) => BranchLengthTreeImp t -> IModel -> Map.Map String Int -> d (AlignmentOnTree (BranchLengthTreeImp t))


instance HasRandomAlignment Distribution where
    random_alignment tree model tip_lengths = Distribution "random_alignment"
                                                 (annotated_alignment_prs tree hmms model)
                                                 (no_quantile "random_alignment")
                                                 (RandomStructure do_nothing triggered_modifiable_alignment (sample_alignment tree hmms tip_lengths))
                                                 NoRange
        where hmms = branch_hmms model (branch_lengths tree) (numBranches tree)

instance HasRandomAlignment Random where
    random_alignment tree model tip_lengths = RanDistribution $ random_alignment tree model tip_lengths
