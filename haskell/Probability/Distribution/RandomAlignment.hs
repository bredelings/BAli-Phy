module Probability.Distribution.RandomAlignment where

import           Probability.Random
import           Tree
import           Bio.Alignment
import           Control.DeepSeq
import           Data.Array

import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text

type IModel = (IntMap Double -> Int -> PairHMM, Int -> LogDouble)

modifiable_alignment a@(AlignmentOnTree tree n_seqs ls as) | numNodes tree < 2 = a
modifiable_alignment (AlignmentOnTree tree n_seqs ls as) | otherwise           = AlignmentOnTree tree n_seqs ls' as'
  where
    as' = fmap modifiable as
    ls' = getNodesSet tree & IntMap.fromSet (seqlength as' tree)

-- Compare to unaligned_alignments_on_tree in parameters.cc
unaligned_alignments_on_tree tree ls = getEdgesSet tree & IntMap.fromSet make_a'
  where
    length_for_node node = case get_label tree node of
                             Just label -> ls Map.! label
                             Nothing    -> 0

    make_a' b | b > b2     = flip_alignment $ make_a b2
              | otherwise  = make_a b
              where b2 = reverseEdge tree b

    make_a b = unaligned_pairwise_alignment l1 l2
        where l1 = length_for_node $ sourceNode tree b
              l2 = length_for_node $ targetNode tree b

median xs = ys !! n where
    ys = sort xs
    n = length xs `div` 2

-- Compare to unaligned_alignments_on_tree in parameters.cc
left_aligned_alignments_on_tree tree ls = getEdgesSet tree & IntMap.fromSet make_a'
  where
    internal_sequence_length = median $ Map.elems $ ls

    length_for_node node = case get_label tree node of
                             Just label -> ls Map.! label
                             Nothing    -> internal_sequence_length

    make_a' b | b > b2     = flip_alignment $ make_a b2
              | otherwise  = make_a b
              where b2 = reverseEdge tree b

    make_a branch = left_aligned_pairwise_alignment l1 l2
        where l1 = length_for_node $ sourceNode tree branch
              l2 = length_for_node $ targetNode tree branch


-- Here we want to (i) force the tree, (ii) force the hmms, and (iii) match parameters.cc:unaligned_alignments_on_tree 
sample_alignment tree hmms tip_lengths = return (hmms `deepseq` (AlignmentOnTree tree n_nodes ls as))
  where
    n_leaves = Map.size tip_lengths
    ls       = getNodesSet tree & IntMap.fromSet (\node -> case get_label tree node of
                                                             Just label -> tip_lengths Map.! label
                                                             Nothing -> seqlength as tree node )
    as       = unaligned_alignments_on_tree tree tip_lengths
    n_nodes  = numNodes tree

alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a IntMap.! b)) (hmms IntMap.! b)

alignment_pr_top as tree hmms = product [ alignment_branch_pr as hmms b | b <- getUEdges tree]
alignment_pr_bot as tree (_, lengthp) = (product $ map (lengthp . seqlength as tree) (internal_nodes tree)) ^ 2
alignment_pr1 length (_, lengthp) = lengthp length

-- FIXME: Maybe the alignment argument should be last?
-- QUESTION: Should I be passing the tree in here separately?
alignment_pr (AlignmentOnTree tree n_seqs ls as) hmms model | numNodes tree < 1 = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                            | numNodes tree == 1 = alignment_pr1 (ls IntMap.! 0) model
                                                            | otherwise = (alignment_pr_top as tree hmms) / (alignment_pr_bot as tree model)

alignment_prs_bot as tree (_, lengthp) = map ((\x -> x*x) . (1/) . lengthp . seqlength as tree) (internal_nodes tree)
alignment_prs_top as tree hmms = map (alignment_branch_pr as hmms) (getUEdges tree)
alignment_prs hmms model (AlignmentOnTree tree n_seqs ls as) | numNodes tree < 1  = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                             | numNodes tree == 1 = [alignment_pr1 (ls IntMap.! 0) model]
                                                             | otherwise = alignment_prs_top as tree hmms ++ alignment_prs_bot as tree model -- [ doubleToLogDouble 1.0 / alignment_pr_bot as tree model]

--FIXME: I should make this only trigger if you start looking at the VALUES of the pairwise alignments!
--FIXME: Maybe I should also reduce this to just a list of pairwise alignments?
triggered_modifiable_alignment value effect = triggered_a where
    raw_a       = modifiable_alignment value
    effect'     = unsafePerformIO $ effect raw_a
    triggered_a = withEffect effect' raw_a


data RandomAlignmentProperties = RandomAlignmentProperties 
    {
      probability :: LogDouble,
      hmms :: IntMap PairHMM,
      lengthp :: Int -> LogDouble,
      as :: IntMap PairwiseAlignment,
      get_lengths :: IntMap Int,
      get_length_prs :: IntMap LogDouble
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
  property "properties" (RandomAlignmentProperties pr hmms lengthp as ls length_prs)
  return $ prs



data RandomAlignment t = (HasLabels t, Tree t) => RandomAlignment (BranchLengthTreeImp t) IModel (Map.Map Text Int) (IntMap PairHMM)

instance Dist (RandomAlignment t) where
    type Result (RandomAlignment t) = AlignmentOnTree (BranchLengthTreeImp t)

instance Sampleable (RandomAlignment t) where
    sample dist@(RandomAlignment tree model tip_lengths hmms) = RanDistribution3 dist do_nothing triggered_modifiable_alignment (sample_alignment tree hmms tip_lengths)

instance HasAnnotatedPdf (RandomAlignment t) where
    annotated_densities dist@(RandomAlignment tree model tip_lengths hmms) = annotated_alignment_prs tree hmms model

random_alignment tree model tip_lengths = RandomAlignment tree model tip_lengths (branch_hmms model tree)


