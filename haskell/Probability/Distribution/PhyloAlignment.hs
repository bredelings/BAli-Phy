module Probability.Distribution.PhyloAlignment where

import           Probability.Random
import           Tree
import           Bio.Alignment
import           Control.DeepSeq
import           Data.Array
import           Data.Maybe (mapMaybes)
import           MCMC
import           Effect (getProperties)

import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text

type IModel = (IntMap Double -> Int -> PairHMM, Int -> LogDouble)


-- If we change the number of tip sequences, then we need to update the modifiable node sequence lengths {ls'}.

modifiableAlignment a@(AlignmentOnTree tree n_seqs ls as) | numNodes tree < 2 = a
modifiableAlignment (AlignmentOnTree tree n_seqs ls as) | otherwise           = AlignmentOnTree tree n_seqs ls' as'
  where
    as' = fmap modifiable as
    ls' = modifiable ls

-- Compare to unaligned_alignments_on_tree in parameters.cc
unaligned_alignments_on_tree tree ls = getEdgesSet tree & IntMap.fromSet make_a'
  where
    length_for_node node = case getLabel tree node of
                             Just label -> ls Map.! label
                             Nothing    -> 0

    make_a' b | b > b2     = flip_alignment $ make_a b2
              | otherwise  = make_a b
              where b2 = reverseEdge b

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

    length_for_node node = case getLabel tree node of
                             Just label -> ls Map.! label
                             Nothing    -> internal_sequence_length

    make_a' b | b > b2     = flip_alignment $ make_a b2
              | otherwise  = make_a b
              where b2 = reverseEdge b

    make_a branch = left_aligned_pairwise_alignment l1 l2
        where l1 = length_for_node $ sourceNode tree branch
              l2 = length_for_node $ targetNode tree branch

{-
  OK, so here we are only storing node->lengths for tip nodes where the length is fixed.
  The length for other nodes is computed by looking up the length of a relevant pairwise alignment.  
-}

-- Here we want to (i) force the tree, (ii) force the hmms, and (iii) match parameters.cc:unaligned_alignments_on_tree 
sample_alignment tree hmms tip_lengths = return (hmms `deepseq` (AlignmentOnTree tree n_nodes ls as))
  where
    n_leaves = Map.size tip_lengths
    ls       = IntMap.fromList $ mapMaybes maybeLength (getNodes tree)
    as       = left_aligned_alignments_on_tree tree tip_lengths
    n_nodes  = numNodes tree
    maybeLength node = case getLabel tree node of Just label -> Just (node, tip_lengths Map.! label)
                                                  _          -> Nothing

alignment_branch_pr a hmms b = pairwise_alignment_probability_from_counts (transition_counts (a IntMap.! b)) (hmms IntMap.! b)

alignment_pr_top as tree hmms = product [ alignment_branch_pr as hmms b | b <- getUEdges tree]
alignment_pr_bot as tree (_, lengthp) = (product $ map (lengthp . seqlength as tree) (internalNodes tree)) ^ 2
alignment_pr1 length (_, lengthp) = lengthp length

-- FIXME: Maybe the alignment argument should be last?
-- QUESTION: Should I be passing the tree in here separately?
alignment_pr (AlignmentOnTree tree n_seqs ls as) hmms model | numNodes tree < 1 = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                            | numNodes tree == 1 = alignment_pr1 (ls IntMap.! 0) model
                                                            | otherwise = (alignment_pr_top as tree hmms) / (alignment_pr_bot as tree model)

alignment_prs_bot as tree (_, lengthp) = [ pow pr (1-n) | node <- internalNodes tree,
                                                          let pr = lengthp $ seqlength as tree node,
                                                          let n = fromIntegral $ nodeDegree tree node
                                         ]
alignment_prs_top as tree hmms = map (alignment_branch_pr as hmms) (getUEdges tree)
alignment_prs hmms model (AlignmentOnTree tree n_seqs ls as) | numNodes tree < 1  = error $ "Tree only has " ++ show (numNodes tree) ++ " nodes."
                                                             | numNodes tree == 1 = [alignment_pr1 (ls IntMap.! 0) model]
                                                             | otherwise = alignment_prs_top as tree hmms ++ alignment_prs_bot as tree model -- [ doubleToLogDouble 1.0 / alignment_pr_bot as tree model]

--FIXME: I should make this only trigger if you start looking at the VALUES of the pairwise alignments!
--FIXME: Maybe I should also reduce this to just a list of pairwise alignments?
triggeredModifiableAlignment value effect = triggered_a where
    raw_a       = modifiableAlignment value
    effect'     = unsafePerformIO $ effect raw_a
    triggered_a = withEffect effect' raw_a


data PhyloAlignmentProperties = PhyloAlignmentProperties 
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
      as = pairwiseAlignments alignment
      ls = mkSequenceLengthsMap alignment
      lengthp = snd model
      length_prs = fmap lengthp ls
      props = PhyloAlignmentProperties pr hmms lengthp as ls length_prs
  return $ (prs, props)

alignment_effect (AlignmentOnTree tree n ls as) = do
  SamplingRate 1 $ add_move $ walkTreeSampleAlignments tree as
  SamplingRate 0.1 $ add_move $ realignFromTips tree as

data PhyloAlignment t = (IsTree t, Eq (LabelType t)) => PhyloAlignment t IModel (Map.Map (LabelType t) Int) (IntMap PairHMM)

instance Dist (PhyloAlignment t) where
    type Result (PhyloAlignment t) = AlignmentOnTree t
    dist_name _ = "PhyloAlignment"

instance Sampleable (PhyloAlignment t) where
    sample dist@(PhyloAlignment tree model tip_lengths hmms) = RanDistribution3 dist alignment_effect triggeredModifiableAlignment (sample_alignment tree hmms tip_lengths)

instance HasAnnotatedPdf (PhyloAlignment t) where
    type DistProperties (PhyloAlignment t) = PhyloAlignmentProperties
    annotated_densities dist@(PhyloAlignment tree model tip_lengths hmms) a = annotated_alignment_prs tree hmms model a

instance SampleableWithProps (PhyloAlignment t) where
    sampleWithProps dist = do
      x <- sample dist
      let props = getProperties' x dist
          getProperties' :: a -> d -> DistProperties d
          getProperties' x _ = getProperties x
      return (x, props)

phyloAlignment tree model scale tip_lengths = PhyloAlignment tree model tip_lengths (branch_hmms model tree scale)
