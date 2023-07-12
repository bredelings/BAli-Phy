module Bio.Alignment (module Bio.Alignment,
                      module Bio.Sequence,
                      module Bio.Alignment.Matrix,
                      module Bio.Alignment.Pairwise) where

import Tree
import Data.BitVector
import Data.Foldable
import Data.Array
import Parameters
import Foreign.Vector
import Foreign.Pair
import Bio.Sequence
import Bio.Alphabet -- for Alphabet type
import Bio.Alignment.Matrix
import Bio.Alignment.Pairwise

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified Data.Text as Text (unpack)

data VectorPairIntInt -- ancestral sequences with (int letter, int category) for each site.

foreign import bpcall "Alignment:leaf_sequence_counts" builtin_leaf_sequence_counts :: AlignmentMatrix -> Int -> EVector Int -> EVector (EVector Int)

branch_hmms (model,_) tree = getUEdgesSet tree & IntMap.fromSet (model $ branch_lengths tree)
  
seqlength as tree node = pairwise_alignment_length1 (as IntMap.! b) where
    b = edgesOutOfNode tree node!0

{-
pairwise_alignments_from_matrix a tree = [ pairwise_alignment_from_bits bits1 bits2 | b <- [0..2*numBranches tree-1],
                                                                                           let bits1 = bits ! sourceNode tree b,
                                                                                           let bits2 = bits ! targetNode tree b]
    where bits = minimally_connect_characters a tree
-}

-- We can't just do forall t.AlignmentOnTree t, because then any constraints on t will be on existential variables, resulting in ambiguity.
data AlignmentOnTree t = AlignmentOnTree t Int (IntMap Int) (IntMap PairwiseAlignment)
n_sequences         (AlignmentOnTree _ n _  _) = n
sequence_lengths    (AlignmentOnTree _ _ ls _) = ls
pairwise_alignments (AlignmentOnTree _ _ _ as) = as
sequenceLength a node = sequence_lengths a IntMap.! node

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

alignment_on_tree_length (AlignmentOnTree t _ ls as) = (ls IntMap.! node0) + sum [numInsert (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                                       where node0 = head $ getNodes t

totalNumIndels (AlignmentOnTree t _ ls as) = sum [numIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

totalLengthIndels (AlignmentOnTree t _ ls as) = sum [lengthIndels (as IntMap.! b) | b <- allEdgesFromNode t node0]
                                         where node0 = head $ getNodes t

foreign import bpcall "Alignment:uncompress_alignment" builtin_uncompress_alignment :: AlignmentMatrix -> EVector Int -> AlignmentMatrix

uncompress_alignment (a, counts, mapping) = builtin_uncompress_alignment a mapping

-- Alignment -> Int -> EVector Int -> [EVector Int]
leaf_sequence_counts a n counts = list_from_vector $ builtin_leaf_sequence_counts a n counts

foreign import bpcall "Alignment:ancestral_sequence_alignment" builtin_ancestral_sequence_alignment :: AlignmentMatrix -> EVector VectorPairIntInt -> EVector Int -> AlignmentMatrix
ancestral_sequence_alignment tree a0 states smap = builtin_ancestral_sequence_alignment a0 states' smap
    where states' = list_to_vector [ states IntMap.! node  | node <- sort $ getNodes tree]

foreign import bpcall "Alignment:" get_sequence_from_states :: VectorPairIntInt -> EVector Int


{-
  OK, so what do we use the original alignment matrix for?
  * the alphabet
  * the original sequences.

  Right now, I think the node names still correspond to the input alignment names.
  But when the node names stop being [0..n_sequences-1], we will need a way to
  1. construct the alignment from a list of Sequence objects
  2. get a list (leaf_nodes ++ internal_nodes)
  3. for each node, get the leaf label
  4. use smap to project the state to an alphabet letter.
  5. turn the list of Sequence objects into an alignment.
Maybe we want a map from String -> EVector Int.

That would lose the comments on the fastas though.
-}

foreign import bpcall "Alignment:select_alignment_columns" builtin_select_alignment_columns :: AlignmentMatrix -> EVector Int -> AlignmentMatrix
select_alignment_columns alignment sites = builtin_select_alignment_columns alignment (list_to_vector sites)

foreign import bpcall "Alignment:select_alignment_pairs" builtin_select_alignment_pairs :: AlignmentMatrix -> EVector (EPair Int Int) -> Alphabet -> AlignmentMatrix
select_alignment_pairs alignment sites doublets = builtin_select_alignment_pairs alignment sites' doublets
    where sites' = list_to_vector $ map (\(x,y) -> c_pair x y) sites

alignmentOnTreeFromSequences tree sequences alphabet = AlignmentOnTree tree numSequences lengths pairwiseAs
    where -- observedSequences :: IntMap (Maybe (EVector Int))
          observedSequences = fmap (fmap $ sequenceToAlignedIndices alphabet) $ getSequencesOnTree sequences tree
          allSequences = minimally_connect_characters' observedSequences tree
          numSequences = length sequences
          lengths = getNodesSet tree & IntMap.fromSet (\node -> vector_size $ strip_gaps $ allSequences IntMap.! node)
          bits = fmap bitmask_from_sequence' allSequences
          alignmentForBranch b = pairwise_alignment_from_bits (bits IntMap.! source) (bits IntMap.! target)
                                 where source = sourceNode tree b
                                       target = targetNode tree b
          pairwiseAs = getEdgesSet tree & IntMap.fromSet alignmentForBranch


find_sequence label sequences = find (\s -> sequenceName s == label) sequences

getSequencesOnTree :: HasLabels t => [Sequence] -> t -> IntMap (Maybe Sequence)
getSequencesOnTree sequence_data tree = getNodesSet tree & IntMap.fromSet sequence_for_node where
    sequence_for_node node = case get_label tree node of
                               Nothing ->  Nothing
                               Just label ->
                                   case find_sequence label sequence_data of
                                     Just sequence -> Just sequence
                                     Nothing -> error $ "No such sequence " ++ Text.unpack label

