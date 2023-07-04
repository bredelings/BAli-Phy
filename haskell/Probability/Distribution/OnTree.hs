module Probability.Distribution.OnTree where

import Probability.Random
import BAliPhy.ATModel
import Tree
import SModel
import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Array
import Data.Matrix
import Data.Foldable
import Foreign.Maybe
import Data.Text (Text(..))
import qualified Data.Text as Text

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Foreign.IntMap (EIntMap)
import qualified Foreign.IntMap as FIM

import Data.Maybe (fromJust)
-- FIXME: need polymorphism.
--        This needs to be after weighted_frequency_matrix.
--        Because we have no polymorphism, wfm needs to be defined after MixtureModel and MixtureModels.

data CTMCOnTreeProperties = CTMCOnTreeProperties {
      prop_subst_root :: Int,
      prop_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_cond_likes :: IntMap CondLikes,
      prop_anc_seqs :: Text,
      prop_likelihood :: LogDouble,
      prop_taxa :: IntMap (CMaybe CPPString),
      prop_get_weighted_frequency_matrix :: Matrix Double,
      prop_smap :: EVector Int,
      prop_leaf_sequences :: IntMap (EVector Int),
      prop_alphabet :: Alphabet,
      prop_as :: IntMap PairwiseAlignment,
      prop_n_states :: Int,
      prop_n_base_models :: Int,
      prop_n_muts :: Int
    }

data CTMCOnTreeFixedAProperties = CTMCOnTreeFixedAProperties {
      prop_fa_subst_root :: Int,
      prop_fa_transition_ps :: IntMap (EVector (Matrix Double)),
      prop_fa_cond_likes :: IntMap CondLikes,
      prop_fa_anc_seqs :: Text,
      prop_fa_likelihood :: LogDouble,
      prop_fa_taxa :: IntMap (CMaybe CPPString),
      prop_fa_get_weighted_frequency_matrix :: Matrix Double,
      prop_fa_smap :: EVector Int,
      prop_fa_leaf_sequences :: IntMap (EVector Int),
      prop_fa_alphabet :: Alphabet,
      prop_fa_n_states :: Int,
      prop_fa_n_base_models :: Int,
      prop_fa_n_muts :: Int
    }

transition_ps_map smodel_on_tree = IntMap.fromSet (list_to_vector . branch_transition_p smodel_on_tree) edges where
    edges = getEdgesSet $ get_tree' smodel_on_tree

-- Ideally we'd like to do
--    type NodeAlignment = EPair Int BranchAlignments
-- but this creates recursive type synonyms, which are not allowed.

-- We hack around this by removing the definition of NodeAlignment.
-- Then NodeAlignment needs a foreign import to construct it.

data NodeAlignment -- NodeAlignment SourceNode Int (EVector BranchAlignment)
foreign import bpcall "Alignment:" mkNodeAlignment :: Int -> Int -> EVector BranchAlignment -> NodeAlignment
data BranchAlignment -- BranchAlignment TargetNode PairwiseAlignment (EVector BranchAlignment)
foreign import bpcall "Alignment:" mkBranchAlignment :: Int -> PairwiseAlignment -> EVector BranchAlignment -> BranchAlignment
foreign import bpcall "Alignment:" constructPositionSequencesRaw :: NodeAlignment -> EIntMap (EVector Int)
constructPositionSequences a = FIM.importIntMap $ constructPositionSequencesRaw $ exportAlignmentOnTree a
foreign import bpcall "Alignment:" substituteLetters :: EVector Int -> EVector Int -> EVector Int

exportAlignmentOnTree :: Tree t => AlignmentOnTree t -> NodeAlignment
exportAlignmentOnTree (AlignmentOnTree tree _ ls as) = mkNodeAlignment root (ls IntMap.! root) (branchAlignments $ edgesOutOfNode tree root)
    where root = head $ getNodes tree
          branchAlignments edges = list_to_vector [ mkBranchAlignment (targetNode tree e) (as IntMap.! e) (branchAlignments $ edgesAfterEdge tree e) | e <- toList $ edges]

annotated_subst_like_on_tree tree alignment smodel sequences = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      taxa = fmap (cMaybe . fmap (\(Text s) -> s)) $ get_labels tree
      node_sequences = fmap (sequence_to_indices alphabet) $ fmap fromJust $ getSequencesOnTree sequences tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods
              tree
              node_sequences
              as
              alphabet
              transition_ps
              f
              smap
      -- Possibly we should check that the sequence lengths match the alignment..
      -- but instead we just ensure that the alignment is evaluated.
      likelihood | n_nodes > 2    = peel_likelihood tree cls as (weighted_frequency_matrix smodel) subst_root
                 | n_nodes == 1   = let [n1] = getNodes tree
                                    in alignment `seq` peel_likelihood_1 (node_sequences IntMap.! n1) alphabet f
                 | n_nodes == 2   = let [n1, n2] = getNodes tree
                                        [b1, b2] = getEdges tree
                                    in peel_likelihood_2 (node_sequences IntMap.! n1) (node_sequences IntMap.! n2) alphabet (as IntMap.! b1) (transition_ps IntMap.! b1) f
                 | otherwise      = error $ "likelihood: n_nodes = " ++ show n_nodes

      ancestral_states = sample_ancestral_sequences tree subst_root node_sequences as alphabet transition_ps f cls smap

      ancestral_sequences = case n_nodes of 1 -> node_sequences
                                            2 -> node_sequences
                                            _ -> fmap get_sequence_from_states ancestral_states

      fasta = let positionSequences = constructPositionSequences alignment
                  letterSequences = getNodesSet tree & IntMap.fromSet letterSequenceForNode
                  letterSequenceForNode n = substituteLetters (ancestral_sequences IntMap.! n) (positionSequences IntMap.! n)
              in fastaTree tree (fmap (sequenceToText alphabet) letterSequences)

      n_muts = parsimony tree node_sequences as alphabet (unitCostMatrix alphabet)

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  property "properties" (CTMCOnTreeProperties subst_root transition_ps cls fasta likelihood taxa f smap node_sequences alphabet as (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return [likelihood]

data CTMCOnTree t s = CTMCOnTree t (AlignmentOnTree t) s


instance Dist (CTMCOnTree t s) where
    type Result (CTMCOnTree t s) = [Sequence]
    dist_name _ = "ctmc_on_tree"

instance (LabelledTree t,BranchLengthTree t, SimpleSModel s) => HasAnnotatedPdf (CTMCOnTree t s) where
    annotated_densities (CTMCOnTree tree alignment smodel) = annotated_subst_like_on_tree tree alignment smodel

ctmc_on_tree tree alignment smodel = CTMCOnTree tree alignment smodel

----------------------------------------
getCompressedSequencesOnTree compressed_sequences tree = getNodesSet tree & IntMap.fromSet sequence_for_node
    where sequence_for_node node = case get_label tree node of
                                     Nothing ->  error "No label"
                                     Just label ->
                                         case lookup label compressed_sequences of
                                           Just indices -> indices
                                           Nothing -> error $ "No such sequence " ++ Text.unpack label

fastaSeq label seq = Text.concat [Text.singleton '>', label, Text.singleton '\n', seq, Text.singleton '\n']

fastaTree tree sequences =  Text.concat [fastaSeq label sequence | n <- orderedNodes,
                                                                   let label = add_ancestral_label n (get_labels tree),
                                                                   let sequence = sequences IntMap.! n]
    where orderedNodes = leaf_nodes tree ++ internal_nodes tree

{-
ok, so how do we pass IntMaps to C++ functions?
well, we could turn each IntMap into an EIntMap
for alignments, we could also use an ordering of the sequences to ensure that the leaves are written first.
   -}
annotated_subst_likelihood_fixed_A tree smodel sequences = do
  let subst_root = modifiable (head $ internal_nodes tree ++ leaf_nodes tree)

  let a0 = alignment_from_sequences alphabet sequences
      (compressed_alignment,column_counts,mapping) = compress_alignment $ a0
      compressed_sequences = [ (name, (strip_gaps seq, bitmask_from_sequence seq))
                                   | (name,seq) <- isequences_from_alignment compressed_alignment]
      node_seqs_bits = getCompressedSequencesOnTree compressed_sequences tree
      node_sequences = fmap fst node_seqs_bits
      -- stop going through Alignment

      node_sequences0 :: IntMap (Maybe (EVector Int))
      node_sequences0 = fmap (fmap $ sequenceToAlignedIndices alphabet) $ getSequencesOnTree sequences tree

      -- (compressed_node_sequences, column_counts', mapping') = compress_sequences node_sequence0
      -- OK, so how are we going to do this?
      -- * turn the sequences into an EIntMap (EVector Int)
      -- * run the pattern compression on the EIntMap (EVector Int)
      -- * return the compressed EIntMap (EVector Int), an EVector Int for the orig->compress mapping, and an EVector Int for the column counts).
      -- * convert the EIntMap (EVector Int) back to an IntMap EVector Int for the compressed sequences.

      -- OK, so now we need to get the ancestral sequences as an IntMap (EVector Int)
      -- So... actually it looks like we already HAVE a minimally_connect_leaf_characters function!!!

      n_nodes = numNodes tree
      taxa = fmap (cMaybe . fmap (\(Text s) -> s)) $ get_labels tree
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_ps_map smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods_SEV
              tree
              node_seqs_bits
              alphabet
              transition_ps
              smap
      likelihood | n_nodes > 2    = peel_likelihood_SEV tree cls f subst_root column_counts
                 | n_nodes == 1   = peel_likelihood_1_SEV compressed_alignment alphabet f column_counts
                 | n_nodes == 2   = let [n1,n2] = getNodes tree
                                        [b1,b2] = getEdges tree
                                    in peel_likelihood_2_SEV compressed_alignment alphabet (transition_ps IntMap.! b1) f column_counts

--    This also needs the map from columns to compressed columns:
      ancestral_sequences = case n_nodes of
                              1 -> Text.concat [fastaSeq (sequence_name s) (sequenceData s) | s <- sequences]
                              2 -> Text.concat [fastaSeq (sequence_name s) (sequenceData s) | s <- sequences]
                              _ -> let ancestral_states :: IntMap VectorPairIntInt
                                       ancestral_states = sample_ancestral_sequences_SEV
                                         tree
                                         subst_root
                                         node_sequences
                                         alphabet
                                         transition_ps
                                         f
                                         cls
                                         smap
                                         mapping
                                       ancestral_sequences :: IntMap (EVector Int)
                                       ancestral_sequences = fmap get_sequence_from_states ancestral_states
                                       ancestral_sequences' = minimally_connect_characters
                                                                        node_sequences0
                                                                        tree
                                                                        ancestral_sequences
                                       ancestral_sequences'' = fmap (sequenceToText alphabet) ancestral_sequences'
                                   in fastaTree tree ancestral_sequences''

      n_muts = parsimony_fixed_A tree node_seqs_bits alphabet (unitCostMatrix alphabet) column_counts

  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  property "properties" (CTMCOnTreeFixedAProperties subst_root transition_ps cls ancestral_sequences likelihood taxa f smap node_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) n_muts)

  return [likelihood]

data CTMCOnTreeFixedA t s = CTMCOnTreeFixedA t s

instance Dist (CTMCOnTreeFixedA t s) where
    type Result (CTMCOnTreeFixedA t s) = [Sequence]
    dist_name _ = "ctmc_on_tree_fixed_A"

instance (LabelledTree t,BranchLengthTree t, SimpleSModel s) => HasAnnotatedPdf (CTMCOnTreeFixedA t s) where
    annotated_densities (CTMCOnTreeFixedA tree smodel) = annotated_subst_likelihood_fixed_A tree smodel

ctmc_on_tree_fixed_A tree smodel = CTMCOnTreeFixedA tree smodel
