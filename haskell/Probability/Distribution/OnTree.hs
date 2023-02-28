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

-- FIXME: need polymorphism.
--        This needs to be after weighted_frequency_matrix.
--        Because we have no polymorphism, wfm needs to be defined after MixtureModel and MixtureModels.

data CTMCOnTreeProperties = CTMCOnTreeProperties {
      prop_subst_root :: Int,
      prop_transition_ps :: Array Int (EVector (Matrix Double)),
      prop_cond_likes :: Array Int CondLikes,
      prop_anc_seqs :: EVector VectorPairIntInt,
      prop_likelihood :: LogDouble,
      prop_taxa :: IntMap (CMaybe CPPString),
      prop_get_weighted_frequency_matrix :: Matrix Double,
      prop_smap :: EVector Int,
      prop_leaf_sequences :: IntMap (EVector Int),
      prop_alphabet :: Alphabet,
      prop_as :: IntMap PairwiseAlignment,
      prop_n_states :: Int,
      prop_n_base_models :: Int
    }

data CTMCOnTreeFixedAProperties = CTMCOnTreeFixedAProperties {
      prop_fa_subst_root :: Int,
      prop_fa_transition_ps :: Array Int (EVector (Matrix Double)),
      prop_fa_cond_likes :: Array Int CondLikes,
      prop_fa_anc_seqs :: AlignmentMatrix,
      prop_fa_likelihood :: LogDouble,
      prop_fa_taxa :: IntMap (CMaybe CPPString),
      prop_fa_get_weighted_frequency_matrix :: Matrix Double,
      prop_fa_smap :: EVector Int,
      prop_fa_leaf_sequences :: IntMap (EVector Int),
      prop_fa_alphabet :: Alphabet,
      prop_fa_n_states :: Int,
      prop_fa_n_base_models :: Int
    }


annotated_subst_like_on_tree tree alignment smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let n_nodes = numNodes tree
      as = pairwise_alignments alignment
      taxa = fmap (cMaybe . fmap (\(Text s) -> s)) $ get_labels tree
      find_sequence label = find (\s -> sequence_name s == label) sequences
      node_sequences' = getNodesSet tree & IntMap.fromSet (\node -> case get_label tree node of Just label -> find_sequence label;
                                                                                                Nothing ->  error "No label")
      node_sequences = fmap (sequence_to_indices alphabet . fromJust) node_sequences'
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_p_index smodel_on_tree
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
                                    in peel_likelihood_2 (node_sequences IntMap.! n1) (node_sequences IntMap.! n2) alphabet (as IntMap.! 0) (transition_ps ! 0) f
      ancestral_sequences = case n_nodes of
                              1 -> list_to_vector $ []
                              2 -> list_to_vector $ []
                              _ -> array_to_vector $ sample_ancestral_sequences tree subst_root node_sequences as alphabet transition_ps f cls smap

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  property "properties" (CTMCOnTreeProperties subst_root transition_ps cls ancestral_sequences likelihood taxa f smap node_sequences alphabet as (SModel.nStates smodel) (SModel.nBaseModels smodel) )

  return [likelihood]

ctmc_on_tree tree alignment smodel =
    Distribution "ctmc_on_tree" (annotated_subst_like_on_tree tree alignment smodel) (no_quantile "ctmc_on_tree") undefined undefined

annotated_subst_likelihood_fixed_A tree smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let a0 = alignment_from_sequences alphabet sequences
      (compressed_alignment,column_counts,mapping) = compress_alignment $ a0
      n_nodes = numNodes tree
      taxa = fmap (cMaybe . fmap (\(Text s) -> s)) $ get_labels tree
      find_sequence label = find (\s -> sequence_name s == label) sequences
      node_sequences' = getNodesSet tree & IntMap.fromSet (\node -> case get_label tree node of Just label -> find_sequence label;
                                                                                                Nothing ->  error "No label")
      node_sequences = fmap (sequence_to_indices alphabet . fromJust) node_sequences'
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_p_index smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods_SEV
              tree
              node_sequences
              alphabet
              transition_ps
              f
              compressed_alignment
              smap
      likelihood | n_nodes > 2    = peel_likelihood_SEV tree cls f subst_root column_counts
                 | n_nodes == 1   = peel_likelihood_1_SEV compressed_alignment alphabet f column_counts
                 | n_nodes == 2   = peel_likelihood_2_SEV compressed_alignment alphabet (transition_ps!0) f column_counts

--    This also needs the map from columns to compressed columns:
      ancestral_sequences = case n_nodes of
                              1 -> a0
                              2 -> a0
                              _ -> let ancestral_states = array_to_vector $ sample_ancestral_sequences_SEV
                                         tree
                                         subst_root
                                         node_sequences
                                         alphabet
                                         transition_ps
                                         f
                                         cls
                                         smap
                                         mapping
                                   in ancestral_sequence_alignment a0 ancestral_states smap
  in_edge "tree" tree
  in_edge "smodel" smodel

  -- How about stuff related to alignment compression?
  property "properties" (CTMCOnTreeFixedAProperties subst_root transition_ps cls ancestral_sequences likelihood taxa f smap node_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) )

  return [likelihood]

ctmc_on_tree_fixed_A tree smodel =
    Distribution "ctmc_on_tree_fixed_A" (annotated_subst_likelihood_fixed_A tree smodel) (no_quantile "ctmc_on_tree_fixed_A") undefined undefined
