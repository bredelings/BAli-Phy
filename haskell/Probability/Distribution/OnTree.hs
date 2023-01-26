module Probability.Distribution.OnTree where

import Probability.Random
import BAliPhy.ATModel
import Tree
import SModel
import Bio.Sequence -- for sequence_to_indices
import Bio.Alignment
import Bio.Alphabet  -- for type Alphabet
import Data.Matrix
import Data.Foldable

-- FIXME: need polymorphism.
--        This needs to be after weighted_frequency_matrix.
--        Because we have no polymorphism, wfm needs to be defined after MixtureModel and MixtureModels.

data CTMCOnTreeProperties = CTMCOnTreeProperties {
      prop_subst_root :: Int,
      prop_transition_ps :: Array Int (EVector (Matrix Double)),
      prop_cond_likes :: Array Int CondLikes,
      prop_anc_seqs :: EVector VectorPairIntInt,
      prop_likelihood :: LogDouble,
      prop_taxa :: Array Int CPPString,
      prop_get_weighted_frequency_matrix :: Matrix Double,
      prop_smap :: EVector Int,
      prop_leaf_sequences :: Array Int (EVector Int),
      prop_alphabet :: Alphabet,
      prop_as :: Array Int PairwiseAlignment,
      prop_n_states :: Int,
      prop_n_base_models :: Int
    }

data CTMCOnTreeFixedAProperties = CTMCOnTreeFixedAProperties {
      prop_fa_subst_root :: Int,
      prop_fa_transition_ps :: Array Int (EVector (Matrix Double)),
      prop_fa_cond_likes :: Array Int CondLikes,
      prop_fa_anc_seqs :: AlignmentMatrix,
      prop_fa_likelihood :: LogDouble,
      prop_fa_taxa :: [CPPString],
      prop_fa_get_weighted_frequency_matrix :: Matrix Double,
      prop_fa_smap :: EVector Int,
      prop_fa_leaf_sequences :: Array Int (EVector Int),
      prop_fa_alphabet :: Alphabet,
      prop_fa_n_states :: Int,
      prop_fa_n_base_models :: Int
    }


annotated_subst_like_on_tree tree alignment smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let n_leaves = numLeaves tree
      as = pairwise_alignments alignment
      taxa = get_labels tree
      taxa_list = toList taxa
      leaf_sequences = listArray' $ map (sequence_to_indices alphabet) $ reorder_sequences taxa_list sequences
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      transition_ps = transition_p_index smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods
              tree
              leaf_sequences
              as
              alphabet
              transition_ps
              f
              smap
      likelihood = if n_leaves == 1 then
                       peel_likelihood_1 (leaf_sequences ! 0) alphabet f
                   else if n_leaves == 2 then
                       peel_likelihood_2 (leaf_sequences ! 0) (leaf_sequences ! 1) alphabet (as ! 0) (transition_ps ! 0) f
                   else
                       peel_likelihood tree cls as (weighted_frequency_matrix smodel) subst_root
      ancestral_sequences = if n_leaves == 1 then
                                list_to_vector $ []
                            else if n_leaves == 2 then
                                list_to_vector $ []
                            else
                                array_to_vector $ sample_ancestral_sequences tree subst_root leaf_sequences as alphabet transition_ps f cls smap

  in_edge "tree" tree
  in_edge "alignment" alignment
  in_edge "smodel" smodel

  property "taxa" (map list_to_string taxa_list)
  property "properties" (CTMCOnTreeProperties subst_root transition_ps cls ancestral_sequences likelihood (fmap list_to_string taxa) f smap leaf_sequences alphabet as (SModel.nStates smodel) (SModel.nBaseModels smodel) )

  return [likelihood]

ctmc_on_tree tree alignment smodel =
    Distribution "ctmc_on_tree" (annotated_subst_like_on_tree tree alignment smodel) (no_quantile "ctmc_on_tree") undefined undefined

annotated_subst_likelihood_fixed_A tree smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let a0 = alignment_from_sequences alphabet sequences
      (compressed_alignment',column_counts,mapping) = compress_alignment $ a0
      n_leaves = numLeaves tree
      taxa = get_labels tree
      taxa_list = toList taxa
      compressed_alignment = reorder_alignment taxa_list compressed_alignment'
      alphabet = getAlphabet smodel
      smap   = stateLetters smodel
      smodel_on_tree = SingleBranchLengthModel tree smodel
      leaf_sequences = listArray' $ sequences_from_alignment compressed_alignment
      transition_ps = transition_p_index smodel_on_tree
      f = weighted_frequency_matrix smodel
      cls = cached_conditional_likelihoods_SEV
              tree
              leaf_sequences
              alphabet
              transition_ps
              f
              compressed_alignment
              smap
      likelihood = if n_leaves == 1 then
                       peel_likelihood_1_SEV compressed_alignment alphabet f column_counts
                   else if n_leaves == 2 then
                       peel_likelihood_2_SEV compressed_alignment alphabet (transition_ps!0) f column_counts
                   else
                       peel_likelihood_SEV tree cls f subst_root column_counts
--    This also needs the map from columns to compressed columns:
      ancestral_sequences = if n_leaves == 1 then
                                a0
                            else if n_leaves == 2 then
                                a0
                            else
                                let ancestral_states = array_to_vector $ sample_ancestral_sequences_SEV
                                      tree
                                      subst_root
                                      leaf_sequences
                                      alphabet
                                      transition_ps
                                      f
                                      cls
                                      smap
                                      mapping
                                in ancestral_sequence_alignment a0 ancestral_states smap
  in_edge "tree" tree
  in_edge "smodel" smodel

  property "taxa" (map list_to_string taxa_list)
  -- How about stuff related to alignment compression?
  property "properties" (CTMCOnTreeFixedAProperties subst_root transition_ps cls ancestral_sequences likelihood (map list_to_string taxa_list) f smap leaf_sequences alphabet (SModel.nStates smodel) (SModel.nBaseModels smodel) )

  return [likelihood]

ctmc_on_tree_fixed_A tree smodel =
    Distribution "ctmc_on_tree_fixed_A" (annotated_subst_likelihood_fixed_A tree smodel) (no_quantile "ctmc_on_tree_fixed_A") undefined undefined
