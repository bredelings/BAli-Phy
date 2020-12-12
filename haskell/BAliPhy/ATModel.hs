module BAliPhy.ATModel where

import BAliPhy.ATModel.DataPartition
import BAliPhy.ATModel.DataPartition as DP
import Foreign.Vector
import Bio.Alignment
import Tree
import SModel

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel {
      tree:: Tree ,
      smodels :: [MixtureModels],
      scales :: [Double],
      branch_lengths :: [Double],
      branch_categories :: [Int],
      partitions :: [Partition]
    }

data ConditionalLikelihoodVector
data Matrix
data LeafSequence
data ATModelExport = ATModelExport
    {
      get_atmodel :: ATModel,
      get_all_transition_ps :: [Array Int Matrix],
      get_all_cond_likes :: [Array Int EVector],
      get_all_ancestral_sequences :: [Array Int ConditionalLikelihoodVector],
      get_all_likelihoods :: [Double],
      leaf_sequences :: [Array Int LeafSequence],
      imodel_training :: Bool,
      heat :: Double,
      variable_alignment :: Bool,
      subst_root :: Int,
      sequence_names :: EVector
    }

observe_partition_type_0 partition sequence_data subst_root = (transition_ps, cls, ancestral_sequences, likelihood)
    where tree = DP.get_tree partition
          n_leaves = numLeaves tree
          taxa = get_labels tree
          leaf_sequences = listArray' $ map (sequence_to_indices alphabet) $ reorder_sequences taxa sequence_data
          as = pairwise_alignments (DP.get_alignment partition)
          distances = DP.get_branch_lengths partition
          smodel = DP.smodel partition
          alphabet = getAlphabet smodel
          smap   = stateLetters smodel
          smodel_on_tree = SModel.SingleBranchLengthModel tree distances smodel
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
                                    0
                                else if n_leaves == 2 then
                                    0
                                else
                                    array_to_vector $ sample_ancestral_sequences tree subst_root leaf_sequences as alphabet transition_ps f cls smap

observe_partition_type_1 partition sequences subst_root = (transition_ps, cls, ancestral_sequences, likelihood)
    where a0 = alignment_from_sequences alphabet sequences
          (compressed_alignment',column_counts,mapping) = compress_alignment $ a0
          tree = DP.get_tree partition
          n_leaves = numLeaves tree
          taxa = get_labels tree
          compressed_alignment = reorder_alignment taxa compressed_alignment'
          distances = DP.get_branch_lengths partition
          smodel = DP.smodel partition
          alphabet = getAlphabet smodel
          smap   = stateLetters smodel
          smodel_on_tree = SModel.SingleBranchLengthModel tree distances smodel
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
--        This also needs the map from columns to compressed columns:
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
