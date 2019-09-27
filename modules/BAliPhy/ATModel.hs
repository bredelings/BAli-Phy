module BAliPhy.ATModel where

import BAliPhy.ATModel.DataPartition
import BAliPhy.ATModel.DataPartition as DP
import Foreign.Vector
import Alignment
import Tree
import SModel

-- Assumptions FIXME: branch lengths -- we could have multiple set of them.
--                                      what we really need is transition probabilities for each partition.

data IndelModel = IndelModel
data ATModel = ATModel Tree [MixtureModels] [IndelModel] [Double] [Double] [Int] [Partition]
tree               (ATModel t _  _ _ _ _ _  ) = t
smodels            (ATModel _ ss _ _ _ _ _  ) = ss
imodels            (ATModel _ _ is _ _ _ _  ) = is
scales             (ATModel _ _ _ rs _ _ _  ) = rs
branch_lengths     (ATModel _ _ _ _ ls _ _  ) = ls
branch_categories  (ATModel _ _ _ _ _ cs _  ) = cs
partitions         (ATModel _ _ _ _ _  _ ps ) = ps

data ConditionalLikelihoodVector
data Matrix
data LeafSequence
data ATModelExport = ATModelExport ATModel [Array Int Matrix] [Array Int EVector] [Array Int ConditionalLikelihoodVector] [Double] [Array Int LeafSequence] Bool Double Bool Int EVector

-- we need to add field declarations!

get_atmodel                  (ATModelExport at _  _  _  _  _  _ _ _ _ _ ) = at
get_all_transition_ps        (ATModelExport _  ps _  _  _  _  _ _ _ _ _ ) = ps
get_all_cond_likes           (ATModelExport _  _  cl _  _  _  _ _ _ _ _ ) = cl
get_all_ancestral_sequences  (ATModelExport _  _  _  as _  _  _ _ _ _ _ ) = as
get_all_likelihoods          (ATModelExport _  _  _  _  ls _  _ _ _ _ _ ) = ls
leaf_sequences               (ATModelExport _  _  _  _  _  ls _ _ _ _ _ ) = ls
imodel_training              (ATModelExport _  _  _  _  _  _  t _ _ _ _ ) = t
heat                         (ATModelExport _  _  _  _  _  _  _ h _ _ _ ) = h
variable_alignment           (ATModelExport _  _  _  _  _  _  _ _ v _ _ ) = v
subst_root                   (ATModelExport _  _  _  _  _  _  _ _ _ r _ ) = r
sequence_names               (ATModelExport _  _  _  _  _  _  _ _ _ _ ns) = ns

observe_partition_type_0 partition compressed_alignment leaf_sequences column_counts alphabet branch_lengths subst_root = (transition_ps, cls, ancestral_sequences, likelihood)
    where tree = DP.get_tree partition
          as = pairwise_alignments (DP.get_alignment partition)
          scale = DP.scale partition
          smodel = DP.smodel partition
          smap   = stateLetters smodel
          distances = listArray' (map (scale *) branch_lengths)
          smodel_on_tree = SModel.SingleBranchLengthModel tree distances smodel
          transition_ps = transition_p_index smodel_on_tree
          n_leaves = numLeaves tree
          leaf_sequence_counts = listArray' (Alignment.leaf_sequence_counts compressed_alignment n_leaves column_counts)
          f = weighted_frequency_matrix smodel
          cls = cached_conditional_likelihoods
                  tree
                  leaf_sequences
                  leaf_sequence_counts
                  as
                  alphabet
                  transition_ps
                  f
          likelihood = peel_likelihood tree cls as (weighted_frequency_matrix smodel) subst_root
          ancestral_sequences = array_to_vector $ sample_ancestral_sequences tree subst_root leaf_sequences as alphabet transition_ps f cls smap
