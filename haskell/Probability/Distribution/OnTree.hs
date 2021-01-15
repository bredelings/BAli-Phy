module Probability.Distribution.OnTree where

import Probability.Random
import BAliPhy.ATModel
import BAliPhy.ATModel.DataPartition
import Tree
import SModel
import Bio.Sequence -- for sequence_to_indices

-- FIXME: need polymorphism.
--        This needs to be after weighted_frequency_matrix.
--        Because we have no polymorphism, wfm needs to be defined after MixtureModel and MixtureModels.

subst_like_on_tree tree root as smodel seqs = substitution_likelihood tree root seqs' as alphabet ps f smap
    where taxa = get_labels tree
          ds = Tree.branch_lengths tree
          f = weighted_frequency_matrix smodel
          ps = transition_p_index (SingleBranchLengthModel tree ds smodel)
          seqs' = listArray' $ map (sequence_to_indices alphabet) $ reorder_sequences taxa seqs
          alphabet = getAlphabet smodel
          smap = get_smap smodel

ctmc_on_tree tree root as smodel =
    Distribution (\seqs -> [subst_like_on_tree tree root as smodel seqs]) (no_quantile "ctmc_on_tree") () ()

subst_likelihood_fixed_A tree smodel sequences = likelihood
    where (_,_,_,likelihood) = observe_partition_type_1 tree lengths smodel sequences subst_root
          lengths = Tree.branch_lengths tree
          subst_root = numNodes tree - 1

ctmc_on_tree_fixed_A tree smodel =
    Distribution (\seqs -> [subst_likelihood_fixed_A tree smodel seqs]) (no_quantile "ctmc_on_tree_fixed_A") () ()
