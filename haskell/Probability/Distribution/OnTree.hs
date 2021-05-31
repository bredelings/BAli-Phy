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

subst_like_on_tree tree alignment smodel seqs = likelihood
    where (_,_,_,likelihood) = observe_partition_type_0 tree alignment smodel seqs subst_root
          subst_root = numNodes tree - 1

ctmc_on_tree tree alignment smodel =
    Distribution "ctmc_on_tree" (\seqs -> [subst_like_on_tree tree alignment smodel seqs]) (no_quantile "ctmc_on_tree") () ()

subst_likelihood_fixed_A tree smodel sequences = likelihood
    where (_,_,_,likelihood) = observe_partition_type_1 tree smodel sequences subst_root
          subst_root = numNodes tree - 1

ctmc_on_tree_fixed_A tree smodel =
    Distribution "ctmc_on_tree_fixed_A" (\seqs -> [subst_likelihood_fixed_A tree smodel seqs]) (no_quantile "ctmc_on_tree_fixed_A") () ()
