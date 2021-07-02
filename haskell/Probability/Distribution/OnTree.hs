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

annotated_subst_like_on_tree tree alignment smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_0 tree alignment smodel sequences subst_root

  tree <- in_edge "tree" tree
  alignment <- in_edge "alignment" alignment
  smodel <- in_edge "smodel" smodel

  property "subst_root" subst_root
  property "transition_prs" transition_ps
  property "cond_likes" cls
  property "anc_seqs" anc_seqs
  property "likelihood" likelihood

  return [likelihood]

ctmc_on_tree tree alignment smodel =
    Distribution "ctmc_on_tree" (annotated_subst_like_on_tree tree alignment smodel) (no_quantile "ctmc_on_tree") () ()

annotated_subst_likelihood_fixed_A tree smodel sequences = do
  let subst_root = modifiable (numNodes tree - 1)

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_1 tree smodel sequences subst_root

  in_edge "tree" tree
  in_edge "smodel" smodel

  property "subst_root" subst_root
  property "transition_prs" transition_ps
  property "cond_likes" cls
  property "anc_seqs" anc_seqs
  property "likelihood" likelihood

  return [likelihood]

ctmc_on_tree_fixed_A tree smodel =
    Distribution "ctmc_on_tree_fixed_A" (annotated_subst_likelihood_fixed_A tree smodel) (no_quantile "ctmc_on_tree_fixed_A") () ()
