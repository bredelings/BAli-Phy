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

annotated_subst_like_on_tree tree' alignment' smodel' seqs = do
  tree <- in_edge "tree" tree'
  alignment <- in_edge "alignment" alignment'
  smodel <- in_edge "smodel" smodel'

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_0 tree alignment smodel seqs subst_root
      subst_root = numNodes tree - 1

  -- we may want to make almost every variable from observe_partition_type into a property
  property "transition_prs" transition_ps
  property "condition likelihoods" cls
  property "ancestral sequences" anc_seqs
  property "likelihood" likelihood
  property "weighted frequencies" ()

  return [likelihood]

ctmc_on_tree tree alignment smodel =
    Distribution "ctmc_on_tree" (annotated_subst_like_on_tree tree alignment smodel) (no_quantile "ctmc_on_tree") () ()

annotated_subst_likelihood_fixed_A tree' smodel' sequences = do
  tree <- in_edge "tree" tree'
  smodel <- in_edge "smodel" smodel'

  let (transition_ps, cls, anc_seqs, likelihood) = observe_partition_type_1 tree smodel sequences subst_root
      subst_root = numNodes tree - 1

  -- we may want to make almost every variable from observe_partition_type into a property
  property "transition_prs" transition_ps
  property "condition likelihoods" cls
--  property "ancestral sequences" anc_seqs
  property "likelihood" likelihood
  property "weighted frequencies" ()

  return [likelihood]

ctmc_on_tree_fixed_A tree smodel =
    Distribution "ctmc_on_tree_fixed_A" (annotated_subst_likelihood_fixed_A tree smodel) (no_quantile "ctmc_on_tree_fixed_A") () ()
