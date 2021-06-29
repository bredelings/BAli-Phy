module Probability.Distribution.FakeDist where

import Probability.Random

fake_sample_error = error "sampling from fake_dist is not allowed.  You can only observe it."
fake_range_error = error "fake_dist has no range.  You can only observe it."

annotated_fake_dist_0_pr tree' alignment' smodel' subst_root transition_ps cls ancestral_sequences likelihood sequences = do
  tree <- in_edge "tree" tree'
  alignment <- in_edge "alignment" alignment'
  smodel <- in_edge "smodel" smodel'
  property "subst_root" subst_root
  property "transition_ps" transition_ps
  property "cond_likes" cls
  property "anc_seqs" ancestral_sequences
  property "likelihood" likelihood
  return $ tree `seq` alignment `seq` smodel `seq` [likelihood]

fake_dist_0 tree alignment smodel subst_root transition_ps cls ancestral_sequences likelihood =
    Distribution "fake_dist_0" (annotated_fake_dist_0_pr tree alignment smodel subst_root transition_ps cls ancestral_sequences likelihood) (no_quantile "fake_dist_0") fake_sample_error fake_range_error

annotated_fake_dist_1_pr tree' smodel' subst_root transition_ps cls ancestral_sequences likelihood sequences = do
  tree <- in_edge "tree" tree'
  smodel <- in_edge "smodel" smodel'
  property "subst_root" subst_root
  property "transition_ps" transition_ps
  property "cond_likes" cls
  property "anc_seqs" ancestral_sequences
  property "likelihood" likelihood
  return $ tree `seq` smodel `seq` [likelihood]

fake_dist_1 tree smodel subst_root transition_ps cls ancestral_sequences likelihood =
    Distribution "fake_dist_1" (annotated_fake_dist_1_pr tree smodel subst_root transition_ps cls ancestral_sequences likelihood) (no_quantile "fake_dist_1") fake_sample_error fake_range_error
