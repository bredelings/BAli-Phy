module MCMC where

import Foreign.Pair
import Foreign.Vector
import Range

builtin builtin_register_transition_kernel 2 "register_transition_kernel" "MCMC"
register_transition_kernel rate move = IOAction (\s -> (s,builtin_register_transition_kernel rate move))

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
builtin builtin_gibbs_sample_categorical 4 "gibbs_sample_categorical" "MCMC"
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

builtin builtin_discrete_uniform_avoid_mh 5 "discrete_uniform_avoid_mh" "MCMC"
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
builtin builtin_inc_dec_mh 4 "inc_dec_mh" "MCMC"
inc_dec_mh x bnds c = IOAction (pair_from_c . builtin_inc_dec_mh x bnds c)

builtin builtin_slice_sample_real_random_variable 4 "slice_sample_real_random_variable" "MCMC"
slice_sample_real_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x (c_range bnds) c)

builtin builtin_slice_sample_integer_random_variable 4 "slice_sample_integer_random_variable" "MCMC"
slice_sample_integer_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x (c_range bnds) c)

builtin builtin_walk_tree_path 2 "walk_tree_path" "MCMC"
walk_tree_path tree c = vector_to_list $ builtin_walk_tree_path tree c

-- This is "unsafe" because it doesn't update alignments
builtin builtin_nni_on_branch_unsafe 3 "NNI_on_branch_unsafe" "MCMC"
nni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_nni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin builtin_tnni_on_branch_unsafe 3 "TT_NNI_on_branch_unsafe" "MCMC"
tnni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_tnni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin builtin_fnpr_unsafe_proposal 4 "FNPR_unsafe" "MCMC"
fnpr_unsafe_proposal tree node c = IOAction (pair_from_c . builtin_fnpr_unsafe_proposal tree node c)

walk_tree_sample_nni_unsafe tree c = sequence_ [ nni_on_branch_unsafe tree branch c | branch <- walk_tree_path tree c]

builtin builtin_walk_tree_sample_alignments 3 "walk_tree_sample_alignments" "MCMC"
walk_tree_sample_alignments tree c = IOAction (pair_from_c . builtin_walk_tree_sample_alignments tree c)

builtin builtin_walk_tree_sample_NNI 3 "walk_tree_sample_NNI" "MCMC"
walk_tree_sample_NNI tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI tree c)

builtin builtin_walk_tree_sample_NNI_and_branch_lengths 3 "walk_tree_sample_NNI_and_branch_lengths" "MCMC"
walk_tree_sample_NNI_and_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_branch_lengths tree c)

builtin builtin_copy_context 2 "copy_context" "MCMC"
copy_context c = IOAction (pair_from_c . builtin_copy_context c)

builtin builtin_release_context 2 "release_context" "MCMC"
release_context c = IOAction (pair_from_c . builtin_release_context c)

builtin builtin_switch_to_context 3 "switch_to_context" "MCMC"
switch_to_context c1 c2  = IOAction (pair_from_c . builtin_switch_to_context c1 c2)

builtin builtin_accept_MH 4 "accept_MH" "MCMC"
accept_MH c1 c2 ratio  = IOAction (pair_from_c . builtin_accept_MH c1 c2 ratio)

-- TODO: What if copy_context returns a Box<context>?
--       Then if memory is tight, we would destroy the context object, and release the  context.
--       This might take a while though if garbage-collection didn't happen immediately.
--       And we might need to pivot back to c2 later to release it later.


-- Proposal = Context -> IO LogDouble
data Proposal
data Context

metropolis_hastings :: Proposal -> Context -> IO Bool
metropolis_hastings proposal c1 = do
  c2 <- copy_context c1
  ratio <- proposal c2
  accept <- accept_MH c1 c2 ratio
  if accept then switch_to_context c1 c2 else return ()
  release_context c2
  return accept

