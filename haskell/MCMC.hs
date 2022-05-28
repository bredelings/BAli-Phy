module MCMC where

import Foreign.Pair
import Foreign.Vector
import Range

builtin "MCMC:register_transition_kernel" builtin_register_transition_kernel 2
register_transition_kernel rate move = IOAction (\s -> (s,builtin_register_transition_kernel rate move))

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
builtin "MCMC:gibbs_sample_categorical" builtin_gibbs_sample_categorical 4
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

builtin "MCMC:discrete_uniform_avoid_mh" builtin_discrete_uniform_avoid_mh 5
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
builtin "MCMC:inc_dec_mh" builtin_inc_dec_mh 4
inc_dec_mh x bnds c = IOAction (pair_from_c . builtin_inc_dec_mh x bnds c)

builtin "MCMC:slice_sample_real_random_variable" builtin_slice_sample_real_random_variable 4
slice_sample_real_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x (c_range bnds) c)

builtin "MCMC:slice_sample_integer_random_variable" builtin_slice_sample_integer_random_variable 4
slice_sample_integer_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x (c_range bnds) c)

builtin "MCMC:walk_tree_path" builtin_walk_tree_path 2
walk_tree_path tree c = vector_to_list $ builtin_walk_tree_path tree c

-- This is "unsafe" because it doesn't update alignments
builtin "MCMC:NNI_on_branch_unsafe" builtin_nni_on_branch_unsafe 3
nni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_nni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin "MCMC:TT_NNI_on_branch_unsafe" builtin_tnni_on_branch_unsafe 3
tnni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_tnni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
builtin "MCMC:FNPR_unsafe" builtin_fnpr_unsafe_proposal 4
fnpr_unsafe_proposal tree node c = IOAction (pair_from_c . builtin_fnpr_unsafe_proposal tree node c)

walk_tree_sample_nni_unsafe tree c = sequence_ [ nni_on_branch_unsafe tree branch c | branch <- walk_tree_path tree c]

builtin "MCMC:walk_tree_sample_alignments" builtin_walk_tree_sample_alignments 3
walk_tree_sample_alignments tree c = IOAction (pair_from_c . builtin_walk_tree_sample_alignments tree c)

builtin "MCMC:realign_from_tips" builtin_realign_from_tips 3
realign_from_tips tree c = IOAction (pair_from_c . builtin_realign_from_tips tree c)

builtin "MCMC:walk_tree_sample_NNI" builtin_walk_tree_sample_NNI 3
walk_tree_sample_NNI tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI tree c)

builtin "MCMC:walk_tree_sample_NNI_and_A" builtin_walk_tree_sample_NNI_and_A 3
walk_tree_sample_NNI_and_A tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_A tree c)

builtin "MCMC:walk_tree_sample_NNI_and_branch_lengths" builtin_walk_tree_sample_NNI_and_branch_lengths 3
walk_tree_sample_NNI_and_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_branch_lengths tree c)

builtin "MCMC:walk_tree_sample_branch_lengths" builtin_walk_tree_sample_branch_lengths 3
walk_tree_sample_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_branch_lengths tree c)

builtin "MCMC:sample_SPR_all" builtin_sample_SPR_all 3
sample_SPR_all tree c = IOAction (pair_from_c . builtin_sample_SPR_all tree c)

builtin "MCMC:sample_SPR_nodes" builtin_sample_SPR_nodes 3
sample_SPR_nodes tree c = IOAction (pair_from_c . builtin_sample_SPR_nodes tree c)

builtin "MCMC:sample_SPR_flat" builtin_sample_SPR_flat 3
sample_SPR_flat tree c = IOAction (pair_from_c . builtin_sample_SPR_flat tree c)

builtin "MCMC:copy_context" builtin_copy_context 2
copy_context c = IOAction (pair_from_c . builtin_copy_context c)

builtin "MCMC:release_context" builtin_release_context 2
release_context c = IOAction (pair_from_c . builtin_release_context c)

builtin "MCMC:switch_to_context" builtin_switch_to_context 3
switch_to_context c1 c2  = IOAction (pair_from_c . builtin_switch_to_context c1 c2)

builtin "MCMC:accept_MH" builtin_accept_MH 4
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

