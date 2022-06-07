module MCMC where

import Foreign.Pair
import Foreign.Vector
import Range
import Effect
import Tree

type ContextIndex = Int

type TransitionKernel a = ContextIndex -> IO a

type Proposal = ContextIndex -> IO LogDouble

foreign import bpcall "MCMC:register_transition_kernel" builtin_register_transition_kernel :: Double -> TransitionKernel a -> Effect
register_transition_kernel rate move = IOAction (\s -> (s,builtin_register_transition_kernel rate move))

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
foreign import bpcall "MCMC:gibbs_sample_categorical" builtin_gibbs_sample_categorical :: Int -> Int -> Int -> RealWorld -> EPair RealWorld ()
gibbs_sample_categorical x n c = IOAction (pair_from_c . builtin_gibbs_sample_categorical x n c)

foreign import bpcall "MCMC:discrete_uniform_avoid_mh" builtin_discrete_uniform_avoid_mh :: Int -> Int -> Int -> Int -> Int -> EPair RealWorld ()
discrete_uniform_avoid_mh x low high c = IOAction (pair_from_c . builtin_discrete_uniform_avoid_mh x low high c)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
foreign import bpcall "MCMC:inc_dec_mh" builtin_inc_dec_mh :: Int -> BuiltinBounds -> Int -> Int -> EPair RealWorld ()
inc_dec_mh x bnds c = IOAction (pair_from_c . builtin_inc_dec_mh x (c_range bnds) c)

foreign import bpcall "MCMC:slice_sample_real_random_variable" builtin_slice_sample_real_random_variable :: Double -> BuiltinBounds -> ContextIndex -> RealWorld -> EPair RealWorld ()
slice_sample_real_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_real_random_variable x (c_range bnds) c)

foreign import bpcall "MCMC:slice_sample_integer_random_variable" builtin_slice_sample_integer_random_variable :: Int -> BuiltinBounds -> ContextIndex -> RealWorld -> EPair RealWorld ()
slice_sample_integer_random_variable x bnds c = IOAction (pair_from_c . builtin_slice_sample_integer_random_variable x (c_range bnds) c)

foreign import bpcall "MCMC:walk_tree_path" builtin_walk_tree_path :: TreeImp -> ContextIndex -> EVector Int
walk_tree_path tree c = vector_to_list $ builtin_walk_tree_path tree c

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:NNI_on_branch_unsafe" builtin_nni_on_branch_unsafe :: TreeImp -> Int -> ContextIndex -> ()
nni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_nni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:TT_NNI_on_branch_unsafe" builtin_tnni_on_branch_unsafe :: TreeImp -> Int -> ContextIndex -> ()
tnni_on_branch_unsafe tree branch c = IOAction (\s->(s,builtin_tnni_on_branch_unsafe tree branch c))

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:FNPR_unsafe" builtin_fnpr_unsafe_proposal :: TreeImp -> Int -> ContextIndex -> RealWorld -> EPair RealWorld LogDouble
fnpr_unsafe_proposal tree node c = IOAction (pair_from_c . builtin_fnpr_unsafe_proposal tree node c)

walk_tree_sample_nni_unsafe tree c = sequence_ [ nni_on_branch_unsafe tree branch c | branch <- walk_tree_path tree c]

foreign import bpcall "MCMC:walk_tree_sample_alignments" builtin_walk_tree_sample_alignments :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
walk_tree_sample_alignments tree c = IOAction (pair_from_c . builtin_walk_tree_sample_alignments tree c)

foreign import bpcall "MCMC:realign_from_tips" builtin_realign_from_tips :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
realign_from_tips tree c = IOAction (pair_from_c . builtin_realign_from_tips tree c)

foreign import bpcall "MCMC:walk_tree_sample_NNI" builtin_walk_tree_sample_NNI :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
walk_tree_sample_NNI tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI tree c)

foreign import bpcall "MCMC:walk_tree_sample_NNI_and_A" builtin_walk_tree_sample_NNI_and_A :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
walk_tree_sample_NNI_and_A tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_A tree c)

foreign import bpcall "MCMC:walk_tree_sample_NNI_and_branch_lengths" builtin_walk_tree_sample_NNI_and_branch_lengths :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
walk_tree_sample_NNI_and_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_NNI_and_branch_lengths tree c)

foreign import bpcall "MCMC:walk_tree_sample_branch_lengths" builtin_walk_tree_sample_branch_lengths :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
walk_tree_sample_branch_lengths tree c = IOAction (pair_from_c . builtin_walk_tree_sample_branch_lengths tree c)

foreign import bpcall "MCMC:sample_SPR_all" builtin_sample_SPR_all :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
sample_SPR_all tree c = IOAction (pair_from_c . builtin_sample_SPR_all tree c)

foreign import bpcall "MCMC:sample_SPR_nodes" builtin_sample_SPR_nodes :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
sample_SPR_nodes tree c = IOAction (pair_from_c . builtin_sample_SPR_nodes tree c)

foreign import bpcall "MCMC:sample_SPR_flat" builtin_sample_SPR_flat :: TreeImp -> ContextIndex -> RealWorld -> EPair RealWorld ()
sample_SPR_flat tree c = IOAction (pair_from_c . builtin_sample_SPR_flat tree c)

foreign import bpcall "MCMC:copy_context" builtin_copy_context :: ContextIndex -> RealWorld -> EPair RealWorld ContextIndex
copy_context c = IOAction (pair_from_c . builtin_copy_context c)

foreign import bpcall "MCMC:release_context" builtin_release_context :: ContextIndex -> RealWorld -> EPair RealWorld ContextIndex
release_context c = IOAction (pair_from_c . builtin_release_context c)

foreign import bpcall "MCMC:switch_to_context" builtin_switch_to_context :: ContextIndex -> ContextIndex -> RealWorld -> EPair RealWorld ()
switch_to_context c1 c2  = IOAction (pair_from_c . builtin_switch_to_context c1 c2)

foreign import bpcall "MCMC:accept_MH" builtin_accept_MH :: ContextIndex -> ContextIndex -> LogDouble -> RealWorld -> EPair RealWorld Bool
accept_MH c1 c2 ratio  = IOAction (pair_from_c . builtin_accept_MH c1 c2 ratio)

-- TODO: What if copy_context returns a Box<context>?
--       Then if memory is tight, we would destroy the context object, and release the  context.
--       This might take a while though if garbage-collection didn't happen immediately.
--       And we might need to pivot back to c2 later to release it later.


metropolis_hastings :: Proposal -> ContextIndex -> IO Bool
metropolis_hastings proposal c1 = do
  c2 <- copy_context c1
  ratio <- proposal c2
  accept <- accept_MH c1 c2 ratio
  if accept then switch_to_context c1 c2 else return ()
  release_context c2
  return accept

