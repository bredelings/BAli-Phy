module MCMC where

import Foreign.Vector
import Range
import Effect
import Tree
import Numeric.LogDouble
import Data.Text (Text(..))

type ContextIndex = Int

type TransitionKernel a = ContextIndex -> IO a

type LoggerAction = Int -> Double -> Double -> Double -> IO ()

type Proposal = ContextIndex -> IO LogDouble

foreign import bpcall "MCMC:" register_transition_kernel :: Double -> TransitionKernel a -> IO Effect

foreign import bpcall "MCMC:" register_logger :: LoggerAction -> IO Effect

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
foreign import bpcall "MCMC:" gibbs_sample_categorical :: Int -> Int -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" discrete_uniform_avoid_mh :: Int -> Int -> Int -> ContextIndex -> IO ()

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
foreign import bpcall "MCMC:" inc_dec_mh_raw :: Int -> BuiltinBounds -> Int -> IO ()
inc_dec_mh x bnds c = inc_dec_mh_raw x (c_range bnds) c

foreign import bpcall "MCMC:" slice_sample_real_random_variable_raw :: Double -> BuiltinBounds -> ContextIndex -> IO ()
slice_sample_real_random_variable x bnds c = slice_sample_real_random_variable_raw x (c_range bnds) c

foreign import bpcall "MCMC:" slice_sample_integer_random_variable_raw :: Int -> BuiltinBounds -> ContextIndex -> IO ()
slice_sample_integer_random_variable x bnds c = slice_sample_integer_random_variable_raw x (c_range bnds) c

foreign import bpcall "MCMC:walk_tree_path" builtin_walk_tree_path :: t -> ContextIndex -> EVector Int
walk_tree_path tree c = vector_to_list $ builtin_walk_tree_path tree c

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" nni_on_branch_unsafe :: t -> Int -> ContextIndex -> IO ()

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" tnni_on_branch_unsafe :: t -> Int -> ContextIndex -> IO ()

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" fnpr_unsafe_proposal :: t -> Int -> ContextIndex -> IO LogDouble

walk_tree_sample_nni_unsafe tree c = sequence_ [ nni_on_branch_unsafe tree branch c | branch <- walk_tree_path tree c]

foreign import bpcall "MCMC:" walk_tree_sample_alignments :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" realign_from_tips :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walk_tree_sample_NNI :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walk_tree_sample_NNI_and_A :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walk_tree_sample_NNI_and_branch_lengths :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walk_tree_sample_branch_lengths :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sample_SPR_all :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sample_SPR_nodes :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sample_SPR_flat :: t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" copy_context :: ContextIndex -> IO ContextIndex

foreign import bpcall "MCMC:" release_context :: ContextIndex -> IO ()

foreign import bpcall "MCMC:" switch_to_context :: ContextIndex -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" accept_MH :: ContextIndex -> ContextIndex -> LogDouble -> IO Bool

-- TODO: What if copy_context returns a Box<context>?
--       Then if memory is tight, we would destroy the context object, and release the  context.
--       This might take a while though if garbage-collection didn't happen immediately.
--       And we might need to pivot back to c2 later to release it later.
--
--       So... we need some kind of ContextPtr object that holds a reference to the context
--       until the ContextPtr is destroyed?

foreign import bpcall "MCMC:" scale_means_only_slice :: [Double] -> [Double] -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" scale_means_only_proposal :: [Double] -> [Double] -> ContextIndex -> IO LogDouble

foreign import bpcall "MCMC:" createContext :: a -> IO ContextIndex
makeModel m = createContext (unsafePerformIO m)

foreign import bpcall "MCMC:" runMCMC :: Int -> ContextIndex -> IO ()
foreign import bpcall "MCMC:" logLineRaw :: Int -> IO CPPString
logLine context = Text <$> logLineRaw context

scale_means_only_MH scales lengths = metropolis_hastings $ scale_means_only_proposal scales lengths

metropolis_hastings :: Proposal -> ContextIndex -> IO Bool
metropolis_hastings proposal c1 = do
  c2 <- copy_context c1
  ratio <- proposal c2
  accept <- accept_MH c1 c2 ratio
  if accept then switch_to_context c1 c2 else return ()
  release_context c2
  return accept
