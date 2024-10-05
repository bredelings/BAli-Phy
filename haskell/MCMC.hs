module MCMC where

import Foreign.Vector
import Range
import Effect
import Tree
import Numeric.LogDouble
import qualified Data.Text as T
import Data.JSON
import Data.Foldable (toList)
import Probability.Dist

-- data ContextIndex = ContextIndex Int
type ContextIndex = Int

data TransitionKernel = TransitionKernel (ContextIndex -> IO ())

runTK c (TransitionKernel kernel) = kernel c

--- The first four arguments allow giving the logger the generation number, prior, likelihood, and probability.
type LoggerAction = Int -> Double -> Double -> Double -> IO ()

data Proposal = Proposal (ContextIndex -> IO LogDouble)

-- It is unfortunate that modifiable-ness is not visible at the type level.
type Modifiable a = a

foreign import bpcall "MCMC:" registerTransitionKernelRaw :: Double -> (ContextIndex -> IO ()) -> IO Effect
registerTransitionKernel rate (TransitionKernel kernel) = registerTransitionKernelRaw rate kernel                                                             

foreign import bpcall "MCMC:" registerLogger :: LoggerAction -> IO Effect

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
foreign import bpcall "MCMC:" gibbsSampleCategoricalRaw :: Modifiable Int -> Int -> ContextIndex -> IO ()
gibbsSampleCategorical i n = TransitionKernel (gibbsSampleCategoricalRaw i n)                                                           

foreign import bpcall "MCMC:" discreteUniformAvoidMHRaw :: Modifiable Int -> Int -> Int -> ContextIndex -> IO ()
discreteUniformAvoidMH i l u = TransitionKernel (discreteUniformAvoidMHRaw i l u)

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
foreign import bpcall "MCMC:" incDecMHRaw :: Modifiable Int -> BuiltinBounds -> ContextIndex -> IO ()
incDecMH x bnds = TransitionKernel $ incDecMHRaw x (c_range bnds)

foreign import bpcall "MCMC:" sliceSampleRaw :: Modifiable Double -> BuiltinBounds -> ContextIndex -> IO ()
sliceSample x bnds = TransitionKernel $ sliceSampleRaw x (c_range bnds)

foreign import bpcall "MCMC:" sliceSampleIntegerRaw :: Modifiable Int -> BuiltinBounds -> ContextIndex -> IO ()
sliceSampleInteger x bnds = TransitionKernel $ sliceSampleIntegerRaw x (c_range bnds)

foreign import bpcall "MCMC:" walkTreePathRaw :: Modifiable t -> ContextIndex -> EVector Int
walk_tree_path tree c = vector_to_list $ walkTreePathRaw tree c

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" fnprUnsafeProposalRaw :: Modifiable t -> Int -> ContextIndex -> IO LogDouble
fnprUnsafeProposal tree branch = Proposal $ fnprUnsafeProposalRaw tree branch

foreign import bpcall "MCMC:" walkTreeSampleNNIRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNI tree = TransitionKernel $ walkTreeSampleNNIRaw tree

foreign import bpcall "MCMC:" walkTreeSampleNNIandARaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNIandA tree = TransitionKernel $ walkTreeSampleNNIandARaw tree

foreign import bpcall "MCMC:" walkTreeSampleNNIandBranchLengthsRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleNNIandBranchLengths tree = TransitionKernel $ walkTreeSampleNNIandBranchLengthsRaw tree

foreign import bpcall "MCMC:" walkTimeTreeSampleNNIandNodeTimesRaw :: Modifiable t -> ContextIndex -> IO ()
walkTimeTreeSampleNNIandNodeTimes tree = TransitionKernel $ walkTimeTreeSampleNNIandNodeTimesRaw tree

foreign import bpcall "MCMC:" walkTreeSampleBranchLengthsRaw :: Modifiable t -> ContextIndex -> IO ()
walkTreeSampleBranchLengths tree = TransitionKernel $ walkTreeSampleBranchLengthsRaw tree                                                             
foreign import bpcall "MCMC:" sampleSPRAllRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRAll tree = TransitionKernel $ sampleSPRAllRaw tree

foreign import bpcall "MCMC:" sampleSPRNodesRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRNodes tree = TransitionKernel $ sampleSPRNodesRaw tree

foreign import bpcall "MCMC:" sampleSPRFlatRaw :: Modifiable t -> ContextIndex -> IO ()
sampleSPRFlat tree = TransitionKernel $ sampleSPRFlatRaw tree

foreign import bpcall "MCMC:" copyContext :: ContextIndex -> IO ContextIndex

foreign import bpcall "MCMC:" releaseContext :: ContextIndex -> IO ()

foreign import bpcall "MCMC:" switchToContext :: ContextIndex -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" acceptMH :: ContextIndex -> ContextIndex -> LogDouble -> IO Bool

-- TODO: What if copyContext returns a Box<context>?
--       Then if memory is tight, we would destroy the context object, and release the  context.
--       This might take a while though if garbage-collection didn't happen immediately.
--       And we might need to pivot back to c2 later to release it later.
--
--       So... we need some kind of ContextPtr object that holds a reference to the context
--       until the ContextPtr is destroyed?

foreign import bpcall "MCMC:" scaleGroupsSliceRaw :: [Double] -> [Double] -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" scaleGroupsProposalRaw :: [Double] -> [Double] -> ContextIndex -> IO LogDouble

foreign import bpcall "MCMC:" runMCMC :: Int -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" logJSONRaw :: Int -> Int -> IO CJSON
logJSONLine context iter = cjsonToText <$> logJSONRaw context iter
foreign import bpcall "MCMC:" jsonToTableLineRaw :: CJSON -> CPPString
logTableLine context iter = T.fromCppString . jsonToTableLineRaw <$> logJSONRaw context iter

foreign import bpcall "MCMC:" prior :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:" likelihood :: ContextIndex -> IO LogDouble
foreign import bpcall "MCMC:" posterior :: ContextIndex -> IO LogDouble

scaleGroupsSlice xs ys = TransitionKernel $ scaleGroupsSliceRaw (toList xs) (toList ys)

scaleGroupSlice xs = scaleGroupsSlice xs []

scaleGroupsProposal xs ys = Proposal $ scaleGroupsProposalRaw (toList xs) (toList ys)

scaleGroupProposal xs = scaleGroupsProposal xs []

scaleGroupsMH xs ys = metropolisHastings $ scaleGroupsProposal xs ys

metropolisHastings :: Proposal -> TransitionKernel
metropolisHastings (Proposal proposal) = TransitionKernel $ \c1 -> do
  c2 <- copyContext c1
  ratio <- proposal c2
  accept <- acceptMH c1 c2 ratio
  if accept then switchToContext c1 c2 else return ()
  releaseContext c2
  return ()              -- Should we allow `return accept`?

foreign import bpcall "MCMC:" getAtomicModifiableValueInContext :: Modifiable a -> ContextIndex -> IO a
foreign import bpcall "MCMC:" setAtomicModifiableValueInContext :: Modifiable a -> a -> ContextIndex -> IO ()

propose :: (Dist d, IOSampleable d, HasPdf d, Result d ~ a) => Modifiable a -> (a -> d) -> Proposal
propose x dist = Proposal $ \c -> do
  x1 <- getAtomicModifiableValueInContext x c -- This is guaranteed to be an Int, Double, LogDouble, Char, or Object.
                                              -- x has to be an atomic modifiable.  We can't use it to propose trees...
  x2 <- sampleIO (dist x1)
  setAtomicModifiableValueInContext x x2 c
  let rho12 = pdf (dist x1) x2
      rho21 = pdf (dist x2) x1
      hastingsRatio = rho21/rho12
  return hastingsRatio

