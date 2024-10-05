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

-- TransitionKernel a = TransitionKernel (ContextIndex -> IO a)
type TransitionKernel a = ContextIndex -> IO a

--- The first four arguments allow giving the logger the generation number, prior, likelihood, and probability.
type LoggerAction = Int -> Double -> Double -> Double -> IO ()

data Proposal = Proposal (ContextIndex -> IO LogDouble)

-- It is unfortunate that modifiable-ness is not visible at the type level.
type Modifiable a = a

foreign import bpcall "MCMC:" registerTransitionKernel :: Double -> TransitionKernel a -> IO Effect

foreign import bpcall "MCMC:" registerLogger :: LoggerAction -> IO Effect

-- Transition kernel: Perform gibbs sampling on modifiable x, which takes values [0..n-1], in context c
foreign import bpcall "MCMC:" gibbsSampleCategorical :: Modifiable Int -> Int -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" discreteUniformAvoidMH :: Modifiable Int -> Int -> Int -> ContextIndex -> IO ()

-- It would be nice if we could (i) seq x and bounds HERE, and (ii) convert range to bounds HERE.
-- But the seq needs to be done during changeable execution, and we execute the IO unchangeably.
foreign import bpcall "MCMC:" incDecMHRaw :: Modifiable Int -> BuiltinBounds -> Int -> IO ()
incDecMH x bnds c = incDecMHRaw x (c_range bnds) c

foreign import bpcall "MCMC:" sliceSampleRaw :: Modifiable Double -> BuiltinBounds -> ContextIndex -> IO ()
sliceSample x bnds c = sliceSampleRaw x (c_range bnds) c

foreign import bpcall "MCMC:" sliceSampleIntegerRaw :: Modifiable Int -> BuiltinBounds -> ContextIndex -> IO ()
sliceSampleInteger x bnds c = sliceSampleIntegerRaw x (c_range bnds) c

foreign import bpcall "MCMC:" walkTreePathRaw :: Modifiable t -> ContextIndex -> EVector Int
walk_tree_path tree c = vector_to_list $ walkTreePathRaw tree c

-- This is "unsafe" because it doesn't update alignments
foreign import bpcall "MCMC:" fnprUnsafeProposalRaw :: Modifiable t -> Int -> ContextIndex -> IO LogDouble
fnprUnsafeProposal tree branch = Proposal $ fnprUnsafeProposalRaw tree branch

foreign import bpcall "MCMC:" walkTreeSampleNNI :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walkTreeSampleNNIandA :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walkTreeSampleNNIandBranchLengths :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walkTimeTreeSampleNNIandNodeTimes :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" walkTreeSampleBranchLengths :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sampleSPRAll :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sampleSPRNodes :: Modifiable t -> ContextIndex -> IO ()

foreign import bpcall "MCMC:" sampleSPRFlat :: Modifiable t -> ContextIndex -> IO ()

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

scaleGroupsSlice xs ys = scaleGroupsSliceRaw (toList xs) (toList ys)

scaleGroupSlice xs = scaleGroupsSlice xs []

scaleGroupsProposal xs ys = Proposal $ scaleGroupsProposalRaw (toList xs) (toList ys)

scaleGroupsMH xs ys = metropolisHastings $ scaleGroupsProposal xs ys

metropolisHastings :: Proposal -> ContextIndex -> IO Bool
metropolisHastings (Proposal proposal) c1 = do
  c2 <- copyContext c1
  ratio <- proposal c2
  accept <- acceptMH c1 c2 ratio
  if accept then switchToContext c1 c2 else return ()
  releaseContext c2
  return accept

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

