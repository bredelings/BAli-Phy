module Probability.Distribution.DirichletProcess
    ( module Probability.Distribution.DirichletProcess.Stick
    , builtin_crp_density
    , crp_density
    , sample_crp_native
    , sample_crp
    , ran_sample_crp
    , triggeredModifiableList
    , crp_effect
    , CRP(..)
    , crp
    ) where

import Probability.Random
import Control.Monad.IO.Class

import Probability.Distribution.List
import Probability.Distribution.DirichletProcess.Stick

import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (intVectorFromNativeWithLength,
                                     intVectorNativeView)
import Foreign.NativeVector (NativeVector)

import MCMC -- for GibbsSampleCategorical

foreign import bpcall "Distribution:CRP_density" builtin_crp_density :: Double -> Int -> Int -> Int -> Int -> NativeVector Int -> LogDouble

-- Marshal assignments into contiguous unboxed storage so the native density
-- can scan primitive integers without copying or unboxing each element.
crp_density alpha n d z =
    case intVectorNativeView (U.fromList z) of
      (offset, count, native) ->
          builtin_crp_density alpha n d offset count native

foreign import bpcall "Distribution:sample_CRP" sample_crp_native :: Double -> Int -> Int -> IO (NativeVector Int)

sample_crp alpha n d = do
    native <- sample_crp_native alpha n d
    return $ U.toList (intVectorFromNativeWithLength n native)
ran_sample_crp alpha n d = liftIO $ sample_crp alpha n d

triggeredModifiableList n value effect = let raw_list = mapn n modifiable value
                                             effect' = unsafePerformIO $ effect raw_list
                                             triggered_list = mapn n (withEffect effect') raw_list
                                         in triggered_list

crp_effect n d x = addMove 1 $ TransitionKernel (\c -> mapM_ (\l-> runTK c $ gibbsSampleCategorical (x!!l) (n+d)) [0..n-1])


data CRP = CRP Double Int Int

instance Dist CRP where
    type Result CRP = [Int]
    distName _ = "crp"

instance IOSampleable CRP where
    sampleIO (CRP alpha n d) = sample_crp alpha n d

instance HasPdf CRP where
    pdf (CRP alpha n d) = crp_density alpha n d

instance HasAnnotatedPdf CRP where
    annotatedDensities dist = make_densities $ pdf dist

instance Sampleable CRP where
    sample dist@(CRP alpha n d) = RanDistribution3 dist (crp_effect n d) (triggeredModifiableList n) (ran_sample_crp alpha n d)

crp = CRP
