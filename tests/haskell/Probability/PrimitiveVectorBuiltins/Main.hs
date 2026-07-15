{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Fractional
import Compiler.IO (IO)
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (intVectorNativeView)
import Foreign.NativeVector (NativeVector)
import Foreign.Vector (EVector, listToVector, vectorToList)
import Probability (LogDouble)
import Probability.Distribution.Multinomial (multinomial_density)
import System.IO (print)

foreign import trcall "SMC:haplotype01_from_plaf_probability" builtin_haplotype01_from_plaf_probability :: U.Vector Double -> EVector Int -> LogDouble
foreign import trcall "SMC:sample_haplotype01_from_plaf" builtin_sample_haplotype01_from_plaf :: U.Vector Double -> IO (EVector Int)
foreign import bpcall "PopGen:selfing_coalescence_probability" builtin_selfing_coalescence_probability :: Int -> Double -> Int -> Int -> NativeVector Int -> LogDouble

-- Exercise primitive-vector probability boundaries without exposing their
-- internal unboxed representation in the public list-facing interface.
main = do
    print (multinomial_density 2 [0.25,0.75] [1,1])
    print (multinomial_density 2 [0.25,0.75] [2,1])
    print (builtin_haplotype01_from_plaf_probability (U.fromList [0.25,0.75])
                                                     (listToVector [0,1]))
    sampled <- builtin_sample_haplotype01_from_plaf (U.fromList [0,1])
    print (vectorToList sampled)
    print (builtin_selfing_coalescence_probability 2 0 indicatorOffset
                                                    indicatorCount indicatorNative)
  where
    (indicatorOffset, indicatorCount, indicatorNative) = intVectorNativeView
                                                             (U.fromList [0,0])
