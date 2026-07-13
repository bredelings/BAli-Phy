{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Fractional
import Compiler.IO (IO)
import Compiler.Num
import qualified Data.Vector.Unboxed as U
import Data.Vector.Unboxed.Internal (doubleVectorNativeView)
import Foreign.NativeVector (NativeVector)
import Foreign.Vector (EVector, listToVector, vectorToList)
import Probability (LogDouble)
import Probability.Distribution.Multinomial (multinomial_density)
import System.IO (print)

foreign import bpcall "SMC:haplotype01_from_plaf_probability" builtin_haplotype01_from_plaf_probability :: Int -> Int -> NativeVector Double -> EVector Int -> LogDouble
foreign import bpcall "SMC:sample_haplotype01_from_plaf" builtin_sample_haplotype01_from_plaf :: Int -> Int -> NativeVector Double -> IO (EVector Int)

-- Exercise primitive-vector probability boundaries without exposing their
-- internal unboxed representation in the public list-facing interface.
main = do
    print (multinomial_density 2 [0.25,0.75] [1,1])
    print (multinomial_density 2 [0.25,0.75] [2,1])
    print (builtin_haplotype01_from_plaf_probability offset count native
                                                     (listToVector [0,1]))
    sampled <- builtin_sample_haplotype01_from_plaf sampleOffset sampleCount
                                                    sampleNative
    print (vectorToList sampled)
  where
    (offset, count, native) = doubleVectorNativeView
                                  (U.fromList [0.25,0.75])
    (sampleOffset, sampleCount, sampleNative) = doubleVectorNativeView
                                                   (U.fromList [0,1])
