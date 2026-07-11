{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Compiler.Fractional ((/))
import Compiler.Num
import Control.Monad (replicateM)
import Data.Bool
import Data.Foldable (all)
import Data.List (replicate)
import Data.Ord
import Probability.Dist (IOSampleable(sampleIO))
import Probability.Distribution.Categorical (categorical)
import System.IO (print)

-- Sample an environment-backed probability vector and verify every native
-- dependent read produces an in-range categorical index.
main = do
    let probability = 1 / 11
        distribution = categorical (replicate 11 probability)
    samples <- replicateM 32 (sampleIO distribution)
    print (all (\index -> 0 <= index && index < 11) samples)
