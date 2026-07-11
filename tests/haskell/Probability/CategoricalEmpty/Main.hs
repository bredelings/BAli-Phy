{-# LANGUAGE NoImplicitPrelude #-}

import Compiler.Classes
import Probability.Dist (IOSampleable(sampleIO))
import Probability.Distribution.Categorical (categorical)
import System.IO (print)

-- Sampling an empty categorical distribution should report a controlled
-- runtime error rather than indexing empty scratch storage.
main = do
    value <- sampleIO (categorical [])
    print value
