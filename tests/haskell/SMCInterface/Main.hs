{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment.Matrix (alignment_from_sequences)
import Bio.Alphabet (dna)
import Compiler.Num
import Data.Floating.Types (toFloating)
import Data.Ord ((<))
import Data.Text (pack)
import SMC (smc_density, smc_trace, trace_to_trees)
import System.IO (print)

-- Run the native SMC density path on a small deterministic alignment while
-- retaining compile coverage for the trace-facing interface.
main = do
    let alignment = alignment_from_sequences dna [(pack "a", pack "ACGT"), (pack "b", pack "ACGT")]
        density = toFloating (smc_density 1 [1] [0] 0.01 alignment) :: Double
    print (abs (density - 0.243969070600281) < 1e-14)
