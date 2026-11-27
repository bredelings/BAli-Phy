{-# LANGUAGE NoImplicitPrelude #-}

import Bio.Alignment.Matrix (alignment_from_sequences)
import Bio.Alphabet (dna)
import Compiler.Enum (maxBound)
import Compiler.Num
import Compiler.Fractional
import Data.Bool ((&&))
import Data.Eq ((==))
import Foreign.Vector (toVector)
import Numeric.Pow (Pow(ln))
import PopGen.Deploid (sample_reads01, probability_of_reads01)
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
    -- Depth and both alleles must survive the native boundary, including when the
    -- reported pair exceeds maxBound in total. Small alignments cannot check either case.
    let depth = maxBound
        haplotypes = [toVector [1]]
        score n e c pair = ln (probability_of_reads01 [n] [1] haplotypes e c 0 [pair])
    [(ref,alt)] <- sample_reads01 [depth] [1] haplotypes 0.25 4 0
    print (depth-ref == alt && abs (score depth 0.25 4 (depth-1,1) + 128.1144453679335) < 1e-10
                            && score depth 0.25 4 (depth,1) == -1/0)
    -- Comparable concentration and depth lose the central log mass in a difference of log gammas.
    let n = 1000000000000000000
        half = 500000000000000000
    print (abs (score n 0.5 2e18 (half,half) + 21.15178974364522) < 1e-10)
