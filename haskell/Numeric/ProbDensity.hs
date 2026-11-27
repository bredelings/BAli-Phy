{-# LANGUAGE NoImplicitPrelude #-}
module Numeric.ProbDensity
    ( ProbDensity
    , fromLogDensity
    , collapseDensity
    , reciprocalDensity
    ) where

import Compiler.FFI.Import (CInput, COutput)
import Compiler.FFI.Runtime (RuntimeValue)
import Numeric.Log (Log)

-- An opaque product of density factors that retains zero, infinity, and NaN multiplicities.
-- Zero multiplicities use Double. Above 2^53, counts one apart can compare equal,
-- leaving the finite coefficients to determine the ratio of two zero-density states.
data ProbDensity

instance RuntimeValue ProbDensity
instance CInput ProbDensity
instance COutput ProbDensity

foreign import bpcall "Num:" fromLogDensity :: Log Double -> ProbDensity

-- Project a density product to its ordinary log value, discarding defect multiplicities.
foreign import bpcall "Num:" collapseDensity :: ProbDensity -> Log Double

-- Invert a density while retaining signed zero, infinity, and NaN multiplicities.
foreign import bpcall "Num:" reciprocalDensity :: ProbDensity -> ProbDensity
