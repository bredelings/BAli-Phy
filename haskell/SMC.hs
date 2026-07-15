module SMC where

import Data.Text
import Bio.Alignment.Matrix (AlignmentMatrix)
import Numeric.LinearAlgebra (Vector, fromList)

import Probability
foreign import trcall "SMC:smc_density" smcDensityNative :: Double -> Vector Double -> Vector Double -> Double -> AlignmentMatrix -> LogDouble
foreign import trcall "SMC:smc_trace" smcTraceNative :: Double -> Vector Double -> Vector Double -> Double -> AlignmentMatrix -> EVector (EPair Double Int)
foreign import bpcall "SMC:trace_to_trees" builtin_trace_to_trees :: EVector (EPair Double Int) -> CPPString

trace_to_trees = fromCppString . builtin_trace_to_trees

smc_density rho_over_theta rates level_boundaries error_rate sequences =
    smcDensityNative rho_over_theta (fromList rates) (fromList level_boundaries)
                     error_rate sequences

smc_trace rho_over_theta rates level_boundaries error_rate sequences =
    smcTraceNative rho_over_theta (fromList rates) (fromList level_boundaries)
                   error_rate sequences

data SMC = SMC Double [Double] [Double] Double

instance Dist SMC where
    type Result SMC = AlignmentMatrix
    dist_name _ = "SMC"

instance HasAnnotatedPdf SMC where
    annotated_densities (SMC rhoOverTheta rates boundaries errorRate) =
        make_densities $ smc_density rhoOverTheta rates boundaries errorRate

smc = SMC
