module SMC where

import Data.Text
import Bio.Alignment.Matrix (AlignmentMatrix)
import Numeric.LinearAlgebra (Vector, fromList)

import Probability
foreign import bpcall "SMC:smc_density" builtin_smc_density :: Double -> Vector Double -> Vector Double -> Double -> AlignmentMatrix -> LogDouble
foreign import bpcall "SMC:smc_trace" builtin_smc_trace :: Double -> Vector Double -> Vector Double -> Double -> AlignmentMatrix -> EVector (EPair Double Int)
foreign import bpcall "SMC:trace_to_trees" builtin_trace_to_trees :: EVector (EPair Double Int) -> CPPString

trace_to_trees = fromCppString . builtin_trace_to_trees

smc_density rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_density rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = fromList rates
                                                                      level_boundaries' = fromList level_boundaries

smc_trace   rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_trace rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = fromList rates
                                                                      level_boundaries' = fromList level_boundaries

data SMC = SMC Double [Double] [Double] Double

instance Dist SMC where
    type Result SMC = AlignmentMatrix
    dist_name _ = "SMC"

instance HasAnnotatedPdf SMC where
    annotated_densities (SMC rhoOverTheta rates boundaries errorRate) =
        make_densities $ smc_density rhoOverTheta rates boundaries errorRate

smc = SMC
