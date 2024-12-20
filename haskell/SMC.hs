module SMC where

import Data.Text

import Probability
foreign import bpcall "SMC:smc_density" builtin_smc_density :: () -> () -> () -> () -> () -> ()
foreign import bpcall "SMC:smc_trace" builtin_smc_trace   :: () -> () -> () -> () -> () -> ()
foreign import bpcall "SMC:trace_to_trees" builtin_trace_to_trees      :: () -> ()

trace_to_trees = Text . builtin_trace_to_trees

smc_density rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_density rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = toVector rates
                                                                      level_boundaries' = toVector level_boundaries

smc_trace   rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_trace rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = toVector rates
                                                                      level_boundaries' = toVector level_boundaries

smc rho_over_theta rates level_boundaries error_rate = Distribution "SMC" (make_densities $ smc_density rho_over_theta rates level_boundaries error_rate)
                                                                    (error "SMC has no quantile") (return 0) (list_to_string("AlignmentRangeString"))
