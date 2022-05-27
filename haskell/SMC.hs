module SMC where

import Data.Text

import Probability
builtin builtin_smc_density 5 "SMC:smc_density"
builtin builtin_smc_trace   5 "SMC:smc_trace"
builtin builtin_trace_to_trees      1 "SMC:trace_to_trees"

trace_to_trees = Text . builtin_trace_to_trees

smc_density rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_density rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = list_to_vector rates
                                                                      level_boundaries' = list_to_vector level_boundaries

smc_trace   rho_over_theta rates level_boundaries error_rate sequences = builtin_smc_trace rho_over_theta rates' level_boundaries' error_rate sequences
                                                                where rates' = list_to_vector rates
                                                                      level_boundaries' = list_to_vector level_boundaries

smc rho_over_theta rates level_boundaries error_rate = Distribution "SMC" (make_densities $ smc_density rho_over_theta rates level_boundaries error_rate)
                                                                    (error "SMC has no quantile") (return 0) (list_to_string("AlignmentRangeString"))
