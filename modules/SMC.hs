module SMC where
{
  import Distributions;
  builtin smc_density 3 "smc_density" "SMC";
  smc theta rho = ProbDensity (smc_density theta rho) (error "SMC has no quantile") (return 0) (listToString("AlignmentRangeString"));
}  
