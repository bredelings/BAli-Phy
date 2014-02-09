module IModel where
{
import Distributions;
import Tree;
  
builtin rs07_branch_HMM 4 "rs07_branch_HMM";
builtin rs07_lengthp 2 "rs07_lengthp";

rs07_model tree = Prefix "RS07"
(do { logLambda <- laplace (-4.0) (1.0/sqrt 2.0);
      Log "logLambda" logLambda;
      let {lambda = exp logLambda};

      meanIndelLengthMinus1 <- exponential 10.0;
      let {epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1)};

      Log "meanIndelLength" (meanIndelLengthMinus1+1.0);
      let {f1 d b heat training = rs07_branch_HMM epsilon (lambda*d!b) heat training};
      return $ (f1, rs07_lengthp epsilon)
});

rs07_relaxed_rates_model tree = Prefix "RelaxedRatesRS07"
(do {
   let {n_branches = numBranches tree;
        delta = 4};

   mean <- iid (n_branches + delta) (laplace (-4.0) (1.0/sqrt 2.0));
   sigma <- iid (n_branches + delta) (gamma 1.05 0.05);
  
   alpha <- gamma 2.0 (1.0/6.0);

   category <- crp alpha n_branches delta;

   z <- iid n_branches (normal 0.0 1.0);

  let {logLambdas = [ mean!!k + z!!i * sigma!!k | i <- take n_branches [0..], let {k=category!!i}]};

  meanIndelLengthMinus1 <- exponential 10.0;
    
  let {epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
      lambdas = map exp logLambdas};

  return $ (\d b heat training -> rs07_branch_HMM epsilon (lambdas!!b * d!b) heat training, \l -> rs07_lengthp epsilon l)
});

}
