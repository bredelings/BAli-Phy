module RelaxedRatesRS07 where
{
  import Distributions;
  import Tree;
  import MyTree;
  import MCMC;
  
  builtin rs07_branch_HMM 4 "rs07_branch_HMM";
  builtin rs07_lengthp 2 "rs07_lengthp";
  
  n_branches = numBranches MyTree.tree;
  
  delta = 4;

note mean ~ iid (n_branches + delta) (laplace (-4.0) (1.0/sqrt 2.0));
note sigma ~ iid (n_branches + delta) (gamma 1.05 0.05);
  
note alpha ~ gamma 2.0 (1.0/6.0);

note category ~ crp alpha n_branches delta;

note z ~ iid n_branches (normal 0.0 1.0);

  logLambdas = [ mean!!k + z!!i * sigma!!k | i <- take n_branches [0..], let {k=category!!i}];

note meanIndelLengthMinus1 ~ exponential 10.0;
    
  epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
  lambdas = map exp logLambdas;

  main = (\d b heat training -> rs07_branch_HMM epsilon (lambdas!!b * d!b) heat training, \l -> rs07_lengthp epsilon l);

note MakeLogger logLambdas;
note MakeMove (\pr -> mapM_ (\l-> gibbs_sample_categorical (category!!l) (n_branches+delta) pr) [0..n_branches-1]);
}
