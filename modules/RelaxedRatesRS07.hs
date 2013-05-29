module RelaxedRatesRS07 where
{
  import Distributions;
  import Tree;
  import MyTree;
  
  builtin rs07_branch_HMM 4 "rs07_branch_HMM";
  builtin rs07_lengthp 2 "rs07_lengthp";
  
  n = 5;
  n_branches = numBranches MyTree.tree;
  
note mean ~ iid(n, laplace(-4.0,1.0));
note sigma ~ iid(n, gamma(1.05,0.05) );
  
  alpha = 0.1;

note q ~ iid (n, beta(1.0,alpha) );

  normalize l = let {total = sum l} in map (/total) l;

  q' = map (1.0-) q;
  left = [ product (take i q') | i <- take n [0..]];
  p' = zipWith (*) q left;
  
  p = normalize p';
  
  lambda_dist = mixture [ (p!!i, normal (mean!!i,sigma!!i)) | i <- take n [0..]];
  
note log_lambda_sample ~ lambda_dist;
  
note category ~ iid(n_branches, categorical p);

note z ~ iid(n_branches, normal(0.0, 1.0));

  logLambdas = [ mean!!k + z!!i * sigma!!k | i <- take n_branches [0..], let {k=category!!i}];

note meanIndelLengthMinus1 ~ exponential(10.0);
    
  epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
  lambdas = map exp logLambdas;

  main = (\d b heat training -> rs07_branch_HMM epsilon (lambdas!!b * d!b) heat training, \l -> rs07_lengthp epsilon l);
note MakeLogger logLambdas;
}
