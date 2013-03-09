module RelaxedRatesRS07 where
{
  import Distributions;
  import MyTree;
  
  builtin rs07_branch_HMM 4 "rs07_branch_HMM";
  builtin rs07_lengthp 2 "rs07_lengthp";
  
  n = 5;
  n_branches = numBranches Mytree.tree;
  
note mean ~ iid(n, laplace(-4.0,1.0));
note sigmaOverMu ~ iid(n, gamma(1.05,0.1) );
  
  a = map (\x->1.0/(x^2)) sigmaOverMu;
  b = zipWith (/) mean a;
  
  alpha = 1.0;

note q ~ iid (n, beta(1.0,alpha) );

  normalize l = let {total = sum l} in map (/total) l;

  q' = map (1.0-) q;
  left = [ product (take i q') | i <- take n [0..]];
  p' = zipWith (*) q left;
  
  p = normalize p';
  
  lambda_dist = mixture [ (p!!i, gamma (a!!i,b!!i)) | i <- take n [0..]];
  
note logLambdas ~ iid(n_branches, lambda_dist);
note e ~ exponential(10.0);
    
  epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
  lambdas = map exp logLambdas;

  main = (\t b h t -> rs07_branch_HMM epsilon (lambdas!!b * t!b) h t, \l -> rs07_lengthp epsilon l);
}
