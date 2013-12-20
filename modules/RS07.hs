module RS07 where
{
  import Distributions;
  
  builtin rs07_branch_HMM 4 "rs07_branch_HMM";
  builtin rs07_lengthp 2 "rs07_lengthp";
  
note logLambda ~ laplace (-4.0) (1.0/sqrt 2.0);
note meanIndelLengthMinus1 ~ exponential 10.0;
  
  epsilon = meanIndelLengthMinus1/(1.0 + meanIndelLengthMinus1);
  
  lambda = exp logLambda;
  
  main = (\d b heat training -> rs07_branch_HMM epsilon (lambda*d!b) heat training, \l -> rs07_lengthp epsilon l);
}
