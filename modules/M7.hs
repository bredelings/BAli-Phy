module M7 where
{
  import Distributions;
  import SModel;
note mu ~ uniform 0.0 1.0;
-- sigma^2/mu
note gamma ~ beta 1.0 10.0;
  cap = min (mu/(1.0+mu)) ((1.0-mu)/(2.0-mu));
  gamma' = gamma*cap;
  n = (1.0/gamma')-1.0;
  a = n*mu;
  b = n*(1.0 - mu);

  dist n = uniformDiscretize (quantile (beta a b)) n;

  main m0 n = multiParameter m0 (dist n);
}
