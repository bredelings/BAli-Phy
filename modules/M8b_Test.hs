module M8b_Test where
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

note [fConserved,fNeutral,fDiversifying] ~ dirichlet [10.0, 10.0, 1.0];
note omega3 ~ logGamma 4.0 0.25;
note posSelection ~ bernoulli 0.5;
  omega3' = if (posSelection == 1) then omega3 else 1.0;

  d1 n = uniformDiscretize (quantile (beta a b)) n;
  d2 n = (fDiversifying, omega3'):(fNeutral, 1.0):(fmap1 (*fConserved) (unwrapDD (d1 n)));
  dist n = DiscreteDistribution (d2 n);

  main m0 n = multiParameter m0 (dist n);
}
