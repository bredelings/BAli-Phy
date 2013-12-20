module M1a where
{
  import SModel;
  import Distributions;

note [fConserved, fNeutral] ~ dirichlet[10.0, 11.0];
note omega1 ~ uniform 0.0 1.0;

  dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0)];

  main m0 = multiParameter m0 dist;
}
