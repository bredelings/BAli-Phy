module M2a where
{
  import SModel;
  import Distributions;

note [fConserved, fNeutral, fSelection] ~ dirichlet[10.0, 10.0, 1.0];
note omega1 ~ uniform(0.0, 1.0);
note omega3 ~ logGamma(4.0, 0.25);

  dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0),(fSelection,omega3)];

  main m0 = multiParameter m0 dist;
}