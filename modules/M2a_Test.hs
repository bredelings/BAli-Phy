module M2a_Test where
{
  import SModel;
  import Distributions;

note [fConserved, fNeutral, fDiversifying] ~ dirichlet[10.0, 10.0, 1.0];
note omega1 ~ uniform(0.0, 1.0);
note omega3 ~ logGamma(4.0, 0.25);
note pos_selection ~ bernoulli 0.5;

  omega3' = if pos_selection then omega3 else 1.0;
  dist = DiscreteDistribution [(fConserved,omega1),(fNeutral, 1.0),(fDiversifying,omega3')];

  main m0 = multiParameter m0 dist;
}