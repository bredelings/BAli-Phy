module BranchSiteTest where
{
  import Distributions;
  import SModel;

note posW ~ logGamma 4.0 0.25;
note posSelection ~ bernoulli 0.5;
  posW' = if (posSelection == 1) then posW else 1.0;

note [f0,f1] ~ dirichlet' 2 1.0;
note w0 ~ uniform 0.0 1.0;

  d1 = DiscreteDistribution [(f0,w0),(f1,1.0)];
  d2 = DiscreteDistribution [(f0,posW'),(f1,posW')];

note posP ~ beta 1.0 10.0;

main m0 = MixtureModels [mixture1,mixture2] where {
           mixture1 = multiParameter m0 (mixDiscreteDistributions [posP,1.0-posP] [d1,d1]);
           mixture2 = multiParameter m0 (mixDiscreteDistributions [posP,1.0-posP] [d1,d2])};
}
