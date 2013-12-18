module Gamma where
{
  import Distributions;
  import SModel;
note sigmaOverMu ~ logLaplace(-3.0,1.0);

b = sigmaOverMu^2;
a = 1.0/b;

discretize n = uniformDiscretize (quantile (gamma (a,b))) n;

main base n = multiRate base (discretize n)
}
