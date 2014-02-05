module Gamma where
{
import Distributions;
import SModel;
main base n = Prefix "Gamma" 
  (do {
     sigmaOverMu <- logLaplace (-3.0) 1.0;
     Log "sigmaOverMu" sigmaOverMu;
     let {a = 1.0/b; 
          b = sigmaOverMu^2;
          discretize n = uniformDiscretize (quantile (gamma a b)) n};
     return $ multiRate base (discretize n)
});
}
