module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical

mixture_pdf (p:ps) (dist:dists) x = (doubleToLogDouble p)*(pdf dist x) + (mixture_pdf ps dists x)
mixture_pdf []     []           _ = 0

mixture_density (p:ps) (dist:dists) x = (doubleToLogDouble p)*(density dist x) + (mixture_density ps dists x)
mixture_density []     []           _ = 0

data Mixture d = Mixture [Double] [d]

instance Dist (Mixture d) where
    type Result (Mixture d) = Result d
    dist_name _ = "mixture"

instance IOSampleable d => IOSampleable (Mixture d) where
    sampleIO (Mixture ps dists) = do cat <- sampleIO $ categoricalDist ps
                                     sampleIO $ dists !! cat

instance HasPdf d => HasPdf (Mixture d) where
    pdf (Mixture ps dists) x = mixture_pdf ps dists x

instance HasAnnotatedPdf d => HasAnnotatedPdf (Mixture d) where
    annotated_densities (Mixture ps dists) = make_densities $ mixture_density ps dists

instance Sampleable d => Sampleable (Mixture d) where
    sample (Mixture ps dists) = do cat <- categorical ps
                                   sample $ dists !! cat


mixtureDist ps dists | length ps /= length dists  = error "mixture distribution has different number of weights and distributions"
                     | otherwise                  = Mixture ps dists

mixture ps dists = sample $ mixtureDist ps dists
