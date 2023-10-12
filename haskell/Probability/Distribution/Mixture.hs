module Probability.Distribution.Mixture where

import Probability.Random
import Probability.Distribution.Categorical
import Probability.Distribution.Discrete

{-
   PROBLEM:  We CANNOT write e.g. equalMixture [ always 0, always 1, uniform 0 1 ]
              because currently Mixture requires all the component distributions
              to be the same type.

             We CAN write equalMixture [ sample $ always 0, sample $ uniform 0 1 ]
              because then all the entries are of type Random Double

             The problem is that usually we'd like the component distributions
              to all support some property -- such as Sampleable, or Dist1D or something.
             But then we'd need some kind of wrapper that packages the distributions and
              a dictionary for those pieces of functionality.  We can't derive the required
              functionality from the calling context.

             Ideally, therefore, it would be best if equalMixture could support a
              heterogeneous collection that depended on the types of the arguments.
             Then equalMixture (always 0, uniform 0 1, always 1) would have type
              Mixture (Discrete Double) Uniform (Discrete Double).
             We could derive properties such as Sampleable or Dist1D (for cdf) at the call site.
             But this wouldn't support mixtures of unknown length.
             In order to handle collections of generic length, we need some kind of
              metaprogramming.
-}

{-
   Generalization: Mixture (IO a)?

   Mixture allows us to
     * sample from the collection
     * perform the chosen entry.
   This sounds like it requires one monad for the choosing, and a second monad for the performing.

   Mixture allows us to choose a random entry, and then do something with it.
   If the chosen entry is a distribution, we would sample from it as well.
   But how about when x is something else?  Like an action?  Then shouldn't Mixture d be an action that can be taken?
   For example, suppose we have a Mixture (IO a).  Then shouldn't this ALSO be an IO a?

   Perhaps Mixture should then be a functor. Perhaps Mixture d a should return IO (d a) or Random (d a)
   Should we also attempt to join then?  Random (Random a) could reduce to Random a.
   But how about IO (Random a)
   Random (IO a) should be performable in IO as join (sample IO)
-}

data Mixture d = Mixture d

instance (Dist d, Dist (Result d)) => Dist (Mixture d) where
    type Result (Mixture d) = Result (Result d)
    dist_name _ = "mixture"

instance (IOSampleable d, IOSampleable (Result d)) => IOSampleable (Mixture d) where
    sampleIO (Mixture dist1) = do dist2 <- sampleIO dist1
                                  sampleIO dist2

instance (Sampleable d, Sampleable (Result d)) => Sampleable (Mixture d) where
    sample (Mixture dist1) = do dist2 <- sample dist1
                                sample dist2

instance HasPdf d => HasPdf (Mixture (Discrete d)) where
    pdf (Mixture (Discrete pairs)) x = sum [ doubleToLogDouble p * pdf dist x | (dist,p) <- pairs ]

instance HasAnnotatedPdf d => HasAnnotatedPdf (Mixture (Discrete d)) where
    annotated_densities (Mixture (Discrete pairs)) x = return $ ([sum [doubleToLogDouble p * density dist x | (dist,p) <- pairs]], ())


mixture ps dists | length ps /= length dists  = error "mixture distribution has different number of weights and distributions"
                 | otherwise                  = Mixture $ Discrete $ zip dists ps 

equalMixture dists = mixture ps dists where
    n = length dists
    ps = replicate n (1/fromIntegral n)

